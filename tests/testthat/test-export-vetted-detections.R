make_vetted_export_fixture <- function(pass_only = FALSE) {
  detections <- tibble::tibble(
    source_database = rep("db_a", 5),
    motusTagID = rep(101L, 5),
    mfgID = rep("M101", 5),
    speciesEN = rep("Example bird", 5),
    tagDeployID = rep(1001L, 5),
    runID = c(1L, 1L, 2L, 2L, 3L),
    hitID = c(1L, 2L, 1L, 2L, 1L),
    batchID = rep(50L, 5),
    deviceID = rep(500L, 5),
    ts = as.POSIXct(
      c(
        "2025-05-01 00:00:00",
        "2025-05-01 00:00:05",
        "2025-05-02 00:00:00",
        "2025-05-02 00:00:05",
        "2025-05-03 00:00:00"
      ),
      tz = "UTC"
    ),
    recvDeployID = rep(9001L, 5),
    recvDeployName = rep("Example Station", 5),
    recvSiteName = rep("Legacy Station", 5),
    recv = rep("SG-001", 5),
    recvDeployLat = rep(45, 5),
    recvDeployLon = rep(-75, 5),
    freq = rep(166.38, 5),
    sig = c(-55, -54, -60, -59, -80),
    sigsd = rep(1.5, 5),
    noise = rep(-100, 5),
    qa_status = c("PASS", "PASS", "REVIEW", "REVIEW", "REVIEW"),
    recv_deploy_resolution = c(
      "not_needed", "not_needed", "temporal", "temporal", "not_needed"
    ),
    port = rep(1L, 5),
    antType = rep("yagi", 5),
    antBearing = rep(90, 5),
    receiver_type = rep("SensorGnome", 5),
    runLen = c(2L, 2L, 3L, 3L, 2L),
    motusFilter = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    review_reasons = c(
      NA, NA, "Motus filter", "Motus filter", "Ambiguous tag identity"
    )
  )

  if (pass_only) {
    detections <- dplyr::filter(detections, qa_status == "PASS")
  }

  excluded <- detections[1, , drop = FALSE] |>
    dplyr::mutate(
      runID = 4L,
      hitID = 1L,
      qa_status = "EXCLUDE",
      exclude_reason = "Missing receiver coordinates"
    )

  clean <- list(
    detections = detections,
    excluded = excluded,
    summary = list(source = "fixture"),
    settings = list(
      sg_min_run_length = 3,
      lotek_pass_min = 4
    )
  )
  class(clean) <- c("motus_clean", "list")

  if (pass_only) {
    return(apply_validation(clean, NULL))
  }

  review <- tibble::tibble(
    review_id = c("R0001", "R0002"),
    source_database = c("db_a", "db_a"),
    runID = c(2L, 3L),
    decision = c("KEEP", "REJECT"),
    notes = c("retain after review", "false detection")
  )

  apply_validation(clean, review)
}


test_that("PASS-only validation exports explicit automated dispositions", {
  validated <- make_vetted_export_fixture(pass_only = TRUE)
  artifact <- export_vetted_detections(validated)

  expect_s3_class(artifact, "motus_vetted_export")
  expect_identical(artifact$schema_version, "1.0.0")
  expect_identical(artifact$artifact_status, "complete")
  expect_equal(nrow(artifact$detections), 2L)
  expect_true(all(
    artifact$detections$automated_qa_status == "PASS"
  ))
  expect_true(all(
    artifact$detections$final_disposition == "automated_pass"
  ))
  expect_true(all(is.na(
    artifact$detections$validation_review_id
  )))
  expect_identical(
    artifact$build_metadata$counts$unresolved_review_runs,
    0L
  )
})


test_that("export distinguishes kept and rejected REVIEW runs", {
  validated <- make_vetted_export_fixture()
  artifact <- export_vetted_detections(validated)
  exported <- artifact$detections

  expect_named(
    artifact,
    c(
      "schema_version",
      "artifact_status",
      "build_metadata",
      "detections",
      "receiver_deployments",
      "qa_summary"
    )
  )

  required_fields <- c(
    "motus_detection_id",
    "tag_id",
    "time_utc",
    "source_database",
    "run_id",
    "hit_id",
    "receiver_deployment_id",
    "receiver_name",
    "lat",
    "lon",
    "observed_frequency",
    "signal",
    "signal_sd",
    "noise",
    "automated_qa_status",
    "final_disposition",
    "validation_review_id",
    "validation_notes",
    "recv_deploy_resolution"
  )
  expect_true(all(required_fields %in% names(exported)))

  expect_equal(nrow(exported), 4L)
  expect_equal(
    unique(exported$final_disposition[exported$run_id == "1"]),
    "automated_pass"
  )
  expect_equal(
    unique(exported$final_disposition[exported$run_id == "2"]),
    "review_keep"
  )
  expect_equal(
    unique(exported$validation_review_id[exported$run_id == "2"]),
    "R0001"
  )
  expect_equal(
    unique(exported$validation_notes[exported$run_id == "2"]),
    "retain after review"
  )

  expect_false("3" %in% exported$run_id)
  expect_false("4" %in% exported$run_id)
  expect_equal(anyDuplicated(exported$motus_detection_id), 0L)
  expect_equal(
    exported$motus_detection_id,
    export_vetted_detections(validated)$detections$motus_detection_id
  )

  expect_type(exported$motus_detection_id, "character")
  expect_type(exported$tag_id, "character")
  expect_type(exported$run_id, "character")
  expect_type(exported$hit_id, "character")
  expect_type(exported$receiver_deployment_id, "character")
  expect_s3_class(exported$time_utc, "POSIXct")
  expect_identical(attr(exported$time_utc, "tzone"), "UTC")

  expect_equal(nrow(artifact$receiver_deployments), 1L)
  expect_equal(
    artifact$receiver_deployments$receiver_name,
    "Example Station"
  )
  expect_equal(
    artifact$receiver_deployments$receiver_deployment_id,
    "9001"
  )
})


test_that("export refuses unresolved or inconsistent REVIEW state", {
  unresolved <- make_vetted_export_fixture()
  unresolved$decisions <- unresolved$decisions |>
    dplyr::filter(runID != "2")

  expect_error(
    export_vetted_detections(unresolved),
    "unresolved REVIEW"
  )

  inconsistent <- make_vetted_export_fixture()
  inconsistent$decisions$decision[
    inconsistent$decisions$runID == "2"
  ] <- "REJECT"

  expect_error(
    export_vetted_detections(inconsistent),
    "Retained REVIEW runs must have decision KEEP"
  )
})


test_that("artifact counts reconcile with validated source tables", {
  validated <- make_vetted_export_fixture()
  artifact <- export_vetted_detections(validated)
  counts <- artifact$build_metadata$counts

  expect_identical(counts$retained_automated_pass_runs, 1L)
  expect_identical(counts$retained_automated_pass_hits, 2L)
  expect_identical(counts$retained_review_keep_runs, 1L)
  expect_identical(counts$retained_review_keep_hits, 2L)
  expect_identical(counts$rejected_review_runs, 1L)
  expect_identical(counts$rejected_review_hits, 1L)
  expect_identical(counts$structural_excluded_runs, 1L)
  expect_identical(counts$structural_excluded_hits, 1L)
  expect_identical(counts$unresolved_review_runs, 0L)
  expect_identical(counts$total_exported_detections, 4L)

  expect_equal(
    counts$total_exported_detections,
    nrow(validated$detections)
  )
  expect_equal(
    counts$rejected_review_hits,
    nrow(validated$rejected)
  )
  expect_equal(
    counts$structural_excluded_hits,
    nrow(validated$excluded)
  )

  exclusion_reason <- artifact$qa_summary |>
    dplyr::filter(
      category == "structural_excluded_reason",
      reason == "Missing receiver coordinates"
    )
  expect_equal(exclusion_reason$n_runs, 1L)
  expect_equal(exclusion_reason$n_hits, 1L)
  expect_identical(
    artifact$build_metadata$qa_settings,
    validated$settings
  )
})


test_that("optional RDS writing is guarded and readable", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)

  artifact <- export_vetted_detections(
    make_vetted_export_fixture(),
    file = path
  )

  expect_true(file.exists(path))
  expect_identical(readRDS(path), artifact)
  expect_error(
    export_vetted_detections(
      make_vetted_export_fixture(),
      file = path
    ),
    "already exists"
  )
})
