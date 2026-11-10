test_that("duplicated physical hits resolve to the unique temporally valid deployment", {

  det <- tibble::tibble(
    source_database = c("source_1", "source_1"),
    runID = c(1L, 1L),
    hitID = c(10L, 10L),
    tsCorrected = c(50, 50),
    recvDeployID = c(100L, 200L)
  )

  recv_deps <- tibble::tibble(
    source_database = c("source_1", "source_1"),
    deployID = c(100L, 200L),
    tsStart = c(0, 100),
    tsEnd = c(99, 200)
  )

  expect_message(
    out <- resolve_receiver_deployments(det, recv_deps),
    "Resolved 1 duplicated physical hit"
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$recvDeployID, 100L)
  expect_equal(out$recv_deploy_resolution, "temporal")
})


test_that("ambiguous receiver-deployment resolution is refused", {

  det <- tibble::tibble(
    source_database = c("source_1", "source_1"),
    runID = c(1L, 1L),
    hitID = c(10L, 10L),
    tsCorrected = c(50, 50),
    recvDeployID = c(100L, 200L)
  )

  no_valid_deps <- tibble::tibble(
    source_database = c("source_1", "source_1"),
    deployID = c(100L, 200L),
    tsStart = c(100, 200),
    tsEnd = c(150, 250)
  )

  expect_error(
    resolve_receiver_deployments(det, no_valid_deps),
    "temporally valid=0",
    fixed = TRUE
  )

  multiple_valid_deps <- tibble::tibble(
    source_database = c("source_1", "source_1"),
    deployID = c(100L, 200L),
    tsStart = c(0, 25),
    tsEnd = c(100, 75)
  )

  expect_error(
    resolve_receiver_deployments(det, multiple_valid_deps),
    "temporally valid=2",
    fixed = TRUE
  )
})


test_that("prepare_motus_detections passes the extracted tables to the summarizer", {

  alltags <- tibble::tibble(
    source_database = "source_1",
    runID = 1L,
    tsCorrected = 100,
    tagDeployStart = 0,
    tagDeployEnd = 200
  )

  tables <- list(
    alltags = alltags,
    runs = tibble::tibble(),
    activity = tibble::tibble(),
    recv_deps = tibble::tibble()
  )

  exclusion_flags <- c(
    "exclude_missing_tag_id",
    "exclude_missing_run_id",
    "exclude_missing_run_length",
    "exclude_invalid_timestamp",
    "exclude_missing_tag_deploy_start",
    "exclude_pre_tag_deployment",
    "exclude_missing_recv_deploy",
    "exclude_missing_recv_coords",
    "exclude_missing_lotek_bi",
    "exclude_ctt_short_run",
    "exclude_sgss_short_run",
    "exclude_sgss_insufficient_consecutive",
    "exclude_lotek_receiver_short_run"
  )

  review_flags <- c(
    "flag_motus_filter",
    "flag_ambiguous",
    "flag_unknown_tag_technology",
    "flag_unknown_receiver_type",
    "flag_high_runfreqsd",
    "flag_high_hit_freqsd",
    "flag_high_pulse_activity",
    "flag_short_lotek_receiver_run"
  )

  testthat::local_mocked_bindings(
    extract_motus_tables = function(connections, source_names) tables,
    resolve_receiver_deployments = function(det, recv_deps) det,
    join_detection_context = function(alltags, runs, activity) alltags,
    classify_detection_technology = function(det) det,
    calculate_lotek_run_metrics = function(det) det,
    flag_structural_exclusions = function(det, ...) {
      for (flag in exclusion_flags) {
        det[[flag]] <- FALSE
      }
      det$exclude <- FALSE
      det
    },
    flag_review_runs = function(det, ...) {
      for (flag in review_flags) {
        det[[flag]] <- FALSE
      }
      det$qa_status <- "PASS"
      det$review_reasons <- NA_character_
      det
    },
    .package = "motusclean"
  )

  fake_connection <- structure(
    list(),
    class = "DBIConnection"
  )

  out <- prepare_motus_detections(fake_connection)

  expect_s3_class(out, "motus_clean")
  expect_equal(out$summary$overall$raw_hits, 1L)
  expect_equal(out$summary$overall$raw_runs, 1L)
  expect_equal(out$summary$overall$eligible_hits, 1L)
})


test_that("Lotek burst slop is joined with the full physical-hit key", {

  det <- tibble::tibble(
    source_database = rep("source_1", 4),
    runID = c(1L, 1L, 2L, 2L),
    hitID = c(1L, 2L, 1L, 2L),
    tsCorrected = c(0, 10, 100, 110),
    tagBI = rep(10, 4),
    freqsd = rep(0.01, 4),
    burstSlop = rep(0.001, 4),
    tag_technology = rep("Lotek", 4)
  )

  expect_no_error(
    out <- calculate_lotek_run_metrics(det)
  )

  expect_equal(nrow(out), nrow(det))
  expect_equal(
    dplyr::n_distinct(out$source_database, out$runID, out$hitID),
    nrow(det)
  )
  expect_true(all(!is.na(out$n_bad_bursts)))
})


test_that("burst-slop diagnostics do not create review reasons", {

  det <- tibble::tibble(
    source_database = "source_1",
    runID = 1L,
    motusFilter = 1,
    ambigID = NA_integer_,
    tag_technology = "Lotek",
    receiver_type = "SensorGnome",
    runfreqsd = 0.01,
    pulseCount = 100,
    burstSlop = 0.01,
    maxburstSlop = 0.001,
    freqsd = 0.01,
    longRun = 5L,
    runLen = 5L,
    numRuns = NA_real_,
    run2 = NA_real_
  )

  out <- flag_review_runs(det)

  expect_true(out$flag_excess_burst_slop)
  expect_false(out$needs_review)
  expect_equal(out$qa_status, "PASS")
  expect_true(is.na(out$review_reasons))
})
test_that("short Motus-filtered runs receive explicit human-facing reasons", {

  det <- tibble::tibble(
    source_database = rep("source_1", 3),
    runID = 1:3,
    motusFilter = rep(0, 3),
    ambigID = c(NA_integer_, NA_integer_, 99L),
    tag_technology = rep("CTT", 3),
    receiver_type = rep("SensorStation", 3),
    runfreqsd = rep(NA_real_, 3),
    pulseCount = rep(NA_real_, 3),
    burstSlop = rep(NA_real_, 3),
    maxburstSlop = rep(NA_real_, 3),
    freqsd = rep(NA_real_, 3),
    longRun = rep(NA_integer_, 3),
    runLen = 2:4,
    numRuns = rep(NA_real_, 3),
    run2 = rep(NA_real_, 3)
  )

  out <- flag_review_runs(det)

  expect_true(all(out$flag_motus_filter))
  expect_true(all(out$needs_review))
  expect_true(all(out$qa_status == "REVIEW"))
  expect_equal(
    out$review_reasons,
    c(
      "Short run (2 hits)",
      "Short run (3 hits)",
      "Motus filter; Ambiguous tag identity"
    )
  )
})
