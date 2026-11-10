make_validation_fixture <- function() {
  detections <- tibble::tibble(
    source_database = c("db_a", "db_a", "db_a", "db_a", "db_b"),
    runID = c(1L, 1L, 2L, 2L, 2L),
    hitID = 1:5,
    qa_status = c("PASS", "PASS", "REVIEW", "REVIEW", "REVIEW"),
    qa_marker = c("pass", "pass", "keep", "keep", "reject")
  )
  excluded <- tibble::tibble(
    source_database = "db_a",
    runID = 3L,
    hitID = 6L,
    qa_status = "EXCLUDE"
  )
  out <- list(
    detections = detections,
    excluded = excluded,
    summary = list(preparation_sentinel = 42L),
    settings = list(setting_sentinel = "unchanged")
  )
  class(out) <- c("motus_clean", "list")
  out
}

make_completed_review <- function() {
  tibble::tibble(
    review_id = c("duplicate-human-label", "duplicate-human-label"),
    source_database = c("db_a", "db_b"),
    runID = c(2L, 2L),
    decision = c(" keep ", "reject"),
    notes = c("", "not credible"),
    panel = c(1L, 2L)
  )
}

test_that("apply_validation partitions hits using the composite run key", {
  x <- make_validation_fixture()
  review <- make_completed_review()

  result <- apply_validation(x, review)

  expect_s3_class(result, "motus_validated")
  expect_equal(result$decisions$decision, c("KEEP", "REJECT"))
  expect_equal(result$decisions$notes, c("", "not credible"))
  expect_equal(nrow(result$detections), 4L)
  expect_equal(
    dplyr::distinct(result$detections, source_database, runID),
    tibble::tibble(source_database = c("db_a", "db_a"), runID = c(1L, 2L))
  )
  expect_equal(nrow(result$rejected), 1L)
  expect_equal(result$rejected$source_database, "db_b")
  expect_equal(result$rejected$runID, 2L)
  expect_equal(result$rejected$validation_decision, "REJECT")
  expect_equal(result$rejected$validation_notes, "not credible")
  expect_equal(result$rejected$validation_review_id, "duplicate-human-label")

  expect_equal(result$detections$qa_status, x$detections$qa_status[1:4])
  expect_equal(result$detections$qa_marker, x$detections$qa_marker[1:4])
  expect_equal(result$rejected$qa_status, "REVIEW")
  expect_identical(result$excluded, x$excluded)
  expect_identical(result$settings, x$settings)
  expect_identical(result$summary$preparation, x$summary)

  expect_equal(
    result$summary$validation,
    tibble::tibble(
      automated_pass_runs = 1L,
      automated_pass_hits = 2L,
      review_runs = 2L,
      review_hits = 3L,
      human_keep_runs = 1L,
      human_keep_hits = 2L,
      human_reject_runs = 1L,
      human_reject_hits = 1L,
      vetted_runs = 2L,
      vetted_hits = 4L,
      structural_excluded_runs = 1L,
      structural_excluded_hits = 1L
    )
  )
})

test_that("apply_validation accepts a completed review CSV", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(make_completed_review(), path, row.names = FALSE, na = "NA")

  result <- apply_validation(make_validation_fixture(), path)

  expect_equal(result$decisions$decision, c("KEEP", "REJECT"))
  expect_equal(result$decisions$notes, c("", "not credible"))
  expect_equal(nrow(result$detections), 4L)
  expect_equal(nrow(result$rejected), 1L)
})

test_that("apply_validation requires complete, valid, unique REVIEW decisions", {
  x <- make_validation_fixture()
  review <- make_completed_review()

  expect_error(apply_validation(x, NULL), "cannot be `NULL`")
  expect_error(apply_validation(x, review[1, ]), "missing decision")
  expect_error(
    apply_validation(x, dplyr::bind_rows(review, review[1, ])),
    "duplicate decisions"
  )

  invalid <- review
  invalid$decision[1] <- "maybe"
  expect_error(apply_validation(x, invalid), "KEEP or REJECT")

  blank <- review
  blank$decision[1] <- "  "
  expect_error(apply_validation(x, blank), "KEEP or REJECT")

  unknown <- review
  unknown$runID[1] <- 999L
  expect_error(apply_validation(x, unknown), "unknown or non-REVIEW")

  pass_run <- review
  pass_run$runID[1] <- 1L
  expect_error(apply_validation(x, pass_run), "unknown or non-REVIEW")

  expect_error(
    apply_validation(x, dplyr::select(review, -notes)),
    "missing required column"
  )
})

test_that("apply_validation permits NULL review only with no REVIEW runs", {
  x <- make_validation_fixture()
  x$detections <- dplyr::filter(x$detections, qa_status == "PASS")

  result <- apply_validation(x, NULL)

  expect_s3_class(result, "motus_validated")
  expect_identical(result$detections, x$detections)
  expect_equal(nrow(result$rejected), 0L)
  expect_equal(nrow(result$decisions), 0L)
  expect_equal(result$summary$validation$review_runs, 0L)
  expect_equal(result$summary$validation$review_hits, 0L)

  expect_error(
    apply_validation(x, make_completed_review()[1, ]),
    "unknown or non-REVIEW"
  )
})

test_that("apply_validation rejects inconsistent run-level QA status", {
  x <- make_validation_fixture()
  x$detections$qa_status[2] <- "REVIEW"

  expect_error(
    apply_validation(x, make_completed_review()),
    "more than one `qa_status`"
  )
})