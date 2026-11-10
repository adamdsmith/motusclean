make_structural_detection <- function(
    runLen,
    longRun,
    receiver_type = "SensorGnome"
) {

  n <- length(runLen)

  tibble::tibble(
    source_database = rep("source_1", n),
    runID = seq_len(n),
    motusTagID = seq_len(n),
    tsCorrected = rep(100, n),
    tagDeployStart = rep(0, n),
    recvDeployID = seq_len(n),
    recvDeployLat = rep(45, n),
    recvDeployLon = rep(-75, n),
    tagBI = rep(10, n),
    tag_technology = rep("Lotek", n),
    receiver_type = rep(receiver_type, length.out = n),
    runLen = runLen,
    longRun = longRun
  )
}


make_review_detection <- function(
    runLen,
    longRun,
    motusFilter = 1,
    numRuns = NA_real_,
    run2 = NA_real_,
    receiver_type = "SensorGnome",
    ambigID = NA_integer_,
    runfreqsd = 0.01,
    freqsd = 0.01,
    pulseCount = 100,
    tag_technology = "Lotek"
) {

  tibble::tibble(
    source_database = "source_1",
    runID = 1L,
    motusFilter = motusFilter,
    ambigID = ambigID,
    tag_technology = tag_technology,
    receiver_type = receiver_type,
    runfreqsd = runfreqsd,
    pulseCount = pulseCount,
    burstSlop = NA_real_,
    maxburstSlop = NA_real_,
    freqsd = freqsd,
    longRun = longRun,
    runLen = runLen,
    numRuns = numRuns,
    run2 = run2
  )
}


test_that("SG/SS consecutive-hit exclusions stop at run length five", {

  det <- make_structural_detection(
    runLen = 2:5,
    longRun = rep(1L, 4)
  )

  out <- flag_structural_exclusions(det)

  expect_equal(
    out$exclude_sgss_short_run,
    c(TRUE, FALSE, FALSE, FALSE)
  )
  expect_equal(
    out$exclude_sgss_insufficient_consecutive,
    c(FALSE, TRUE, TRUE, FALSE)
  )
  expect_equal(out$exclude, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(
    out$exclude_reason[2:3],
    rep("Insufficient consecutive Lotek hits", 2)
  )
})


test_that("SG/SS long runs without consecutive hits require review", {

  no_consecutive <- flag_review_runs(
    make_review_detection(
      runLen = 5L,
      longRun = 1L
    )
  )

  some_consecutive <- flag_review_runs(
    make_review_detection(
      runLen = 5L,
      longRun = 2L
    )
  )

  expect_true(no_consecutive$flag_sgss_long_run_no_consecutive)
  expect_true(no_consecutive$needs_review)
  expect_equal(
    no_consecutive$review_reasons,
    "Long run with no consecutive hits"
  )
  expect_false(some_consecutive$flag_sgss_long_run_no_consecutive)
  expect_false(some_consecutive$needs_review)
})


test_that("strong consecutive evidence bypasses explained short-run filtering", {

  ordinary <- flag_review_runs(
    make_review_detection(
      runLen = 3L,
      longRun = 2L,
      motusFilter = 0
    )
  )

  strong <- flag_review_runs(
    make_review_detection(
      runLen = 3L,
      longRun = 3L,
      motusFilter = 0
    )
  )

  expect_true(ordinary$flag_motus_filter)
  expect_true(ordinary$flag_motus_filter_review)
  expect_true(ordinary$needs_review)
  expect_equal(ordinary$review_reasons, "Short run (3 hits)")

  expect_true(strong$flag_motus_filter)
  expect_false(strong$flag_motus_filter_review)
  expect_false(strong$needs_review)
  expect_true(is.na(strong$review_reasons))
})


test_that("noisy-hour filtering is labeled and bypassed only with strong evidence", {

  ordinary <- flag_review_runs(
    make_review_detection(
      runLen = 4L,
      longRun = 2L,
      motusFilter = 0,
      numRuns = 100,
      run2 = 85
    )
  )

  strong <- flag_review_runs(
    make_review_detection(
      runLen = 4L,
      longRun = 3L,
      motusFilter = 0,
      numRuns = 100,
      run2 = 85
    )
  )

  expect_true(ordinary$flag_sgss_noisy_hour_motus_filter)
  expect_true(ordinary$flag_motus_filter_review)
  expect_true(ordinary$needs_review)
  expect_equal(ordinary$review_reasons, "Noisy receiver activity")

  expect_true(strong$flag_motus_filter)
  expect_true(strong$flag_sgss_noisy_hour_motus_filter)
  expect_false(strong$flag_motus_filter_review)
  expect_false(strong$needs_review)
})


test_that("noisy-hour thresholds are inclusive and unexplained filters remain", {

  threshold <- flag_review_runs(
    make_review_detection(
      runLen = 4L,
      longRun = 2L,
      motusFilter = 0,
      numRuns = 100,
      run2 = 85
    )
  )

  too_few_runs <- flag_review_runs(
    make_review_detection(
      runLen = 4L,
      longRun = 3L,
      motusFilter = 0,
      numRuns = 99,
      run2 = 85
    )
  )

  too_few_twos <- flag_review_runs(
    make_review_detection(
      runLen = 4L,
      longRun = 3L,
      motusFilter = 0,
      numRuns = 100,
      run2 = 84
    )
  )

  long_manual <- flag_review_runs(
    make_review_detection(
      runLen = 5L,
      longRun = 3L,
      motusFilter = 0,
      numRuns = 100,
      run2 = 85
    )
  )

  expect_true(threshold$flag_sgss_noisy_hour_motus_filter)
  expect_false(too_few_runs$flag_sgss_noisy_hour_motus_filter)
  expect_equal(too_few_runs$review_reasons, "Motus filter")
  expect_false(too_few_twos$flag_sgss_noisy_hour_motus_filter)
  expect_equal(too_few_twos$review_reasons, "Motus filter")
  expect_true(long_manual$flag_motus_filter_review)
  expect_equal(long_manual$review_reasons, "Motus filter")
})


test_that("independent concerns survive the SG/SS Motus-filter bypass", {

  out <- flag_review_runs(
    make_review_detection(
      runLen = 4L,
      longRun = 3L,
      motusFilter = 0,
      numRuns = 100,
      run2 = 85,
      ambigID = 9L,
      runfreqsd = 0.06,
      freqsd = 0.2,
      pulseCount = 13000
    )
  )

  expect_true(out$flag_motus_filter)
  expect_false(out$flag_motus_filter_review)
  expect_true(out$flag_ambiguous)
  expect_true(out$flag_high_runfreqsd)
  expect_true(out$flag_high_hit_freqsd)
  expect_true(out$flag_high_pulse_activity)
  expect_true(out$needs_review)
  expect_equal(
    out$review_reasons,
    paste(
      "Ambiguous tag identity",
      "High run frequency SD",
      "High pulse activity",
      "High hit frequency SD",
      sep = "; "
    )
  )
})


test_that("unknown classifications are not bypassed", {

  unknown_receiver <- flag_review_runs(
    make_review_detection(
      runLen = 4L,
      longRun = 3L,
      motusFilter = 0,
      numRuns = 100,
      run2 = 85,
      receiver_type = "unknown"
    )
  )

  unknown_tag <- flag_review_runs(
    make_review_detection(
      runLen = 3L,
      longRun = 3L,
      motusFilter = 0,
      tag_technology = "unknown"
    )
  )

  expect_true(unknown_receiver$flag_motus_filter_review)
  expect_true(unknown_receiver$flag_unknown_receiver_type)
  expect_true(unknown_receiver$needs_review)
  expect_true(unknown_tag$flag_motus_filter_review)
  expect_true(unknown_tag$flag_unknown_tag_technology)
  expect_true(unknown_tag$needs_review)
})


test_that("Lotek receiver run-length behavior is unchanged", {

  structural <- flag_structural_exclusions(
    make_structural_detection(
      runLen = 2L,
      longRun = 1L,
      receiver_type = "Lotek"
    )
  )

  review <- flag_review_runs(
    make_review_detection(
      runLen = 3L,
      longRun = 3L,
      receiver_type = "Lotek"
    )
  )

  expect_true(structural$exclude_lotek_receiver_short_run)
  expect_false(structural$exclude_sgss_insufficient_consecutive)
  expect_true(review$flag_short_lotek_receiver_run)
  expect_true(review$needs_review)
  expect_equal(review$review_reasons, "Short run on Lotek receiver")
})


test_that("aggregate review counts use effective human-facing reasons", {

  raw <- list(
    alltags = tibble::tibble(
      source_database = rep("source_1", 3),
      runID = 1:3
    )
  )

  detections <- dplyr::bind_rows(
    flag_review_runs(
      make_review_detection(
        runLen = 3L,
        longRun = 2L,
        motusFilter = 0
      )
    ),
    flag_review_runs(
      make_review_detection(
        runLen = 4L,
        longRun = 2L,
        motusFilter = 0,
        numRuns = 100,
        run2 = 85
      )
    ),
    flag_review_runs(
      make_review_detection(
        runLen = 5L,
        longRun = 1L
      )
    )
  ) |>
    dplyr::mutate(runID = 1:3)

  excluded <- make_structural_detection(
    runLen = integer(),
    longRun = integer()
  ) |>
    flag_structural_exclusions()

  out <- summarize_preparation(raw, detections, excluded)

  expect_equal(
    sort(out$review$reason),
    sort(
      c(
        "Short run (3 hits)",
        "Noisy receiver activity",
        "Long run with no consecutive hits"
      )
    )
  )
  expect_true(all(out$review$n_runs == 1L))
})
