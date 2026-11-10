#' Flag Motus runs requiring human review
#'
#' Internal helper that applies review-only QA rules to detections that have
#' already survived structural exclusion.
#'
#' Review flags do not imply that a detection is false. They identify runs
#' that deserve additional scrutiny before being accepted for downstream use.
#'
#' @param det Hit-level Motus detection data that have survived structural
#'   exclusion.
#' @param lotek_review_min Minimum run length retained for review on Lotek
#'   receivers.
#' @param lotek_pass_min Run length at or above which Lotek-receiver detections
#'   are not automatically reviewed solely because of run length.
#' @param runfreqsd_review Review threshold for median Lotek run frequency SD.
#' @param pulse_count_review Review threshold for hourly Motus-like pulse count.
#'
#' @return Detection data with review flags, `needs_review`, `qa_status`,
#'   and `review_reasons`.
#' @keywords internal
flag_review_runs <- function(
    det,
    lotek_review_min = 3,
    lotek_pass_min = 4,
    runfreqsd_review = 0.05,
    pulse_count_review = 12000
) {

  det <- det |>
    dplyr::mutate(

      # ----------------------------------------------------------
      # Generic review flags
      # ----------------------------------------------------------

      flag_motus_filter =
        !is.na(motusFilter) &
        motusFilter < 1,

      flag_sgss_short_motus_filter =
        flag_motus_filter &
        tag_technology == "Lotek" &
        receiver_type %in% c("SensorGnome", "SensorStation") &
        runLen %in% 3,

      flag_sgss_noisy_hour_motus_filter =
        flag_motus_filter &
        tag_technology == "Lotek" &
        receiver_type %in% c("SensorGnome", "SensorStation") &
        runLen %in% 4 &
        !is.na(numRuns) &
        !is.na(run2) &
        numRuns >= 100 &
        run2 >= 0.85 * numRuns,

      flag_motus_filter_review =
        flag_motus_filter &
        !(
          !is.na(longRun) &
            longRun >= 3 &
            (
              flag_sgss_short_motus_filter |
                flag_sgss_noisy_hour_motus_filter
            )
        ),

      flag_ambiguous =
        !is.na(ambigID),

      flag_unknown_tag_technology =
        tag_technology == "unknown",

      flag_unknown_receiver_type =
        receiver_type == "unknown",


      # ----------------------------------------------------------
      # Lotek-specific review flags
      # ----------------------------------------------------------

      flag_high_runfreqsd =
        tag_technology == "Lotek" &
        !is.na(runfreqsd) &
        runfreqsd > runfreqsd_review,

      flag_high_pulse_activity =
        tag_technology == "Lotek" &
        !is.na(pulseCount) &
        pulseCount > pulse_count_review,

      flag_excess_burst_slop =
        tag_technology == "Lotek" &
        !is.na(burstSlop) &
        !is.na(maxburstSlop) &
        abs(burstSlop) > maxburstSlop,

      flag_high_hit_freqsd =
        tag_technology == "Lotek" &
        !is.na(freqsd) &
        freqsd > 0.1 &
        !is.na(longRun) &
        longRun < 10,

      flag_sgss_long_run_no_consecutive =
        tag_technology == "Lotek" &
        receiver_type %in% c("SensorGnome", "SensorStation") &
        runLen >= 5 &
        !is.na(longRun) &
        longRun == 1,

      # ----------------------------------------------------------
      # Lotek-receiver run-length gray zone
      # ----------------------------------------------------------

      flag_short_lotek_receiver_run =
        tag_technology == "Lotek" &
        receiver_type == "Lotek" &
        !is.na(runLen) &
        runLen >= lotek_review_min &
        runLen < lotek_pass_min
    )

  # ------------------------------------------------------------
  # Propagate hit-level flags across the entire run
  #
  # The run is the validation unit. A single suspicious hit
  # therefore sends the entire run to human review.
  # ------------------------------------------------------------

  review_flags <- c(
    "flag_motus_filter_review",
    "flag_ambiguous",
    "flag_unknown_tag_technology",
    "flag_unknown_receiver_type",
    "flag_high_runfreqsd",
    "flag_high_hit_freqsd",
    "flag_high_pulse_activity",
    "flag_sgss_long_run_no_consecutive",
    "flag_short_lotek_receiver_run"
  )

  propagated_flags <- c(
    "flag_motus_filter",
    "flag_sgss_short_motus_filter",
    "flag_sgss_noisy_hour_motus_filter",
    review_flags
  )

  det <- det |>
    dplyr::group_by(source_database, runID) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(propagated_flags),
        ~ any(.x %in% TRUE)
      )
    ) |>
    dplyr::ungroup()

  # ------------------------------------------------------------
  # Assign run-level QA status
  # ------------------------------------------------------------

  det <- det |>
    dplyr::mutate(
      needs_review = dplyr::if_any(
        dplyr::all_of(review_flags),
        ~ .x %in% TRUE
      ),

      qa_status = dplyr::if_else(
        needs_review,
        "REVIEW",
        "PASS"
      )
    )

  # ------------------------------------------------------------
  # Human-readable review reasons
  # ------------------------------------------------------------

  review_flag_map <- c(
    flag_motus_filter_review =
      "Motus filter",

    flag_ambiguous =
      "Ambiguous tag identity",

    flag_unknown_tag_technology =
      "Unknown tag technology",

    flag_unknown_receiver_type =
      "Unknown receiver type",

    flag_high_runfreqsd =
      "High run frequency SD",

    flag_high_pulse_activity =
      "High pulse activity",

    flag_sgss_long_run_no_consecutive =
      "Long run with no consecutive hits",

    flag_short_lotek_receiver_run =
      "Short run on Lotek receiver",

    flag_high_hit_freqsd =
      "High hit frequency SD"
  )

  det$review_reasons <- collapse_flag_reasons(
    det,
    review_flag_map
  )

  det <- det |>
    dplyr::mutate(
      review_reasons = dplyr::case_when(
        flag_motus_filter_review &
          flag_sgss_noisy_hour_motus_filter ~ sub(
          "Motus filter",
          "Noisy receiver activity",
          review_reasons,
          fixed = TRUE
        ),
        flag_motus_filter_review & runLen == 2 ~ sub(
          "Motus filter",
          "Short run (2 hits)",
          review_reasons,
          fixed = TRUE
        ),
        flag_motus_filter_review & runLen == 3 ~ sub(
          "Motus filter",
          "Short run (3 hits)",
          review_reasons,
          fixed = TRUE
        ),
        TRUE ~ review_reasons
      )
    )

  det
}
