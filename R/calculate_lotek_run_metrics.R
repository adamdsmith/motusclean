#' Calculate Lotek run-level validation metrics
#'
#' Internal helper that calculates Lotek-specific burst structure,
#' frequency-stability, and burst-slop diagnostic metrics used for
#' detection validation.
#'
#' These metrics are relevant to Lotek-coded tags. For non-Lotek tags,
#' the derived fields remain `NA`.
#'
#' @param det Hit-level Motus detection data containing
#'   `source_database`, `runID`, `hitID`, `tsCorrected`, `tagBI`,
#'   `freqsd`, `burstSlop`, and `tag_technology`.
#'
#' @return Detection data with Lotek-specific validation metrics added.
#' @keywords internal
calculate_lotek_run_metrics <- function(det) {

  # ------------------------------------------------------------
  # Initialize derived fields for all detections
  # ------------------------------------------------------------

  det <- det |>
    dplyr::mutate(
      posInRun = NA_integer_,
      maxburstSlop = NA_real_,
      longRun = NA_integer_,
      maxRunLength = NA_integer_,
      runfreqsd = NA_real_,
      n_bad_bursts = NA_integer_,
      prop_bad_bursts = NA_real_
    )

  # Nothing else to do if there are no Lotek-tag detections.
  if (!any(det$tag_technology == "Lotek", na.rm = TRUE)) {
    return(det)
  }


  # ------------------------------------------------------------
  # Run-level frequency stability
  #
  # This does not depend on burst interval, so calculate it for
  # all Lotek runs, including any with missing/invalid tagBI.
  # ------------------------------------------------------------

  freq_summary <- det |>
    dplyr::filter(tag_technology == "Lotek") |>
    dplyr::group_by(source_database, runID) |>
    dplyr::summarise(
      runfreqsd = if (all(is.na(freqsd))) {
        NA_real_
      } else {
        stats::median(freqsd, na.rm = TRUE)
      },
      .groups = "drop"
    )

  det <- det |>
    dplyr::select(-runfreqsd) |>
    dplyr::left_join(
      freq_summary,
      by = c("source_database", "runID"),
      relationship = "many-to-one"
    )


  # ------------------------------------------------------------
  # Burst-structure metrics
  #
  # These require a valid Lotek burst interval.
  # ------------------------------------------------------------

  lotek_metrics <- det |>
    dplyr::filter(
      tag_technology == "Lotek",
      !is.na(tagBI),
      tagBI > 0
    ) |>
    dplyr::group_by(source_database, runID) |>
    dplyr::arrange(tsCorrected, .by_group = TRUE) |>
    dplyr::mutate(
      posInRun = as.integer(
        round(
          (as.numeric(tsCorrected) - as.numeric(min(tsCorrected))) /
            tagBI
        )
      ) + 1L,

      maxburstSlop =
        (4 + c(-3, diff(posInRun)) - 1) / 1000,

      longRun = max(
        rle(
          cumsum(c(1, diff(posInRun) - 1))
        )$lengths
      ),

      maxRunLength = max(posInRun)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      source_database,
      runID,
      hitID,
      posInRun,
      maxburstSlop,
      longRun,
      maxRunLength
    )


  # ------------------------------------------------------------
  # Burst-slop diagnostics
  #
  # burstSlop is a burst-level metric. We summarize how many
  # bursts within each Lotek run exceed the allowable slop.
  #
  # These fields are retained as diagnostics only. They do not,
  # by themselves, determine whether a run requires review.
  # ------------------------------------------------------------

  burst_summary <- lotek_metrics |>
    dplyr::left_join(
      det |>
        dplyr::select(
          source_database,
          runID,
          hitID,
          burstSlop
        ),
      by = c("source_database", "runID", "hitID"),
      relationship = "one-to-one"
    ) |>
    dplyr::group_by(source_database, runID) |>
    dplyr::summarise(
      n_bad_bursts = sum(
        !is.na(burstSlop) &
          !is.na(maxburstSlop) &
          abs(burstSlop) > maxburstSlop
      ),

      n_evaluable_bursts = sum(
        !is.na(burstSlop) &
          !is.na(maxburstSlop)
      ),

      prop_bad_bursts = dplyr::if_else(
        n_evaluable_bursts > 0,
        n_bad_bursts / n_evaluable_bursts,
        NA_real_
      ),

      .groups = "drop"
    ) |>
    dplyr::select(
      -n_evaluable_bursts
    )


  # ------------------------------------------------------------
  # Join Lotek metrics back to the hit-level detection table
  # ------------------------------------------------------------

  n_before <- nrow(det)

  det <- det |>
    dplyr::select(
      -posInRun,
      -maxburstSlop,
      -longRun,
      -maxRunLength,
      -n_bad_bursts,
      -prop_bad_bursts
    ) |>
    dplyr::left_join(
      lotek_metrics,
      by = c("source_database", "runID", "hitID"),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      burst_summary,
      by = c("source_database", "runID"),
      relationship = "many-to-one"
    )

  if (nrow(det) != n_before) {
    stop(
      "Calculating Lotek run metrics changed the number of detection hits.",
      call. = FALSE
    )
  }

  det
}
