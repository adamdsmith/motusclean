#' Join Motus run and receiver-hour context
#'
#' Internal helper that adds run timing and receiver-hour activity
#' information to the raw `alltags` detection table.
#'
#' @param alltags Raw Motus `alltags` table.
#' @param runs Raw Motus `runs` table.
#' @param activity Raw Motus `activity` table.
#'
#' @return A hit-level tibble enriched with receiver-hour context.
#' @keywords internal
join_detection_context <- function(alltags, runs, activity) {

  n_before <- nrow(alltags)

  # Derive the receiver-hour bin associated with each run.
  runs2 <- runs |>
    dplyr::mutate(
      hourBin = as.integer(tsBegin / 3600)
    ) |>
    dplyr::select(
      source_database,
      runID,
      hourBin
    )

  # Each detection hit belongs to one run.
  det <- alltags |>
    dplyr::left_join(
      runs2,
      by = c("source_database", "runID"),
      relationship = "many-to-one"
    )

  # Add receiver activity for the antenna/hour in which the run occurred.
  det <- det |>
    dplyr::left_join(
      activity,
      by = c(
        "source_database",
        "deviceID" = "motusDeviceID",
        "batchID",
        "hourBin",
        "port" = "ant"
      ),
      relationship = "many-to-one"
    )

  # Joins must never create or remove detection hits.
  if (nrow(det) != n_before) {
    stop(
      "Joining Motus run/activity context changed the number of detection hits. ",
      "This suggests duplicate or malformed run/activity records.",
      call. = FALSE
    )
  }

  det
}
