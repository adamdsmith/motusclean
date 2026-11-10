#' Summarize Motus detection preparation
#'
#' Internal helper that summarizes raw, excluded, pass, and review runs
#' produced during Motus detection preparation.
#'
#' @param raw List containing raw `alltags`, `runs`, and `activity` tables.
#' @param detections Eligible hit-level detections with `qa_status`.
#' @param excluded Hit-level detections excluded before human review.
#'
#' @return A list containing `overall`, `excluded`, and `review` summaries.
#' @keywords internal
summarize_preparation <- function(raw, detections, excluded) {

  # ------------------------------------------------------------
  # Overall counts
  # ------------------------------------------------------------

  overall <- tibble::tibble(
    sources = dplyr::n_distinct(raw$alltags$source_database),

    raw_hits = nrow(raw$alltags),

    raw_runs = raw$alltags |>
      dplyr::distinct(source_database, runID) |>
      nrow(),

    eligible_hits = nrow(detections),

    eligible_runs = detections |>
      dplyr::distinct(source_database, runID) |>
      nrow(),

    pass_runs = detections |>
      dplyr::filter(qa_status == "PASS") |>
      dplyr::distinct(source_database, runID) |>
      nrow(),

    review_runs = detections |>
      dplyr::filter(qa_status == "REVIEW") |>
      dplyr::distinct(source_database, runID) |>
      nrow(),

    excluded_hits = nrow(excluded),

    excluded_runs = excluded |>
      dplyr::distinct(source_database, runID) |>
      nrow()
  )


  # ------------------------------------------------------------
  # Exclusion summary
  #
  # Counts are not mutually exclusive because a run can carry
  # multiple exclusion reasons.
  # ------------------------------------------------------------

  exclude_flag_map <- c(
    exclude_missing_tag_id =
      "Missing Motus tag ID",

    exclude_missing_run_id =
      "Missing run ID",

    exclude_missing_run_length =
      "Missing run length",

    exclude_invalid_timestamp =
      "Invalid detection timestamp",

    exclude_missing_tag_deploy_start =
      "Missing tag deployment start",

    exclude_pre_tag_deployment =
      "Detection before tag deployment",

    exclude_missing_recv_deploy =
      "Missing receiver deployment ID",

    exclude_missing_recv_coords =
      "Missing receiver coordinates",

    exclude_missing_lotek_bi =
      "Missing or invalid Lotek burst interval",

    exclude_ctt_short_run =
      "CTT run below minimum run length",

    exclude_sgss_short_run =
      "Lotek run below SG/SS minimum run length",

    exclude_sgss_insufficient_consecutive =
      "Insufficient consecutive Lotek hits",

    exclude_lotek_receiver_short_run =
      "Lotek receiver run below review minimum"
  )

  excluded_summary <- excluded |>
    dplyr::distinct(source_database, runID, .keep_all = TRUE) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(names(exclude_flag_map)),
      names_to = "flag",
      values_to = "triggered"
    ) |>
    dplyr::filter(triggered %in% TRUE) |>
    dplyr::count(flag, name = "n_runs") |>
    dplyr::mutate(
      reason = unname(exclude_flag_map[flag])
    ) |>
    dplyr::select(
      reason,
      n_runs
    ) |>
    dplyr::arrange(dplyr::desc(n_runs))


  # ------------------------------------------------------------
  # Review summary
  #
  # Burst slop is intentionally excluded from the default review
  # summary because it is retained as a diagnostic only.
  # ------------------------------------------------------------

  review_summary <- detections |>
    dplyr::filter(qa_status == "REVIEW") |>
    dplyr::distinct(source_database, runID, .keep_all = TRUE) |>
    dplyr::select(
      source_database,
      runID,
      review_reasons
    ) |>
    tidyr::separate_longer_delim(
      review_reasons,
      delim = "; "
    ) |>
    dplyr::filter(
      !is.na(review_reasons),
      nzchar(review_reasons)
    ) |>
    dplyr::count(
      reason = review_reasons,
      name = "n_runs"
    ) |>
    dplyr::arrange(dplyr::desc(n_runs))


  # ------------------------------------------------------------
  # Return
  # ------------------------------------------------------------

  list(
    overall = overall,
    excluded = excluded_summary,
    review = review_summary
  )
}
