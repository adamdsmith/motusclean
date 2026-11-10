#' Flag structurally unusable Motus detections
#'
#' Internal helper that identifies detections/runs that are not safely usable
#' for tag-level validation. Exclusion does not necessarily imply that the
#' underlying RF event was false.
#'
#' @param det Hit-level Motus detection data with technology classifications
#'   and derived Lotek run metrics.
#' @param sg_min_run_length Minimum run length retained for Lotek tags detected
#'   by SensorGnome or SensorStation receivers.
#' @param lotek_review_min Minimum run length retained for Lotek tags detected
#'   by Lotek receivers. Runs below this value are excluded.
#' @param ctt_min_run_length Minimum run length retained for CTT tags.
#' @param longrun_min Minimum consecutive-hit run length required for Lotek tags
#'   detected by SensorGnome or SensorStation receivers.
#'
#' @return Detection data with structural exclusion flags, `exclude`, and
#'   `exclude_reason`.
#' @keywords internal
flag_structural_exclusions <- function(
    det,
    sg_min_run_length = 3,
    lotek_review_min = 3,
    ctt_min_run_length = 2,
    longrun_min = 2
) {

  det <- det |>
    dplyr::mutate(

      # ----------------------------------------------------------
      # Core identification / timing requirements
      # ----------------------------------------------------------

      exclude_missing_tag_id =
        is.na(motusTagID),

      exclude_missing_run_id =
        is.na(runID),

      exclude_missing_run_length =
        is.na(runLen),

      exclude_invalid_timestamp =
        is.na(tsCorrected),

      exclude_missing_tag_deploy_start =
        is.na(tagDeployStart),

      exclude_pre_tag_deployment =
        !is.na(tsCorrected) &
        !is.na(tagDeployStart) &
        tsCorrected < tagDeployStart,


      # ----------------------------------------------------------
      # Receiver deployment requirements
      # ----------------------------------------------------------

      exclude_missing_recv_deploy =
        is.na(recvDeployID),

      exclude_missing_recv_coords =
        is.na(recvDeployLat) |
        is.na(recvDeployLon),


      # ----------------------------------------------------------
      # Lotek tag identification requirements
      # ----------------------------------------------------------

      exclude_missing_lotek_bi =
        tag_technology == "Lotek" &
        (is.na(tagBI) | tagBI <= 0),


      # ----------------------------------------------------------
      # Technology-specific minimum evidence
      # ----------------------------------------------------------

      exclude_ctt_short_run =
        tag_technology == "CTT" &
        !is.na(runLen) &
        runLen < ctt_min_run_length,

      exclude_sgss_short_run =
        tag_technology == "Lotek" &
        receiver_type %in% c("SensorGnome", "SensorStation") &
        !is.na(runLen) &
        runLen < sg_min_run_length,

      exclude_sgss_insufficient_consecutive =
        tag_technology == "Lotek" &
        receiver_type %in% c("SensorGnome", "SensorStation") &
        !is.na(runLen) &
        runLen >= sg_min_run_length &
        runLen < 5 &
        !is.na(longRun) &
        longRun < longrun_min,

      exclude_lotek_receiver_short_run =
        tag_technology == "Lotek" &
        receiver_type == "Lotek" &
        !is.na(runLen) &
        runLen < lotek_review_min
    )

  # ------------------------------------------------------------
  # Propagate structural exclusions across the entire run
  #
  # The run, not the individual hit, is the validation unit.
  # If any hit triggers a structural exclusion, the whole run
  # receives that exclusion flag.
  # ------------------------------------------------------------

  det <- det |>
    dplyr::group_by(source_database, runID) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("exclude_"),
        ~ any(.x %in% TRUE)
      )
    ) |>
    dplyr::ungroup()

  exclude_flags <- c(
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

  det <- det |>
    dplyr::mutate(
      exclude = dplyr::if_any(
        dplyr::all_of(exclude_flags),
        ~ .x %in% TRUE
      )
    )

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

  det$exclude_reason <- collapse_flag_reasons(
    det,
    exclude_flag_map
  )

  det
}
