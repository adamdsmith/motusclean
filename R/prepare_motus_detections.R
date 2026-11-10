#' Prepare raw Motus detections for validation
#'
#' Extracts raw Motus detection data from one or more Motus SQLite
#' connections, attaches relevant run and receiver activity context,
#' calculates technology-specific QA metrics, applies structural exclusions,
#' and classifies remaining runs as PASS or REVIEW.
#'
#' Structural exclusions mean that a detection cannot be safely used for
#' tag-level validation. Exclusion does not necessarily mean that the
#' underlying radio-frequency event did not occur.
#'
#' @param x A DBI/Motus SQLite connection, or a list of connections.
#' @param source_names Optional character vector naming each input database.
#'   If omitted, list names are used when available; otherwise sources are
#'   named sequentially.
#' @param sg_min_run_length Minimum Lotek run length accepted on
#'   SensorGnome or SensorStation receivers.
#' @param lotek_review_min Minimum Lotek run length accepted from a Lotek
#'   receiver. Runs below this value are excluded.
#' @param lotek_pass_min Lotek-receiver run length at or above which the run
#'   does not require review solely because of run length.
#' @param ctt_min_run_length Minimum accepted CTT-tag run length.
#' @param longrun_min Minimum consecutive Lotek burst sequence required on
#'   SensorGnome/SensorStation receivers.
#' @param runfreqsd_review Lotek run-level frequency SD threshold triggering
#'   review.
#' @param pulse_count_review Receiver activity pulse-count threshold
#'   triggering review.
#'
#' @return An object of class `motus_clean`.
#'
#' @export
prepare_motus_detections <- function(
    x,
    source_names = NULL,
    sg_min_run_length = 3,
    lotek_review_min = 3,
    lotek_pass_min = 4,
    ctt_min_run_length = 2,
    longrun_min = 2,
    runfreqsd_review = 0.05,
    pulse_count_review = 12000
) {

  # ------------------------------------------------------------
  # Normalize input connections
  # ------------------------------------------------------------

  if (inherits(x, "DBIConnection")) {

    connections <- list(x)

  } else if (
    is.list(x) &&
    length(x) > 0 &&
    all(
      vapply(
        x,
        inherits,
        logical(1),
        what = "DBIConnection"
      )
    )
  ) {

    connections <- x

  } else {

    stop(
      "`x` must be a DBI/Motus connection or a list of DBI connections.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------
  # Source names
  # ------------------------------------------------------------

  if (is.null(source_names)) {

    existing_names <- names(connections)

    if (
      !is.null(existing_names) &&
      length(existing_names) == length(connections) &&
      all(
        !is.na(existing_names) &
        nzchar(existing_names)
      )
    ) {

      source_names <- existing_names

    } else {

      source_names <- paste0(
        "source_",
        seq_along(connections)
      )
    }
  }

  if (length(source_names) != length(connections)) {
    stop(
      "`source_names` must contain one name per input connection.",
      call. = FALSE
    )
  }

  if (
    any(is.na(source_names)) ||
    any(!nzchar(source_names))
  ) {
    stop(
      "`source_names` cannot contain missing or empty values.",
      call. = FALSE
    )
  }

  if (anyDuplicated(source_names)) {
    stop(
      "`source_names` must be unique.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------
  # Validate QA settings
  # ------------------------------------------------------------

  numeric_settings <- c(
    sg_min_run_length = sg_min_run_length,
    lotek_review_min = lotek_review_min,
    lotek_pass_min = lotek_pass_min,
    ctt_min_run_length = ctt_min_run_length,
    longrun_min = longrun_min,
    runfreqsd_review = runfreqsd_review,
    pulse_count_review = pulse_count_review
  )

  if (
    any(
      is.na(numeric_settings)
    ) ||
    any(
      !is.finite(numeric_settings)
    )
  ) {
    stop(
      "All QA threshold settings must be finite, non-missing numbers.",
      call. = FALSE
    )
  }

  if (
    sg_min_run_length < 2 ||
    lotek_review_min < 2 ||
    lotek_pass_min < lotek_review_min ||
    ctt_min_run_length < 2 ||
    longrun_min < 1 ||
    runfreqsd_review < 0 ||
    pulse_count_review < 0
  ) {
    stop(
      "Invalid QA threshold settings.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------
  # Extract source tables
  # ------------------------------------------------------------

  tables <- extract_motus_tables(
    connections = connections,
    source_names = source_names
  )


  # ------------------------------------------------------------
  # Resolve receiver-deployment attribution
  #
  # Motus alltags can occasionally contain multiple rows for the
  # same physical hit because that hit has been attached to more
  # than one receiver deployment.
  #
  # We resolve this only when recvDeps identifies exactly one
  # receiver deployment that was temporally valid when the hit
  # occurred.
  #
  # This occurs BEFORE run metrics are calculated so duplicated
  # receiver metadata cannot make one RF hit appear to be multiple
  # hits.
  # ------------------------------------------------------------

  det <- resolve_receiver_deployments(
    det = tables$alltags,
    recv_deps = tables$recv_deps
  )


  # ------------------------------------------------------------
  # Attach run and receiver-activity context
  # ------------------------------------------------------------

  det <- join_detection_context(
    alltags = det,
    runs = tables$runs,
    activity = tables$activity
  )


  # ------------------------------------------------------------
  # Standardize timestamps
  # ------------------------------------------------------------

  to_utc <- function(z) {

    if (inherits(z, "POSIXct")) {
      return(
        as.POSIXct(
          z,
          tz = "UTC"
        )
      )
    }

    as.POSIXct(
      z,
      origin = "1970-01-01",
      tz = "UTC"
    )
  }

  det <- det |>
    dplyr::mutate(

      ts = to_utc(
        tsCorrected
      ),

      tagDeployStart = to_utc(
        tagDeployStart
      ),

      tagDeployEnd = to_utc(
        tagDeployEnd
      ),

      date = as.Date(
        ts,
        tz = "UTC"
      )
    )


  # ------------------------------------------------------------
  # Tag and receiver technology
  # ------------------------------------------------------------

  det <- classify_detection_technology(
    det
  )


  # ------------------------------------------------------------
  # Calculate Lotek-specific run metrics
  # ------------------------------------------------------------

  det <- calculate_lotek_run_metrics(
    det
  )


  # ------------------------------------------------------------
  # Structural exclusions
  #
  # These runs cannot safely proceed to tag-level validation.
  # ------------------------------------------------------------

  det <- flag_structural_exclusions(
    det,
    sg_min_run_length = sg_min_run_length,
    lotek_review_min = lotek_review_min,
    ctt_min_run_length = ctt_min_run_length,
    longrun_min = longrun_min
  )


  # ------------------------------------------------------------
  # Split excluded and eligible detections
  # ------------------------------------------------------------

  excluded <- det |>
    dplyr::filter(
      exclude
    )

  eligible <- det |>
    dplyr::filter(
      !exclude
    )


  # ------------------------------------------------------------
  # Review flags
  # ------------------------------------------------------------

  eligible <- flag_review_runs(
    eligible,
    lotek_review_min = lotek_review_min,
    lotek_pass_min = lotek_pass_min,
    runfreqsd_review = runfreqsd_review,
    pulse_count_review = pulse_count_review
  )


  # ------------------------------------------------------------
  # Preparation summary
  # ------------------------------------------------------------

  summary <- summarize_preparation(
    raw = tables,
    detections = eligible,
    excluded = excluded
  )


  # ------------------------------------------------------------
  # Store settings used for reproducibility
  # ------------------------------------------------------------

  settings <- list(

    source_names = source_names,

    sg_min_run_length =
      sg_min_run_length,

    lotek_review_min =
      lotek_review_min,

    lotek_pass_min =
      lotek_pass_min,

    ctt_min_run_length =
      ctt_min_run_length,

    longrun_min =
      longrun_min,

    runfreqsd_review =
      runfreqsd_review,

    pulse_count_review =
      pulse_count_review
  )


  # ------------------------------------------------------------
  # Return motus_clean object
  # ------------------------------------------------------------

  out <- list(
    detections = eligible,
    excluded = excluded,
    summary = summary,
    settings = settings
  )

  class(out) <- c(
    "motus_clean",
    "list"
  )

  out
}
