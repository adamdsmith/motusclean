#' Extract Motus tables needed for detection preparation
#'
#' Internal helper for extracting the minimum set of Motus tables used by
#' `prepare_motus_detections()`.
#'
#' @param connections A list of open DBI connections.
#' @param source_names Character vector giving the source name associated
#'   with each connection.
#'
#' @return A named list containing `alltags`, `runs`, `activity`, and
#'   `recv_deps`.
#'
#' @keywords internal
extract_motus_tables <- function(
    connections,
    source_names
) {

  # ------------------------------------------------------------
  # Validate connection/name lengths
  # ------------------------------------------------------------

  if (length(connections) != length(source_names)) {
    stop(
      "`connections` and `source_names` must have the same length.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------
  # Extract one source database
  # ------------------------------------------------------------

  extract_one <- function(
    con,
    source_name
  ) {

    if (!DBI::dbIsValid(con)) {
      stop(
        "Invalid DBI connection for source `",
        source_name,
        "`.",
        call. = FALSE
      )
    }


    # ----------------------------------------------------------
    # Required tables
    # ----------------------------------------------------------

    required_tables <- c(
      "alltags",
      "runs",
      "activity",
      "recvDeps"
    )

    available_tables <- DBI::dbListTables(
      con
    )

    missing_tables <- setdiff(
      required_tables,
      available_tables
    )

    if (length(missing_tables) > 0) {
      stop(
        "Source `",
        source_name,
        "` is missing required table(s): ",
        paste(
          missing_tables,
          collapse = ", "
        ),
        call. = FALSE
      )
    }


    # ----------------------------------------------------------
    # alltags
    # ----------------------------------------------------------

    alltags_raw <- DBI::dbReadTable(
      con,
      "alltags"
    )

    if (nrow(alltags_raw) == 0) {
      warning(
        "Source `",
        source_name,
        "` contains no rows in `alltags`.",
        call. = FALSE
      )
    }


    # recvDeployName is now the preferred identifying station
    # name. recvSiteName is retained as a legacy fallback.
    required_alltags_fields <- c(
      "tagProjID",
      "speciesID",
      "speciesEN",
      "mfg",
      "tagType",
      "codeSet",
      "tagModel",
      "mfgID",
      "motusTagID",
      "ambigID",
      "runID",
      "batchID",
      "hitID",
      "tsCorrected",
      "sig",
      "sigsd",
      "noise",
      "freq",
      "freqsd",
      "slop",
      "burstSlop",
      "tagBI",
      "runLen",
      "motusFilter",
      "tagDeployID",
      "tagDeployStart",
      "tagDeployEnd",
      "tagDepLat",
      "tagDepLon",
      "recvDeployID",
      "recvDeployName",
      "recvDeployLat",
      "recvDeployLon",
      "recv",
      "recvSiteName",
      "recvProjID",
      "deviceID",
      "port",
      "antType",
      "antBearing"
    )

    missing_alltags_fields <- setdiff(
      required_alltags_fields,
      names(alltags_raw)
    )

    if (length(missing_alltags_fields) > 0) {
      stop(
        "Source `",
        source_name,
        "` is missing required `alltags` field(s): ",
        paste(
          missing_alltags_fields,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    alltags <- alltags_raw |>
      dplyr::select(
        dplyr::all_of(
          required_alltags_fields
        )
      ) |>
      dplyr::mutate(
        source_database = source_name,
        .before = 1
      )


    # ----------------------------------------------------------
    # runs
    # ----------------------------------------------------------

    runs_raw <- DBI::dbReadTable(
      con,
      "runs"
    )

    required_runs_fields <- c(
      "runID",
      "tsBegin"
    )

    missing_runs_fields <- setdiff(
      required_runs_fields,
      names(runs_raw)
    )

    if (length(missing_runs_fields) > 0) {
      stop(
        "Source `",
        source_name,
        "` is missing required `runs` field(s): ",
        paste(
          missing_runs_fields,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    runs <- runs_raw |>
      dplyr::select(
        dplyr::all_of(
          required_runs_fields
        )
      ) |>
      dplyr::mutate(
        source_database = source_name,
        .before = 1
      )


    # ----------------------------------------------------------
    # activity
    # ----------------------------------------------------------

    activity_raw <- DBI::dbReadTable(
      con,
      "activity"
    )

    required_activity_fields <- c(
      "batchID",
      "motusDeviceID",
      "ant",
      "hourBin",
      "numTags",
      "pulseCount",
      "numRuns",
      "numHits",
      "run2",
      "run3",
      "run4",
      "run5",
      "run6",
      "run7plus"
    )

    missing_activity_fields <- setdiff(
      required_activity_fields,
      names(activity_raw)
    )

    if (length(missing_activity_fields) > 0) {
      stop(
        "Source `",
        source_name,
        "` is missing required `activity` field(s): ",
        paste(
          missing_activity_fields,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    activity <- activity_raw |>
      dplyr::select(
        dplyr::all_of(
          required_activity_fields
        )
      ) |>
      dplyr::mutate(
        source_database = source_name,
        .before = 1
      )


    # ----------------------------------------------------------
    # recvDeps
    #
    # This is used defensively to resolve cases where alltags
    # assigns a single physical hit to multiple receiver
    # deployments.
    # ----------------------------------------------------------

    recv_deps_raw <- DBI::dbReadTable(
      con,
      "recvDeps"
    )

    required_recv_dep_fields <- c(
      "deployID",
      "projectID",
      "serno",
      "receiverType",
      "deviceID",
      "status",
      "name",
      "siteName",
      "stationName",
      "fixtureType",
      "latitude",
      "longitude",
      "elevation",
      "isMobile",
      "tsStart",
      "tsEnd",
      "utcOffset",
      "stationID",
      "macAddress"
    )

    missing_recv_dep_fields <- setdiff(
      required_recv_dep_fields,
      names(recv_deps_raw)
    )

    if (length(missing_recv_dep_fields) > 0) {
      stop(
        "Source `",
        source_name,
        "` is missing required `recvDeps` field(s): ",
        paste(
          missing_recv_dep_fields,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    recv_deps <- recv_deps_raw |>
      dplyr::select(
        dplyr::all_of(
          required_recv_dep_fields
        )
      ) |>
      dplyr::mutate(
        source_database = source_name,
        .before = 1
      )


    # ----------------------------------------------------------
    # Return source tables
    # ----------------------------------------------------------

    list(
      alltags = alltags,
      runs = runs,
      activity = activity,
      recv_deps = recv_deps
    )
  }


  # ------------------------------------------------------------
  # Extract every supplied source
  # ------------------------------------------------------------

  extracted <- purrr::map2(
    connections,
    source_names,
    extract_one
  )


  # ------------------------------------------------------------
  # Combine like tables across sources
  # ------------------------------------------------------------

  list(

    alltags = purrr::map_dfr(
      extracted,
      "alltags"
    ),

    runs = purrr::map_dfr(
      extracted,
      "runs"
    ),

    activity = purrr::map_dfr(
      extracted,
      "activity"
    ),

    recv_deps = purrr::map_dfr(
      extracted,
      "recv_deps"
    )
  )
}
