#' Resolve duplicate receiver-deployment attribution
#'
#' Internal helper for resolving cases where the same physical Motus hit
#' appears multiple times because it has been associated with more than one
#' receiver deployment.
#'
#' A duplicated hit is resolved only when exactly one candidate receiver
#' deployment is temporally valid at the hit timestamp according to recvDeps.
#' If no candidate or more than one candidate is temporally valid, the
#' function stops rather than making an arbitrary geographic assignment.
#'
#' @param det Hit-level Motus detections.
#' @param recv_deps Receiver deployment metadata from `recvDeps`.
#'
#' @return A hit-level detection table containing one row per physical hit.
#'   Resolved rows have `recv_deploy_resolution == "temporal"`.
#'
#' @keywords internal
resolve_receiver_deployments <- function(
    det,
    recv_deps
) {

  required_det <- c(
    "source_database",
    "runID",
    "hitID",
    "tsCorrected",
    "recvDeployID"
  )

  missing_det <- setdiff(
    required_det,
    names(det)
  )

  if (length(missing_det) > 0) {
    stop(
      "`det` is missing required fields: ",
      paste(missing_det, collapse = ", "),
      call. = FALSE
    )
  }


  required_deps <- c(
    "source_database",
    "deployID",
    "tsStart",
    "tsEnd"
  )

  missing_deps <- setdiff(
    required_deps,
    names(recv_deps)
  )

  if (length(missing_deps) > 0) {
    stop(
      "`recv_deps` is missing required fields: ",
      paste(missing_deps, collapse = ", "),
      call. = FALSE
    )
  }


  # ------------------------------------------------------------
  # Identify physical hits represented more than once
  # ------------------------------------------------------------

  duplicate_hits <- det |>
    dplyr::count(
      source_database,
      runID,
      hitID,
      name = "n_rows"
    ) |>
    dplyr::filter(
      n_rows > 1
    )

  if (nrow(duplicate_hits) == 0) {

    return(
      det |>
        dplyr::mutate(
          recv_deploy_resolution = "not_needed"
        )
    )
  }


  # ------------------------------------------------------------
  # Separate ordinary hits from duplicated physical hits
  # ------------------------------------------------------------

  ordinary <- det |>
    dplyr::anti_join(
      duplicate_hits,
      by = c(
        "source_database",
        "runID",
        "hitID"
      )
    ) |>
    dplyr::mutate(
      recv_deploy_resolution = "not_needed"
    )

  duplicated <- det |>
    dplyr::semi_join(
      duplicate_hits,
      by = c(
        "source_database",
        "runID",
        "hitID"
      )
    )


  # ------------------------------------------------------------
  # Attach authoritative receiver-deployment time windows
  # ------------------------------------------------------------

  dep_lookup <- recv_deps |>
    dplyr::transmute(
      source_database = source_database,
      recvDeployID = deployID,
      recv_dep_ts_start = tsStart,
      recv_dep_ts_end = tsEnd
    )

  duplicated <- duplicated |>
    dplyr::left_join(
      dep_lookup,
      by = c(
        "source_database",
        "recvDeployID"
      ),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(

      hit_ts_numeric = as.numeric(
        tsCorrected
      ),

      recv_dep_ts_start_numeric = as.numeric(
        recv_dep_ts_start
      ),

      recv_dep_ts_end_numeric = as.numeric(
        recv_dep_ts_end
      ),

      recv_deploy_temporally_valid =
        !is.na(recv_dep_ts_start_numeric) &
        !is.na(hit_ts_numeric) &
        hit_ts_numeric >= recv_dep_ts_start_numeric &
        (
          is.na(recv_dep_ts_end_numeric) |
            hit_ts_numeric <= recv_dep_ts_end_numeric
        )
    )


  # ------------------------------------------------------------
  # Determine whether each duplicated physical hit has exactly
  # one temporally valid receiver deployment.
  # ------------------------------------------------------------

  resolution <- duplicated |>
    dplyr::group_by(
      source_database,
      runID,
      hitID
    ) |>
    dplyr::summarise(

      n_candidate_deployments =
        dplyr::n_distinct(
          recvDeployID,
          na.rm = TRUE
        ),

      n_valid_deployments =
        dplyr::n_distinct(
          recvDeployID[
            recv_deploy_temporally_valid %in% TRUE
          ],
          na.rm = TRUE
        ),

      valid_recvDeployID = {
        ids <- unique(
          recvDeployID[
            recv_deploy_temporally_valid %in% TRUE
          ]
        )

        ids <- ids[
          !is.na(ids)
        ]

        if (length(ids) == 1) {
          ids
        } else {
          NA
        }
      },

      .groups = "drop"
    )


  # ------------------------------------------------------------
  # Refuse ambiguous cases
  # ------------------------------------------------------------

  unresolved <- resolution |>
    dplyr::filter(
      n_valid_deployments != 1
    )

  if (nrow(unresolved) > 0) {

    example_text <- unresolved |>
      dplyr::slice_head(
        n = 10
      ) |>
      dplyr::mutate(
        example = paste0(
          "runID=",
          runID,
          ", hitID=",
          hitID,
          ", candidates=",
          n_candidate_deployments,
          ", temporally valid=",
          n_valid_deployments
        )
      ) |>
      dplyr::pull(
        example
      )

    stop(
      "Receiver-deployment attribution could not be resolved safely for ",
      nrow(unresolved),
      " duplicated physical hit(s).\n\n",
      paste(
        example_text,
        collapse = "\n"
      ),
      "\n\nNo receiver deployment was chosen arbitrarily.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------
  # Keep only the unique temporally valid deployment attribution
  # ------------------------------------------------------------

  resolved <- duplicated |>
    dplyr::inner_join(
      resolution |>
        dplyr::select(
          source_database,
          runID,
          hitID,
          valid_recvDeployID
        ),
      by = c(
        "source_database",
        "runID",
        "hitID"
      ),
      relationship = "many-to-one"
    ) |>
    dplyr::filter(
      recvDeployID == valid_recvDeployID
    ) |>
    dplyr::group_by(
      source_database,
      runID,
      hitID
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      recv_deploy_resolution = "temporal"
    ) |>
    dplyr::select(
      -recv_dep_ts_start,
      -recv_dep_ts_end,
      -hit_ts_numeric,
      -recv_dep_ts_start_numeric,
      -recv_dep_ts_end_numeric,
      -recv_deploy_temporally_valid,
      -valid_recvDeployID
    )


  # ------------------------------------------------------------
  # Recombine and restore original hit ordering
  # ------------------------------------------------------------

  out <- dplyr::bind_rows(
    ordinary,
    resolved
  ) |>
    dplyr::arrange(
      source_database,
      runID,
      tsCorrected,
      hitID
    )


  message(
    "Resolved ",
    nrow(resolution),
    " duplicated physical hit(s) using receiver-deployment time windows."
  )

  out
}
