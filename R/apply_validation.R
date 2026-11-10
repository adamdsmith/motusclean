#' Apply completed run-level validation decisions
#'
#' @param x A prepared `motus_clean` object.
#' @param review A completed validation review data frame or the path to a
#'   completed validation review CSV. May be `NULL` only when `x` contains no
#'   runs with `qa_status == "REVIEW"`.
#'
#' @return A `motus_validated` object.
#' @export
apply_validation <- function(x, review) {
  if (!inherits(x, "motus_clean")) {
    stop("`x` must be a `motus_clean` object.", call. = FALSE)
  }

  required_components <- c("detections", "excluded", "summary", "settings")
  missing_components <- setdiff(required_components, names(x))
  if (length(missing_components) > 0L) {
    stop(
      "`x` is missing required component(s): ",
      paste(missing_components, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!is.data.frame(x$detections) || !is.data.frame(x$excluded)) {
    stop("`x$detections` and `x$excluded` must be data frames.", call. = FALSE)
  }

  required_detection_columns <- c("source_database", "runID", "qa_status")
  missing_detection_columns <- setdiff(
    required_detection_columns,
    names(x$detections)
  )
  if (length(missing_detection_columns) > 0L) {
    stop(
      "`x$detections` is missing required column(s): ",
      paste(missing_detection_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  add_validation_keys <- function(data) {
    dplyr::mutate(
      data,
      .source_key = as.character(source_database),
      .run_key = as.character(runID)
    )
  }
  format_validation_keys <- function(data) {
    keys <- paste0(data$.source_key, " + ", data$.run_key)
    paste(utils::head(keys, 10L), collapse = ", ")
  }
  count_runs <- function(data) {
    if (nrow(data) == 0L) {
      return(0L)
    }
    nrow(dplyr::distinct(data, source_database, runID))
  }

  detections_keyed <- add_validation_keys(x$detections)
  missing_detection_keys <- detections_keyed |>
    dplyr::filter(
      is.na(.source_key) | !nzchar(trimws(.source_key)) |
        is.na(.run_key) | !nzchar(trimws(.run_key))
    )
  if (nrow(missing_detection_keys) > 0L) {
    stop(
      "`x$detections` contains missing `source_database` or `runID` values.",
      call. = FALSE
    )
  }

  invalid_status <- detections_keyed |>
    dplyr::filter(is.na(qa_status) | !(qa_status %in% c("PASS", "REVIEW")))
  if (nrow(invalid_status) > 0L) {
    stop(
      "`x$detections$qa_status` must contain only `PASS` or `REVIEW`.",
      call. = FALSE
    )
  }

  run_status <- detections_keyed |>
    dplyr::distinct(.source_key, .run_key, qa_status)
  inconsistent_status <- run_status |>
    dplyr::count(.source_key, .run_key, name = "n_status") |>
    dplyr::filter(n_status != 1L)
  if (nrow(inconsistent_status) > 0L) {
    stop(
      "A run has more than one `qa_status`: ",
      format_validation_keys(inconsistent_status),
      ".",
      call. = FALSE
    )
  }

  expected_review_keys <- run_status |>
    dplyr::filter(qa_status == "REVIEW") |>
    dplyr::select(.source_key, .run_key)

  if (is.null(review)) {
    if (nrow(expected_review_keys) > 0L) {
      stop(
        "`review` cannot be `NULL`: every REVIEW run requires a KEEP or REJECT decision.",
        call. = FALSE
      )
    }
    decisions <- tibble::tibble(
      source_database = character(),
      runID = character(),
      decision = character(),
      notes = character(),
      .source_key = character(),
      .run_key = character()
    )
  } else {
    if (is.character(review) && length(review) == 1L && !is.na(review)) {
      if (!file.exists(review)) {
        stop("Review CSV does not exist: ", review, call. = FALSE)
      }
      decisions <- utils::read.csv(
        review,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        colClasses = "character",
        na.strings = "NA"
      ) |>
        tibble::as_tibble()
    } else if (is.data.frame(review)) {
      decisions <- tibble::as_tibble(review)
    } else {
      stop("`review` must be a data frame or a single CSV path.", call. = FALSE)
    }

    required_review_columns <- c(
      "source_database", "runID", "decision", "notes"
    )
    missing_review_columns <- setdiff(required_review_columns, names(decisions))
    if (length(missing_review_columns) > 0L) {
      stop(
        "`review` is missing required column(s): ",
        paste(missing_review_columns, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    decisions <- decisions |>
      dplyr::mutate(
        source_database = as.character(source_database),
        runID = as.character(runID),
        decision = toupper(trimws(as.character(decision))),
        notes = as.character(notes),
        .source_key = source_database,
        .run_key = runID
      )

    missing_review_keys <- decisions |>
      dplyr::filter(
        is.na(.source_key) | !nzchar(trimws(.source_key)) |
          is.na(.run_key) | !nzchar(trimws(.run_key))
      )
    if (nrow(missing_review_keys) > 0L) {
      stop(
        "`review` contains missing `source_database` or `runID` values.",
        call. = FALSE
      )
    }

    duplicate_decisions <- decisions |>
      dplyr::count(.source_key, .run_key, name = "n_decisions") |>
      dplyr::filter(n_decisions != 1L)
    if (nrow(duplicate_decisions) > 0L) {
      stop(
        "`review` contains duplicate decisions for run(s): ",
        format_validation_keys(duplicate_decisions),
        ".",
        call. = FALSE
      )
    }

    invalid_decisions <- decisions |>
      dplyr::filter(is.na(decision) | !(decision %in% c("KEEP", "REJECT")))
    if (nrow(invalid_decisions) > 0L) {
      stop(
        "`review$decision` must contain exactly one KEEP or REJECT value for every row.",
        call. = FALSE
      )
    }

    unknown_decisions <- decisions |>
      dplyr::anti_join(
        expected_review_keys,
        by = c(".source_key", ".run_key")
      )
    if (nrow(unknown_decisions) > 0L) {
      stop(
        "`review` contains decision(s) for unknown or non-REVIEW run(s): ",
        format_validation_keys(unknown_decisions),
        ".",
        call. = FALSE
      )
    }

    missing_decisions <- expected_review_keys |>
      dplyr::anti_join(
        decisions,
        by = c(".source_key", ".run_key")
      )
    if (nrow(missing_decisions) > 0L) {
      stop(
        "`review` is missing decision(s) for REVIEW run(s): ",
        format_validation_keys(missing_decisions),
        ".",
        call. = FALSE
      )
    }
  }

  decision_lookup <- decisions |>
    dplyr::select(.source_key, .run_key, decision) |>
    dplyr::rename(.human_decision = decision)

  detections_decided <- detections_keyed |>
    dplyr::left_join(
      decision_lookup,
      by = c(".source_key", ".run_key"),
      relationship = "many-to-one"
    )

  vetted <- detections_decided |>
    dplyr::filter(qa_status == "PASS" | .human_decision == "KEEP") |>
    dplyr::select(-.source_key, -.run_key, -.human_decision)

  audit_lookup <- decisions |>
    dplyr::select(
      .source_key,
      .run_key,
      dplyr::any_of("review_id"),
      decision,
      notes
    ) |>
    dplyr::rename(
      validation_decision = decision,
      validation_notes = notes
    )
  if ("review_id" %in% names(audit_lookup)) {
    audit_lookup <- dplyr::rename(
      audit_lookup,
      validation_review_id = review_id
    )
  }

  rejected <- detections_decided |>
    dplyr::filter(qa_status == "REVIEW" & .human_decision == "REJECT") |>
    dplyr::select(-.human_decision) |>
    dplyr::left_join(
      audit_lookup,
      by = c(".source_key", ".run_key"),
      relationship = "many-to-one"
    ) |>
    dplyr::select(-.source_key, -.run_key)

  keep_keys <- decisions |>
    dplyr::filter(decision == "KEEP") |>
    dplyr::select(.source_key, .run_key)
  reject_keys <- decisions |>
    dplyr::filter(decision == "REJECT") |>
    dplyr::select(.source_key, .run_key)

  pass_hits <- sum(detections_keyed$qa_status == "PASS")
  review_hits <- sum(detections_keyed$qa_status == "REVIEW")
  keep_hits <- sum(detections_decided$.human_decision == "KEEP", na.rm = TRUE)
  reject_hits <- sum(detections_decided$.human_decision == "REJECT", na.rm = TRUE)

  validation_summary <- tibble::tibble(
    automated_pass_runs = sum(run_status$qa_status == "PASS"),
    automated_pass_hits = pass_hits,
    review_runs = nrow(expected_review_keys),
    review_hits = review_hits,
    human_keep_runs = nrow(keep_keys),
    human_keep_hits = keep_hits,
    human_reject_runs = nrow(reject_keys),
    human_reject_hits = reject_hits,
    vetted_runs = count_runs(vetted),
    vetted_hits = nrow(vetted),
    structural_excluded_runs = count_runs(x$excluded),
    structural_excluded_hits = nrow(x$excluded)
  )

  out <- list(
    detections = vetted,
    rejected = rejected,
    excluded = x$excluded,
    decisions = dplyr::select(decisions, -.source_key, -.run_key),
    summary = list(
      validation = validation_summary,
      preparation = x$summary
    ),
    settings = x$settings
  )
  class(out) <- c("motus_validated", "list")
  out
}