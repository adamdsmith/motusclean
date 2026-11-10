#' Export vetted Motus detections
#'
#' Builds a versioned downstream artifact from a completed motus_validated
#' object. The authoritative detections table remains hit-level and contains
#' only automated PASS hits and REVIEW runs that were manually kept.
#'
#' The artifact makes the final disposition explicit so downstream projects do
#' not need to interpret motusclean validation internals. Rejected REVIEW hits
#' and structural exclusions are represented in audit counts but are not
#' included in the exported detections table.
#'
#' @param x A completed motus_validated object returned by apply_validation().
#' @param file Optional path at which to write the artifact as an RDS file.
#'   When NULL, no file is written.
#' @param overwrite Logical; permit replacement of an existing file. Replacement
#'   uses a temporary file in the destination directory and preserves the prior
#'   file if the new artifact cannot be moved into place.
#'
#' @return A motus_vetted_export object containing schema_version,
#'   artifact_status, build_metadata, detections, receiver_deployments, and
#'   qa_summary. If file is supplied, the same object is also written as RDS.
#'
#' @examples
#' \dontrun{
#' artifact <- export_vetted_detections(validated)
#' saveRDS(artifact, "vetted_motus_detections.rds")
#'
#' export_vetted_detections(
#'   validated,
#'   file = "vetted_motus_detections.rds"
#' )
#' }
#' @export
export_vetted_detections <- function(
    x,
    file = NULL,
    overwrite = FALSE
) {
  artifact <- .build_vetted_detection_artifact(x)

  if (!is.null(file)) {
    .write_vetted_detection_artifact(
      artifact,
      file = file,
      overwrite = overwrite
    )
  }

  artifact
}


.build_vetted_detection_artifact <- function(x) {
  validated <- .validate_vetted_export_input(x)
  retained <- validated$detections
  rejected <- validated$rejected
  excluded <- validated$excluded
  decisions <- validated$decisions

  audit_lookup <- decisions |>
    dplyr::select(
      .source_key,
      .run_key,
      review_id,
      notes
    ) |>
    dplyr::rename(
      validation_review_id = review_id,
      validation_notes = notes
    )

  retained <- retained |>
    dplyr::left_join(
      audit_lookup,
      by = c(".source_key", ".run_key"),
      relationship = "many-to-one"
    )

  receiver_name <- .vetted_receiver_name(retained)

  time_utc <- as.POSIXct(retained$ts, tz = "UTC")
  attr(time_utc, "tzone") <- "UTC"

  detections <- tibble::tibble(
    motus_detection_id = .make_motus_detection_id(
      retained$.source_key,
      retained$.run_key,
      retained$.hit_key
    ),
    tag_id = as.character(retained$motusTagID),
    time_utc = time_utc,
    source_database = retained$.source_key,
    run_id = retained$.run_key,
    hit_id = retained$.hit_key,
    receiver_deployment_id = as.character(retained$recvDeployID),
    receiver_name = receiver_name,
    lat = as.numeric(retained$recvDeployLat),
    lon = as.numeric(retained$recvDeployLon),
    observed_frequency = as.numeric(retained$freq),
    signal = as.numeric(retained$sig),
    signal_sd = as.numeric(retained$sigsd),
    noise = as.numeric(retained$noise),
    automated_qa_status = as.character(retained$qa_status),
    final_disposition = dplyr::if_else(
      retained$qa_status == "PASS",
      "automated_pass",
      "review_keep"
    ),
    validation_review_id = dplyr::if_else(
      retained$qa_status == "REVIEW",
      as.character(retained$validation_review_id),
      NA_character_
    ),
    validation_notes = dplyr::if_else(
      retained$qa_status == "REVIEW",
      as.character(retained$validation_notes),
      NA_character_
    ),
    recv_deploy_resolution = as.character(
      retained$recv_deploy_resolution
    ),
    motusTagID = as.character(retained$motusTagID),
    mfgID = .optional_export_character(retained, "mfgID"),
    speciesEN = .optional_export_character(retained, "speciesEN"),
    tagDeployID = .optional_export_character(retained, "tagDeployID"),
    batchID = .optional_export_character(retained, "batchID"),
    deviceID = .optional_export_character(retained, "deviceID"),
    port = .optional_export_character(retained, "port"),
    antType = .optional_export_character(retained, "antType"),
    antBearing = .optional_export_numeric(retained, "antBearing"),
    receiver_type = .optional_export_character(retained, "receiver_type"),
    runLen = .optional_export_numeric(retained, "runLen"),
    motusFilter = .optional_export_native(retained, "motusFilter"),
    review_reasons = .optional_export_character(retained, "review_reasons")
  )

  if (anyDuplicated(detections$motus_detection_id)) {
    stop(
      "Retained detections do not have unique physical hit keys ",
      "(source_database + runID + hitID).",
      call. = FALSE
    )
  }

  receiver_deployments <- .build_export_receiver_deployments(
    retained,
    receiver_name
  )

  counts <- .build_export_counts(
    retained = retained,
    rejected = rejected,
    excluded = excluded
  )
  qa_summary <- .build_export_qa_summary(
    counts = counts,
    excluded = excluded
  )

  artifact <- list(
    schema_version = "1.0.0",
    artifact_status = "complete",
    build_metadata = list(
      motusclean_version = .motusclean_version(),
      git_commit = .motusclean_git_commit(),
      exported_at_utc = .utc_now(),
      qa_settings = x$settings,
      counts = counts
    ),
    detections = detections,
    receiver_deployments = receiver_deployments,
    qa_summary = qa_summary
  )
  class(artifact) <- c("motus_vetted_export", "list")
  artifact
}


.validate_vetted_export_input <- function(x) {
  if (!inherits(x, "motus_validated")) {
    stop(
      "x must be a completed motus_validated object.",
      call. = FALSE
    )
  }

  required_components <- c(
    "detections",
    "rejected",
    "excluded",
    "decisions",
    "summary",
    "settings"
  )
  missing_components <- setdiff(required_components, names(x))
  if (length(missing_components) > 0L) {
    stop(
      "x is missing required component(s): ",
      paste(missing_components, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  data_components <- c("detections", "rejected", "excluded", "decisions")
  invalid_components <- data_components[
    !vapply(x[data_components], is.data.frame, logical(1))
  ]
  if (length(invalid_components) > 0L) {
    stop(
      "These motus_validated components must be data frames: ",
      paste(invalid_components, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_detection_fields <- c(
    "source_database",
    "runID",
    "hitID",
    "motusTagID",
    "ts",
    "recvDeployID",
    "recvDeployName",
    "recvSiteName",
    "recv",
    "recvDeployLat",
    "recvDeployLon",
    "freq",
    "sig",
    "sigsd",
    "noise",
    "qa_status",
    "recv_deploy_resolution"
  )
  .require_export_fields(
    x$detections,
    required_detection_fields,
    "x$detections"
  )
  .require_export_fields(
    x$rejected,
    c("source_database", "runID", "hitID", "qa_status"),
    "x$rejected"
  )
  .require_export_fields(
    x$excluded,
    c("source_database", "runID", "exclude_reason"),
    "x$excluded"
  )
  .require_export_fields(
    x$decisions,
    c("source_database", "runID", "decision", "notes"),
    "x$decisions"
  )

  if (!inherits(x$detections$ts, "POSIXt")) {
    stop("x$detections$ts must be a POSIX date-time vector.", call. = FALSE)
  }
  if (any(is.na(x$detections$ts))) {
    stop("Retained detections contain missing timestamps.", call. = FALSE)
  }

  detections <- .add_vetted_export_keys(
    x$detections,
    include_hit = TRUE
  )
  rejected <- .add_vetted_export_keys(
    x$rejected,
    include_hit = TRUE
  )
  excluded <- .add_vetted_export_keys(
    x$excluded,
    include_hit = FALSE
  )
  decisions <- .add_vetted_export_keys(
    x$decisions,
    include_hit = FALSE
  ) |>
    dplyr::mutate(
      decision = toupper(trimws(as.character(decision))),
      notes = as.character(notes),
      review_id = if ("review_id" %in% names(x$decisions)) {
        as.character(x$decisions$review_id)
      } else {
        rep(NA_character_, nrow(x$decisions))
      }
    )

  .require_complete_export_keys(
    detections,
    "Retained detections",
    include_hit = TRUE
  )
  .require_complete_export_keys(
    rejected,
    "Rejected detections",
    include_hit = TRUE
  )
  .require_complete_export_keys(
    decisions,
    "Validation decisions",
    include_hit = FALSE
  )

  invalid_retained_status <- !(
    detections$qa_status %in% c("PASS", "REVIEW")
  )
  if (any(is.na(detections$qa_status)) || any(invalid_retained_status)) {
    stop(
      "Retained detections must have automated qa_status PASS or REVIEW.",
      call. = FALSE
    )
  }
  if (
    nrow(rejected) > 0L &&
      (
        any(is.na(rejected$qa_status)) ||
          any(rejected$qa_status != "REVIEW")
      )
  ) {
    stop(
      "Rejected detections must all have automated qa_status REVIEW.",
      call. = FALSE
    )
  }

  retained_run_status <- detections |>
    dplyr::distinct(.source_key, .run_key, qa_status) |>
    dplyr::count(.source_key, .run_key, name = "n_status") |>
    dplyr::filter(n_status != 1L)
  if (nrow(retained_run_status) > 0L) {
    stop(
      "A retained run has inconsistent automated qa_status values.",
      call. = FALSE
    )
  }

  duplicate_retained_hits <- detections |>
    dplyr::count(
      .source_key,
      .run_key,
      .hit_key,
      name = "n_rows"
    ) |>
    dplyr::filter(n_rows != 1L)
  if (nrow(duplicate_retained_hits) > 0L) {
    stop(
      "Retained detections contain duplicate physical hit keys.",
      call. = FALSE
    )
  }

  duplicate_decisions <- decisions |>
    dplyr::count(.source_key, .run_key, name = "n_decisions") |>
    dplyr::filter(n_decisions != 1L)
  if (nrow(duplicate_decisions) > 0L) {
    stop(
      "Validation decisions contain duplicate run keys.",
      call. = FALSE
    )
  }
  if (
    any(is.na(decisions$decision)) ||
      any(!(decisions$decision %in% c("KEEP", "REJECT")))
  ) {
    stop(
      "Validation decisions must be exactly KEEP or REJECT.",
      call. = FALSE
    )
  }

  retained_review_runs <- detections |>
    dplyr::filter(qa_status == "REVIEW") |>
    dplyr::distinct(.source_key, .run_key) |>
    dplyr::mutate(.location = "retained")
  rejected_review_runs <- rejected |>
    dplyr::distinct(.source_key, .run_key) |>
    dplyr::mutate(.location = "rejected")
  review_runs <- dplyr::bind_rows(
    retained_review_runs,
    rejected_review_runs
  )

  duplicate_review_locations <- review_runs |>
    dplyr::count(.source_key, .run_key, name = "n_locations") |>
    dplyr::filter(n_locations != 1L)
  if (nrow(duplicate_review_locations) > 0L) {
    stop(
      "A REVIEW run appears in both retained and rejected detections.",
      call. = FALSE
    )
  }

  review_keys <- review_runs |>
    dplyr::select(.source_key, .run_key)

  missing_decisions <- review_keys |>
    dplyr::anti_join(
      decisions,
      by = c(".source_key", ".run_key")
    )
  if (nrow(missing_decisions) > 0L) {
    stop(
      "Cannot export: unresolved REVIEW runs are missing valid decisions.",
      call. = FALSE
    )
  }

  unknown_decisions <- decisions |>
    dplyr::anti_join(
      review_keys,
      by = c(".source_key", ".run_key")
    )
  if (nrow(unknown_decisions) > 0L) {
    stop(
      "Validation decisions contain unknown or non-REVIEW run keys.",
      call. = FALSE
    )
  }

  reviewed <- review_runs |>
    dplyr::left_join(
      decisions |>
        dplyr::select(.source_key, .run_key, decision),
      by = c(".source_key", ".run_key"),
      relationship = "one-to-one"
    )

  invalid_retained <- reviewed |>
    dplyr::filter(
      .location == "retained",
      decision != "KEEP"
    )
  if (nrow(invalid_retained) > 0L) {
    stop(
      "Retained REVIEW runs must have decision KEEP.",
      call. = FALSE
    )
  }
  invalid_rejected <- reviewed |>
    dplyr::filter(
      .location == "rejected",
      decision != "REJECT"
    )
  if (nrow(invalid_rejected) > 0L) {
    stop(
      "Rejected REVIEW runs must have decision REJECT.",
      call. = FALSE
    )
  }

  retained_rejected_overlap <- detections |>
    dplyr::distinct(.source_key, .run_key) |>
    dplyr::inner_join(
      rejected |>
        dplyr::distinct(.source_key, .run_key),
      by = c(".source_key", ".run_key")
    )
  if (nrow(retained_rejected_overlap) > 0L) {
    stop(
      "A run appears in both retained and rejected detections.",
      call. = FALSE
    )
  }

  complete_excluded_keys <- excluded |>
    dplyr::filter(
      !is.na(.source_key),
      nzchar(.source_key),
      !is.na(.run_key),
      nzchar(.run_key)
    ) |>
    dplyr::distinct(.source_key, .run_key)
  retained_excluded_overlap <- detections |>
    dplyr::distinct(.source_key, .run_key) |>
    dplyr::inner_join(
      complete_excluded_keys,
      by = c(".source_key", ".run_key")
    )
  rejected_excluded_overlap <- rejected |>
    dplyr::distinct(.source_key, .run_key) |>
    dplyr::inner_join(
      complete_excluded_keys,
      by = c(".source_key", ".run_key")
    )
  if (
    nrow(retained_excluded_overlap) > 0L ||
      nrow(rejected_excluded_overlap) > 0L
  ) {
    stop(
      "A run appears in structural exclusions and retained or rejected data.",
      call. = FALSE
    )
  }

  list(
    detections = detections,
    rejected = rejected,
    excluded = excluded,
    decisions = decisions
  )
}


.require_export_fields <- function(data, fields, label) {
  missing_fields <- setdiff(fields, names(data))
  if (length(missing_fields) > 0L) {
    stop(
      label,
      " is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}


.add_vetted_export_keys <- function(data, include_hit) {
  data <- data |>
    dplyr::mutate(
      .source_key = as.character(source_database),
      .run_key = as.character(runID)
    )
  if (include_hit) {
    data <- data |>
      dplyr::mutate(.hit_key = as.character(hitID))
  }
  data
}


.require_complete_export_keys <- function(data, label, include_hit) {
  incomplete <- is.na(data$.source_key) |
    !nzchar(trimws(data$.source_key)) |
    is.na(data$.run_key) |
    !nzchar(trimws(data$.run_key))
  if (include_hit) {
    incomplete <- incomplete |
      is.na(data$.hit_key) |
      !nzchar(trimws(data$.hit_key))
  }
  if (any(incomplete)) {
    stop(
      label,
      " contain missing source_database, runID, or hitID keys.",
      call. = FALSE
    )
  }
}


.make_motus_detection_id <- function(source, run, hit) {
  encode_component <- function(value) {
    value <- enc2utf8(as.character(value))
    paste0(
      nchar(value, type = "bytes"),
      ":",
      value
    )
  }
  paste(
    encode_component(source),
    encode_component(run),
    encode_component(hit),
    sep = "|"
  )
}


.vetted_receiver_name <- function(data) {
  dplyr::case_when(
    !is.na(data$recvDeployName) &
      nzchar(trimws(as.character(data$recvDeployName))) ~
      as.character(data$recvDeployName),
    !is.na(data$recvSiteName) &
      nzchar(trimws(as.character(data$recvSiteName))) ~
      as.character(data$recvSiteName),
    !is.na(data$recvDeployID) ~ paste(
      "Receiver",
      data$recvDeployID
    ),
    TRUE ~ as.character(data$recv)
  )
}


.build_export_receiver_deployments <- function(data, receiver_name) {
  candidates <- tibble::tibble(
    source_database = data$.source_key,
    receiver_deployment_id = as.character(data$recvDeployID),
    receiver_name = receiver_name,
    lat = as.numeric(data$recvDeployLat),
    lon = as.numeric(data$recvDeployLon),
    device_id = .optional_export_character(data, "deviceID"),
    receiver_type = .optional_export_character(data, "receiver_type")
  )

  inconsistent <- candidates |>
    dplyr::group_by(
      source_database,
      receiver_deployment_id
    ) |>
    dplyr::summarise(
      n_lat = dplyr::n_distinct(lat, na.rm = TRUE),
      n_lon = dplyr::n_distinct(lon, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(n_lat > 1L | n_lon > 1L)
  if (nrow(inconsistent) > 0L) {
    stop(
      "A receiver deployment has inconsistent retained coordinates.",
      call. = FALSE
    )
  }

  candidates |>
    dplyr::group_by(
      source_database,
      receiver_deployment_id
    ) |>
    dplyr::summarise(
      receiver_name = .first_export_value(receiver_name),
      lat = dplyr::first(lat),
      lon = dplyr::first(lon),
      device_id = .first_export_value(device_id),
      receiver_type = .first_export_value(receiver_type),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      source_database,
      receiver_deployment_id
    )
}


.first_export_value <- function(x) {
  values <- as.character(x)
  values <- values[
    !is.na(values) &
      nzchar(trimws(values))
  ]
  if (length(values) == 0L) {
    return(NA_character_)
  }
  values[[1]]
}


.optional_export_character <- function(data, name) {
  if (!(name %in% names(data))) {
    return(rep(NA_character_, nrow(data)))
  }
  as.character(data[[name]])
}


.optional_export_numeric <- function(data, name) {
  if (!(name %in% names(data))) {
    return(rep(NA_real_, nrow(data)))
  }
  as.numeric(data[[name]])
}


.optional_export_native <- function(data, name) {
  if (!(name %in% names(data))) {
    return(rep(NA, nrow(data)))
  }
  data[[name]]
}


.count_export_runs <- function(data) {
  if (nrow(data) == 0L) {
    return(0L)
  }
  as.integer(
    nrow(
      dplyr::distinct(
        data,
        .source_key,
        .run_key
      )
    )
  )
}


.build_export_counts <- function(retained, rejected, excluded) {
  pass <- retained |>
    dplyr::filter(qa_status == "PASS")
  keep <- retained |>
    dplyr::filter(qa_status == "REVIEW")

  list(
    retained_automated_pass_runs = .count_export_runs(pass),
    retained_automated_pass_hits = as.integer(nrow(pass)),
    retained_review_keep_runs = .count_export_runs(keep),
    retained_review_keep_hits = as.integer(nrow(keep)),
    rejected_review_runs = .count_export_runs(rejected),
    rejected_review_hits = as.integer(nrow(rejected)),
    structural_excluded_runs = .count_export_runs(excluded),
    structural_excluded_hits = as.integer(nrow(excluded)),
    unresolved_review_runs = 0L,
    total_exported_runs = .count_export_runs(retained),
    total_exported_detections = as.integer(nrow(retained))
  )
}


.build_export_qa_summary <- function(counts, excluded) {
  summary <- tibble::tibble(
    category = c(
      "retained_automated_pass",
      "retained_review_keep",
      "rejected_review",
      "structural_excluded",
      "unresolved_review",
      "exported_total"
    ),
    reason = rep(NA_character_, 6L),
    n_runs = c(
      counts$retained_automated_pass_runs,
      counts$retained_review_keep_runs,
      counts$rejected_review_runs,
      counts$structural_excluded_runs,
      counts$unresolved_review_runs,
      counts$total_exported_runs
    ),
    n_hits = c(
      counts$retained_automated_pass_hits,
      counts$retained_review_keep_hits,
      counts$rejected_review_hits,
      counts$structural_excluded_hits,
      0L,
      counts$total_exported_detections
    )
  )

  reason_summary <- .summarize_export_exclusions(excluded)
  if (nrow(reason_summary) > 0L) {
    summary <- dplyr::bind_rows(
      summary,
      dplyr::mutate(
        reason_summary,
        category = "structural_excluded_reason",
        .before = 1
      )
    )
  }

  summary
}


.summarize_export_exclusions <- function(excluded) {
  if (nrow(excluded) == 0L) {
    return(
      tibble::tibble(
        reason = character(),
        n_runs = integer(),
        n_hits = integer()
      )
    )
  }

  reason_text <- as.character(excluded$exclude_reason)
  reason_text[
    is.na(reason_text) |
      !nzchar(trimws(reason_text))
  ] <- "Unspecified structural exclusion"

  reasons <- strsplit(reason_text, "; ", fixed = TRUE)
  row_index <- rep(seq_len(nrow(excluded)), lengths(reasons))
  expanded <- tibble::tibble(
    .source_key = excluded$.source_key[row_index],
    .run_key = excluded$.run_key[row_index],
    reason = trimws(unlist(reasons, use.names = FALSE))
  )

  run_counts <- expanded |>
    dplyr::distinct(
      reason,
      .source_key,
      .run_key
    ) |>
    dplyr::count(
      reason,
      name = "n_runs"
    )
  hit_counts <- expanded |>
    dplyr::count(
      reason,
      name = "n_hits"
    )

  run_counts |>
    dplyr::left_join(
      hit_counts,
      by = "reason",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      n_runs = as.integer(n_runs),
      n_hits = as.integer(n_hits)
    ) |>
    dplyr::arrange(dplyr::desc(n_runs), reason)
}


.utc_now <- function() {
  value <- as.POSIXct(Sys.time(), tz = "UTC")
  attr(value, "tzone") <- "UTC"
  value
}


.motusclean_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("motusclean")),
    error = function(e) NA_character_
  )
}


.motusclean_git_commit <- function() {
  description <- tryCatch(
    utils::packageDescription(
      "motusclean",
      fields = "RemoteSha"
    ),
    error = function(e) NA_character_
  )
  if (
    length(description) == 1L &&
      !is.na(description) &&
      nzchar(description)
  ) {
    return(as.character(description))
  }

  package_path <- tryCatch(
    system.file(package = "motusclean"),
    error = function(e) ""
  )
  if (
    !nzchar(package_path) ||
      !dir.exists(file.path(package_path, ".git"))
  ) {
    return(NA_character_)
  }

  commit <- suppressWarnings(
    tryCatch(
      system2(
        "git",
        c(
          "-C",
          shQuote(
            normalizePath(
              package_path,
              winslash = "/",
              mustWork = TRUE
            )
          ),
          "rev-parse",
          "HEAD"
        ),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(e) character()
    )
  )
  status <- attr(commit, "status")
  if (
    length(commit) != 1L ||
      (!is.null(status) && status != 0L) ||
      !grepl("^[0-9a-fA-F]{40}$", commit)
  ) {
    return(NA_character_)
  }
  tolower(commit)
}


.write_vetted_detection_artifact <- function(
    artifact,
    file,
    overwrite
) {
  if (
    length(file) != 1L ||
      is.na(file) ||
      !is.character(file) ||
      !nzchar(file)
  ) {
    stop("file must be one non-empty path.", call. = FALSE)
  }
  if (
    length(overwrite) != 1L ||
      is.na(overwrite) ||
      !is.logical(overwrite)
  ) {
    stop("overwrite must be one non-missing logical value.", call. = FALSE)
  }

  destination <- normalizePath(
    file,
    winslash = "/",
    mustWork = FALSE
  )
  destination_dir <- dirname(destination)
  if (!dir.exists(destination_dir)) {
    stop(
      "The artifact destination directory does not exist: ",
      destination_dir,
      call. = FALSE
    )
  }
  if (file.exists(destination) && !overwrite) {
    stop(
      "Artifact file already exists. Use overwrite = TRUE to replace it.",
      call. = FALSE
    )
  }

  temporary <- tempfile(
    pattern = paste0(".", basename(destination), "-"),
    tmpdir = destination_dir,
    fileext = ".tmp"
  )
  backup <- NULL
  on.exit(
    {
      if (file.exists(temporary)) {
        unlink(temporary)
      }
      if (!is.null(backup) && file.exists(backup)) {
        if (!file.exists(destination)) {
          file.rename(backup, destination)
        }
        if (file.exists(backup)) {
          unlink(backup)
        }
      }
    },
    add = TRUE
  )

  saveRDS(
    artifact,
    temporary,
    version = 3
  )
  tryCatch(
    readRDS(temporary),
    error = function(e) {
      stop(
        "The temporary artifact could not be read back: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (file.exists(destination)) {
    backup <- tempfile(
      pattern = paste0(".", basename(destination), "-backup-"),
      tmpdir = destination_dir,
      fileext = ".rds"
    )
    if (!file.rename(destination, backup)) {
      stop(
        "Could not preserve the existing artifact before replacement.",
        call. = FALSE
      )
    }
  }

  if (!file.rename(temporary, destination)) {
    restored <- TRUE
    if (!is.null(backup) && file.exists(backup)) {
      restored <- file.rename(backup, destination)
    }
    stop(
      "Could not move the completed artifact into place.",
      if (!restored) {
        " The prior artifact also could not be restored automatically."
      } else {
        ""
      },
      call. = FALSE
    )
  }

  if (!is.null(backup) && file.exists(backup)) {
    unlink(backup)
  }

  invisible(destination)
}
