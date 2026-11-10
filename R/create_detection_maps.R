#' Create final validated detection maps
#'
#' Creates a multi-page PDF with one map per tagged individual using only
#' vetted detections in `x$detections` from a `motus_validated` object.
#'
#' Detections are aggregated to one event per individual, receiver deployment,
#' and UTC calendar date. Repeated station-days remain separate chronological
#' events internally. Map labels summarize repeated dates at a station using
#' the first date, last date, and station-day count rather than enumerating
#' every date.
#'
#' Lines and optional arrows connect successive observed station-day events in
#' chronological order. They provide observation sequence context and are not
#' inferred movement paths. When available and requested, the tag deployment
#' location is shown as a separate marker.
#'
#' Event points can use one neutral color, or be colored by fixed
#' meteorological season or calendar year. Seasons are Winter (December through
#' February), Spring (March through May), Summer (June through August), and
#' Fall (September through November).
#'
#' @param x A `motus_validated` object returned by `apply_validation()`.
#' @param file Character scalar giving the output PDF path.
#' @param color_by Point-color grouping. `"none"` uses neutral points,
#'   `"season"` colors points by fixed meteorological season, and `"year"`
#'   colors points by calendar year.
#' @param show_arrows Logical; show arrowheads on chronological connections.
#' @param show_deployment Logical; show the tag deployment location when
#'   coordinates are available.
#' @param label_stations Logical; include station names in map labels.
#' @param label_dates Logical; include station-day date summaries and counts in
#'   map labels.
#'
#' @return Invisibly returns `file`.
#'
#' @examples
#' \dontrun{
#' validated <- apply_validation(
#'   prepared_detections,
#'   "validation_review.csv"
#' )
#' create_detection_maps(
#'   validated,
#'   file = "detection_maps.pdf",
#'   color_by = "season"
#' )
#' }
#' @export
create_detection_maps <- function(
    x,
    file = "detection_maps.pdf",
    color_by = c("none", "season", "year"),
    show_arrows = TRUE,
    show_deployment = TRUE,
    label_stations = TRUE,
    label_dates = TRUE
) {
  if (!inherits(x, "motus_validated")) {
    stop("x must be a motus_validated object.", call. = FALSE)
  }
  if (!is.list(x) || !is.data.frame(x$detections)) {
    stop("x$detections must be a data frame.", call. = FALSE)
  }

  color_by <- match.arg(color_by)

  logical_options <- c(
    show_arrows = show_arrows,
    show_deployment = show_deployment,
    label_stations = label_stations,
    label_dates = label_dates
  )
  if (
    length(logical_options) != 4L ||
      any(is.na(logical_options)) ||
      !is.logical(logical_options)
  ) {
    stop(
      "show_arrows, show_deployment, label_stations, and label_dates ",
      "must each be one non-missing logical value.",
      call. = FALSE
    )
  }

  if (
    length(file) != 1L ||
      is.na(file) ||
      !is.character(file) ||
      !nzchar(file)
  ) {
    stop("file must be one non-empty path.", call. = FALSE)
  }

  map_data <- .prepare_detection_map_data(x$detections)
  events <- map_data$events
  deployments <- map_data$deployments
  labels <- .build_detection_map_labels(
    events,
    label_stations = label_stations,
    label_dates = label_dates
  )
  segments <- .build_detection_map_segments(events)

  world <- rnaturalearth::ne_countries(
    scale = "medium",
    returnclass = "sf"
  )
  states <- rnaturalearth::ne_states(
    country = c("united states of america", "canada"),
    returnclass = "sf"
  )

  animal_order <- events |>
    dplyr::group_by(animal_id) |>
    dplyr::summarise(
      motusTagID = .first_map_character(motusTagID),
      first_detection = min(first_detection),
      .groups = "drop"
    ) |>
    dplyr::arrange(motusTagID, first_detection, animal_id) |>
    dplyr::pull(animal_id)

  grDevices::pdf(
    file,
    width = 10,
    height = 8.5,
    onefile = TRUE,
    bg = "white"
  )
  device_open <- TRUE
  on.exit(
    {
      if (device_open) {
        grDevices::dev.off()
      }
    },
    add = TRUE
  )

  for (this_animal in animal_order) {
    animal_events <- events |>
      dplyr::filter(animal_id == this_animal)
    animal_segments <- segments |>
      dplyr::filter(animal_id == this_animal)
    animal_labels <- labels |>
      dplyr::filter(animal_id == this_animal)
    animal_deployment <- deployments |>
      dplyr::filter(animal_id == this_animal)

    if (!show_deployment) {
      animal_deployment <- animal_deployment[0, , drop = FALSE]
    }

    extent <- .detection_map_extent(
      c(animal_events$recvDeployLon, animal_deployment$lon),
      c(animal_events$recvDeployLat, animal_deployment$lat)
    )

    species_name <- .first_map_character(
      animal_events$speciesEN,
      fallback = "Unknown species"
    )
    mfg_id <- .first_map_character(
      animal_events$mfgID,
      fallback = "?"
    )
    motus_id <- .first_map_character(
      animal_events$motusTagID,
      fallback = "?"
    )
    animal_title <- paste0(
      species_name,
      ": ",
      mfg_id,
      " (",
      motus_id,
      ")"
    )

    years <- paste(sort(unique(animal_events$year)), collapse = ", ")
    interpretation <- if (show_arrows) {
      paste0(
        "Points are validated station-day detections; lines/arrows connect ",
        "successive observations and are not inferred movement paths."
      )
    } else {
      paste0(
        "Points are validated station-day detections; lines connect ",
        "successive observations and are not inferred movement paths."
      )
    }
    subtitle <- paste0(
      format(min(animal_events$date), "%Y-%m-%d"),
      " to ",
      format(max(animal_events$date), "%Y-%m-%d"),
      " | Years: ",
      years,
      "\n",
      interpretation
    )

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = world,
        fill = "grey95",
        colour = NA
      ) +
      ggplot2::geom_sf(
        data = states,
        fill = NA,
        colour = "grey65",
        linewidth = 0.35
      )

    if (nrow(animal_segments) > 0L) {
      p <- p +
        ggplot2::geom_segment(
          data = animal_segments,
          ggplot2::aes(
            x = from_lon,
            y = from_lat,
            xend = to_lon,
            yend = to_lat
          ),
          colour = "grey35",
          linewidth = 0.7,
          lineend = "round",
          arrow = .detection_map_arrow(show_arrows)
        )
    }

    if (color_by == "none") {
      p <- p +
        ggplot2::geom_point(
          data = animal_events,
          ggplot2::aes(x = recvDeployLon, y = recvDeployLat),
          shape = 21,
          fill = "grey35",
          colour = "black",
          stroke = 0.6,
          size = 4
        )
    } else if (color_by == "season") {
      p <- p +
        ggplot2::geom_point(
          data = animal_events,
          ggplot2::aes(
            x = recvDeployLon,
            y = recvDeployLat,
            fill = season
          ),
          shape = 21,
          colour = "black",
          stroke = 0.6,
          size = 4
        ) +
        ggplot2::scale_fill_manual(
          values = c(
            Winter = "#377EB8",
            Spring = "#4DAF4A",
            Summer = "#FF7F00",
            Fall = "#984EA3"
          ),
          drop = FALSE,
          name = "Season"
        )
    } else {
      year_levels <- sort(unique(animal_events$year))
      year_palette <- grDevices::hcl.colors(
        max(length(year_levels), 3L),
        palette = "Dark 3"
      )[seq_along(year_levels)]
      names(year_palette) <- as.character(year_levels)
      animal_events <- animal_events |>
        dplyr::mutate(
          year_group = factor(
            as.character(year),
            levels = as.character(year_levels)
          )
        )

      p <- p +
        ggplot2::geom_point(
          data = animal_events,
          ggplot2::aes(
            x = recvDeployLon,
            y = recvDeployLat,
            fill = year_group
          ),
          shape = 21,
          colour = "black",
          stroke = 0.6,
          size = 4
        ) +
        ggplot2::scale_fill_manual(
          values = year_palette,
          drop = FALSE,
          name = "Year"
        )
    }

    if (nrow(animal_deployment) > 0L) {
      deployment_label <- ifelse(
        is.na(animal_deployment$deployment_date),
        "Deployment",
        paste0(
          "Deployment\n",
          format(animal_deployment$deployment_date, "%Y-%m-%d")
        )
      )
      p <- p +
        ggplot2::geom_point(
          data = animal_deployment,
          ggplot2::aes(x = lon, y = lat),
          shape = 23,
          fill = "#984EA3",
          colour = "black",
          stroke = 0.7,
          size = 4.5
        ) +
        ggplot2::geom_label(
          data = animal_deployment,
          ggplot2::aes(x = lon, y = lat),
          label = deployment_label,
          nudge_y = extent$lat_span * 0.045,
          fill = "white",
          colour = "black",
          size = 3,
          label.padding = grid::unit(0.18, "lines")
        )
    }

    if (nrow(animal_labels) > 0L) {
      p <- p +
        ggrepel::geom_label_repel(
          data = animal_labels,
          ggplot2::aes(
            x = recvDeployLon,
            y = recvDeployLat,
            label = label
          ),
          fill = "white",
          colour = "black",
          segment.colour = "grey35",
          size = 3,
          box.padding = 0.8,
          point.padding = 0.6,
          force = 2,
          max.overlaps = Inf
        )
    }

    p <- p +
      ggplot2::coord_sf(
        xlim = extent$xlim,
        ylim = extent$ylim,
        expand = FALSE
      ) +
      ggplot2::labs(
        title = animal_title,
        subtitle = subtitle,
        x = NULL,
        y = NULL
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        panel.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = ggplot2::element_text(
          colour = "black",
          size = 16,
          hjust = 0.5
        ),
        plot.subtitle = ggplot2::element_text(
          colour = "black",
          size = 9,
          hjust = 0.5
        ),
        legend.position = if (color_by == "none") "none" else "right"
      )

    print(p)
  }

  grDevices::dev.off()
  device_open <- FALSE
  invisible(file)
}


.prepare_detection_map_data <- function(det) {
  required_fields <- c(
    "source_database", "motusTagID", "tagDeployID", "mfgID", "speciesEN",
    "runID", "ts", "recvDeployID", "recvDeployName", "recvSiteName", "recv",
    "recvDeployLat", "recvDeployLon", "tagDepLat", "tagDepLon",
    "tagDeployStart"
  )
  missing_fields <- setdiff(required_fields, names(det))
  if (length(missing_fields) > 0L) {
    stop(
      "x$detections is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (nrow(det) == 0L) {
    stop("x$detections contains no vetted detections.", call. = FALSE)
  }
  if (!inherits(det$ts, "POSIXt")) {
    stop("x$detections$ts must be a POSIX date-time vector.", call. = FALSE)
  }
  if (any(is.na(det$ts))) {
    stop("x$detections contains missing detection timestamps.", call. = FALSE)
  }
  if (
    any(is.na(det$source_database)) ||
      any(!nzchar(as.character(det$source_database))) ||
      any(is.na(det$motusTagID)) ||
      any(is.na(det$tagDeployID))
  ) {
    stop(
      "Vetted detections must have complete individual-key fields: ",
      "source_database, motusTagID, and tagDeployID.",
      call. = FALSE
    )
  }
  if (any(is.na(det$recvDeployID))) {
    stop("Vetted detections must have a receiver deployment ID.", call. = FALSE)
  }
  if (
    !is.numeric(det$recvDeployLat) ||
      !is.numeric(det$recvDeployLon) ||
      any(!is.finite(det$recvDeployLat)) ||
      any(!is.finite(det$recvDeployLon))
  ) {
    stop(
      "Vetted station detections have missing or nonfinite receiver coordinates.",
      call. = FALSE
    )
  }

  det <- det |>
    dplyr::mutate(
      .input_order = dplyr::row_number(),
      animal_id = paste(
        source_database,
        motusTagID,
        tagDeployID,
        sep = "::"
      ),
      station_name = dplyr::case_when(
        !is.na(recvDeployName) &
          nzchar(trimws(as.character(recvDeployName))) ~
          as.character(recvDeployName),
        !is.na(recvSiteName) &
          nzchar(trimws(as.character(recvSiteName))) ~
          as.character(recvSiteName),
        !is.na(recvDeployID) ~ paste("Receiver", recvDeployID),
        TRUE ~ as.character(recv)
      ),
      detection_date = as.Date(ts, tz = "UTC")
    )

  events <- det |>
    dplyr::group_by(
      animal_id,
      source_database,
      motusTagID,
      tagDeployID,
      recvDeployID,
      detection_date
    ) |>
    dplyr::summarise(
      mfgID = .first_map_character(mfgID),
      speciesEN = .first_map_character(speciesEN),
      station_name = .first_map_character(station_name),
      recvDeployLat = dplyr::first(recvDeployLat),
      recvDeployLon = dplyr::first(recvDeployLon),
      first_detection = min(ts),
      last_detection = max(ts),
      n_runs = dplyr::n_distinct(runID, na.rm = TRUE),
      n_hits = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::rename(date = detection_date) |>
    dplyr::mutate(
      year = as.integer(format(date, "%Y")),
      season = .detection_map_season(date)
    ) |>
    dplyr::group_by(animal_id) |>
    dplyr::arrange(
      first_detection,
      last_detection,
      as.character(recvDeployID),
      .by_group = TRUE
    ) |>
    dplyr::mutate(event_number = dplyr::row_number()) |>
    dplyr::ungroup()

  deployments <- det |>
    dplyr::filter(
      !is.na(tagDepLat),
      !is.na(tagDepLon),
      is.finite(tagDepLat),
      is.finite(tagDepLon)
    ) |>
    dplyr::arrange(.input_order) |>
    dplyr::group_by(animal_id) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      animal_id = animal_id,
      source_database = source_database,
      motusTagID = motusTagID,
      tagDeployID = tagDeployID,
      lat = tagDepLat,
      lon = tagDepLon,
      deployment_date = as.Date(tagDeployStart, tz = "UTC")
    )

  list(events = events, deployments = deployments)
}


.detection_map_season <- function(date) {
  month <- as.integer(format(date, "%m"))
  season <- dplyr::case_when(
    month %in% c(12L, 1L, 2L) ~ "Winter",
    month %in% 3:5 ~ "Spring",
    month %in% 6:8 ~ "Summer",
    month %in% 9:11 ~ "Fall",
    TRUE ~ NA_character_
  )
  factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))
}


.first_map_character <- function(x, fallback = NA_character_) {
  values <- as.character(x)
  values <- values[!is.na(values) & nzchar(trimws(values))]
  if (length(values) == 0L) {
    return(fallback)
  }
  values[[1]]
}


.build_detection_map_segments <- function(events) {
  events |>
    dplyr::group_by(animal_id) |>
    dplyr::arrange(event_number, .by_group = TRUE) |>
    dplyr::mutate(
      from_event_number = dplyr::lag(event_number),
      from_lon = dplyr::lag(recvDeployLon),
      from_lat = dplyr::lag(recvDeployLat),
      to_lon = recvDeployLon,
      to_lat = recvDeployLat
    ) |>
    dplyr::filter(!is.na(from_event_number)) |>
    dplyr::transmute(
      animal_id = animal_id,
      from_event_number = from_event_number,
      to_event_number = event_number,
      from_lon = from_lon,
      from_lat = from_lat,
      to_lon = to_lon,
      to_lat = to_lat
    ) |>
    dplyr::ungroup()
}


.build_detection_map_labels <- function(
    events,
    label_stations,
    label_dates
) {
  if (!label_stations && !label_dates) {
    return(
      tibble::tibble(
        animal_id = character(),
        recvDeployID = character(),
        recvDeployLat = numeric(),
        recvDeployLon = numeric(),
        label = character()
      )
    )
  }

  labels <- events |>
    dplyr::group_by(
      animal_id,
      recvDeployID,
      station_name,
      recvDeployLat,
      recvDeployLon
    ) |>
    dplyr::summarise(
      first_date = min(date),
      last_date = max(date),
      n_station_days = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      date_range = dplyr::if_else(
        n_station_days == 1L,
        format(first_date, "%Y-%m-%d"),
        paste0(
          format(first_date, "%Y-%m-%d"),
          " \u2013 ",
          format(last_date, "%Y-%m-%d")
        )
      ),
      station_day_count = paste(
        n_station_days,
        dplyr::if_else(
          n_station_days == 1L,
          "station-day",
          "station-days"
        )
      ),
      date_label = paste(
        date_range,
        station_day_count,
        sep = "\n"
      )
    )

  if (label_stations && label_dates) {
    labels <- labels |>
      dplyr::mutate(label = paste(station_name, date_label, sep = "\n"))
  } else if (label_stations) {
    labels <- labels |>
      dplyr::mutate(label = station_name)
  } else {
    labels <- labels |>
      dplyr::mutate(label = date_label)
  }

  labels
}


.detection_map_extent <- function(lon, lat) {
  lon <- lon[is.finite(lon)]
  lat <- lat[is.finite(lat)]
  if (length(lon) == 0L || length(lat) == 0L) {
    stop("No usable coordinates are available for a detection map.", call. = FALSE)
  }

  lon_span <- max(max(lon) - min(lon), 4)
  lat_span <- max(max(lat) - min(lat), 4)
  lon_pad <- max(lon_span * 0.18, 0.75)
  lat_pad <- max(lat_span * 0.18, 0.75)
  lon_center <- mean(range(lon))
  lat_center <- mean(range(lat))

  list(
    xlim = c(
      lon_center - (lon_span / 2) - lon_pad,
      lon_center + (lon_span / 2) + lon_pad
    ),
    ylim = c(
      lat_center - (lat_span / 2) - lat_pad,
      lat_center + (lat_span / 2) + lat_pad
    ),
    lon_span = lon_span,
    lat_span = lat_span
  )
}


.detection_map_arrow <- function(show_arrows) {
  if (!show_arrows) {
    return(NULL)
  }
  grid::arrow(
    angle = 25,
    length = grid::unit(0.14, "inches"),
    ends = "last",
    type = "closed"
  )
}
