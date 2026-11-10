#' Create validation detection maps
#'
#' Internal helper used by `create_validation_package()` to generate
#' one geographic validation map per tagged animal.
#'
#' Maps show deployment location, all eligible detection sites, temporal
#' information, and whether each site contains detections requiring review.
#'
#' @param x A `motus_clean` object.
#' @param outfile Output PDF path.
#' @param background Either `"black"` or `"white"`.
#'
#' @return Invisibly returns `outfile`.
#' @keywords internal
create_validation_maps <- function(
    x,
    outfile,
    background = c("black", "white")
) {

  background <- match.arg(background)

  if (!inherits(x, "motus_clean")) {
    stop(
      "`x` must be a `motus_clean` object.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------
  # Theme colors
  # ------------------------------------------------------------

  if (background == "black") {

    bg <- "black"
    fg <- "white"
    border <- "grey35"
    pass_fill <- "#FF7F00"
    review_outline <- "#E31A1C"
    label_bg <- "grey15"

  } else {

    bg <- "white"
    fg <- "black"
    border <- "grey65"
    pass_fill <- "#FF7F00"
    review_outline <- "#E31A1C"
    label_bg <- "white"
  }


  # ------------------------------------------------------------
  # Only animals with at least one REVIEW run need validation maps
  # ------------------------------------------------------------

  animals_to_review <- x$detections |>
    dplyr::filter(
      qa_status == "REVIEW"
    ) |>
    dplyr::distinct(
      source_database,
      motusTagID,
      tagDeployID
    )

  if (nrow(animals_to_review) == 0) {
    stop(
      "There are no animals with REVIEW detections.",
      call. = FALSE
    )
  }


  det <- x$detections |>
    dplyr::semi_join(
      animals_to_review,
      by = c(
        "source_database",
        "motusTagID",
        "tagDeployID"
      )
    ) |>
    dplyr::mutate(

      site_name = dplyr::case_when(

        !is.na(recvDeployName) &
          recvDeployName != "" ~ recvDeployName,

        !is.na(recvSiteName) &
          recvSiteName != "" ~ recvSiteName,

        !is.na(recvDeployID) ~ paste(
          "Receiver",
          recvDeployID
        ),

        TRUE ~ recv
      ),

      det_date = as.Date(
        ts,
        tz = "UTC"
      ),

      animal_id = paste(
        source_database,
        motusTagID,
        tagDeployID,
        sep = "::"
      )
    )


  # ------------------------------------------------------------
  # Geographic layers
  # ------------------------------------------------------------

  world <- rnaturalearth::ne_countries(
    scale = "medium",
    returnclass = "sf"
  )

  states <- rnaturalearth::ne_states(
    country = c(
      "united states of america",
      "canada"
    ),
    returnclass = "sf"
  )


  # ------------------------------------------------------------
  # Helper: compact detection-date label
  # ------------------------------------------------------------

  format_date_range <- function(dates) {

    dates <- sort(
      unique(
        dates[
          !is.na(dates)
        ]
      )
    )

    if (length(dates) == 0) {
      return("")
    }

    if (length(dates) == 1) {
      return(
        format(
          dates,
          "%d %b %Y"
        )
      )
    }

    first_date <- min(dates)
    last_date <- max(dates)

    if (
      format(first_date, "%Y-%m") ==
      format(last_date, "%Y-%m")
    ) {

      return(
        paste0(
          format(first_date, "%d"),
          "\u2013",
          format(last_date, "%d %b %Y")
        )
      )
    }

    if (
      format(first_date, "%Y") ==
      format(last_date, "%Y")
    ) {

      return(
        paste0(
          format(first_date, "%d %b"),
          "\u2013",
          format(last_date, "%d %b %Y")
        )
      )
    }

    paste0(
      format(first_date, "%d %b %Y"),
      "\u2013",
      format(last_date, "%d %b %Y")
    )
  }


  # ------------------------------------------------------------
  # Animal order
  # ------------------------------------------------------------

  animal_order <- det |>
    dplyr::group_by(
      animal_id
    ) |>
    dplyr::summarise(
      motusTagID = dplyr::first(motusTagID),
      tagDeployStart = dplyr::first(tagDeployStart),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      motusTagID,
      tagDeployStart
    ) |>
    dplyr::pull(
      animal_id
    )


  # ------------------------------------------------------------
  # PDF
  # ------------------------------------------------------------

  grDevices::pdf(
    outfile,
    width = 10,
    height = 8.5,
    onefile = TRUE,
    bg = bg
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

    animal_det <- det |>
      dplyr::filter(
        animal_id == this_animal
      )


    # ----------------------------------------------------------
    # Animal metadata
    # ----------------------------------------------------------

    species_name <- animal_det$speciesEN[
      which(!is.na(animal_det$speciesEN))[1]
    ]

    if (
      length(species_name) == 0 ||
      is.na(species_name)
    ) {
      species_name <- "Unknown species"
    }

    mfg_id <- animal_det$mfgID[
      which(!is.na(animal_det$mfgID))[1]
    ]

    motus_id <- animal_det$motusTagID[
      which(!is.na(animal_det$motusTagID))[1]
    ]

    animal_title <- paste0(
      species_name,
      ": ",
      ifelse(
        length(mfg_id) == 0 ||
          is.na(mfg_id),
        "?",
        mfg_id
      ),
      " (",
      motus_id,
      ")"
    )


    # ----------------------------------------------------------
    # Detection-site summary
    # ----------------------------------------------------------

    sites <- animal_det |>
      dplyr::group_by(
        recvDeployID,
        recvDeployName,
        site_name,
        recvDeployLat,
        recvDeployLon
      ) |>
      dplyr::summarise(

        first_detection = min(
          ts,
          na.rm = TRUE
        ),

        last_detection = max(
          ts,
          na.rm = TRUE
        ),

        dates = list(
          sort(
            unique(det_date)
          )
        ),

        has_review = any(
          qa_status == "REVIEW",
          na.rm = TRUE
        ),

        has_pass = any(
          qa_status == "PASS",
          na.rm = TRUE
        ),

        .groups = "drop"
      ) |>
      dplyr::mutate(

        date_label = vapply(
          dates,
          format_date_range,
          character(1)
        ),

        label = paste0(
          site_name,
          "\n",
          date_label
        )
      )


    # ----------------------------------------------------------
    # Deployment location
    # ----------------------------------------------------------

    deployment <- animal_det |>
      dplyr::filter(
        !is.na(tagDepLat),
        !is.na(tagDepLon)
      ) |>
      dplyr::distinct(
        tagDepLat,
        tagDepLon,
        tagDeployStart
      ) |>
      dplyr::slice(1) |>
      dplyr::transmute(
        lat = tagDepLat,
        lon = tagDepLon,
        label = paste(
          "Deployed:",
          format(
            as.Date(
              tagDeployStart,
              tz = "UTC"
            ),
            "%d %b %Y"
          )
        )
      )


    # ----------------------------------------------------------
    # Automatic map extent
    #
    # Even when deployment and detections are very close together,
    # preserve enough surrounding geography to make the location
    # interpretable.
    # ----------------------------------------------------------

    map_lon <- c(
      sites$recvDeployLon,
      deployment$lon
    )

    map_lat <- c(
      sites$recvDeployLat,
      deployment$lat
    )

    map_lon <- map_lon[
      is.finite(map_lon)
    ]

    map_lat <- map_lat[
      is.finite(map_lat)
    ]

    if (
      length(map_lon) == 0 ||
      length(map_lat) == 0
    ) {
      warning(
        "Skipping validation map for Motus tag ",
        motus_id,
        ": no usable coordinates.",
        call. = FALSE
      )
      next
    }

    lon_min <- min(map_lon)
    lon_max <- max(map_lon)

    lat_min <- min(map_lat)
    lat_max <- max(map_lat)


    # Minimum geographic context
    min_lon_span <- 4
    min_lat_span <- 4

    lon_span <- max(
      lon_max - lon_min,
      min_lon_span
    )

    lat_span <- max(
      lat_max - lat_min,
      min_lat_span
    )


    # Pad around the observed extent
    lon_pad <- max(
      lon_span * 0.18,
      0.75
    )

    lat_pad <- max(
      lat_span * 0.18,
      0.75
    )


    # Center extent around actual detections/deployment.
    #
    # This matters when the observed span is smaller than the
    # minimum map span: simply padding min/max would otherwise
    # still produce an overly tight map.
    lon_center <- mean(
      range(map_lon)
    )

    lat_center <- mean(
      range(map_lat)
    )

    xlim <- c(
      lon_center -
        (lon_span / 2) -
        lon_pad,

      lon_center +
        (lon_span / 2) +
        lon_pad
    )

    ylim <- c(
      lat_center -
        (lat_span / 2) -
        lat_pad,

      lat_center +
        (lat_span / 2) +
        lat_pad
    )


    # ----------------------------------------------------------
    # Map
    # ----------------------------------------------------------

    p <- ggplot2::ggplot() +

      ggplot2::geom_sf(
        data = world,
        fill = if (background == "black") {
          "grey12"
        } else {
          "grey95"
        },
        colour = NA
      ) +

      ggplot2::geom_sf(
        data = states,
        fill = NA,
        colour = border,
        linewidth = 0.35
      ) +

      # Eligible detection locations
      ggplot2::geom_point(
        data = sites,
        ggplot2::aes(
          x = recvDeployLon,
          y = recvDeployLat
        ),
        shape = 21,
        fill = pass_fill,
        colour = fg,
        stroke = 0.6,
        size = 4
      ) +

      # REVIEW ring
      ggplot2::geom_point(
        data = sites |>
          dplyr::filter(has_review),
        ggplot2::aes(
          x = recvDeployLon,
          y = recvDeployLat
        ),
        shape = 21,
        fill = NA,
        colour = review_outline,
        stroke = 1.5,
        size = 5.5
      ) +

      # Deployment location
      ggplot2::geom_point(
        data = deployment,
        ggplot2::aes(
          x = lon,
          y = lat
        ),
        shape = 21,
        fill = "#984EA3",
        colour = fg,
        stroke = 0.7,
        size = 4.5
      ) +

      # Detection-site labels
      ggrepel::geom_label_repel(
        data = sites,
        ggplot2::aes(
          x = recvDeployLon,
          y = recvDeployLat,
          label = label
        ),
        fill = label_bg,
        colour = fg,
        segment.colour = fg,
        size = 3,
        box.padding = 0.8,
        point.padding = 0.6,
        force = 2,
        max.overlaps = Inf
      ) +

      # Deployment label deliberately offset from deployment point
      ggplot2::geom_label(
        data = deployment,
        ggplot2::aes(
          x = lon,
          y = lat,
          label = label
        ),
        nudge_y = lat_span * 0.045,
        fill = label_bg,
        colour = fg,
        size = 3,
        label.padding = grid::unit(
          0.18,
          "lines"
        )
      ) +

      ggplot2::coord_sf(
        xlim = xlim,
        ylim = ylim,
        expand = FALSE
      ) +

      ggplot2::labs(
        title = animal_title,
        subtitle = paste0(
          "Orange = eligible detection site; ",
          "red ring = site contains REVIEW run; ",
          "purple = deployment"
        ),
        x = NULL,
        y = NULL
      ) +

      ggplot2::theme_void() +

      ggplot2::theme(

        plot.background = ggplot2::element_rect(
          fill = bg,
          colour = NA
        ),

        panel.background = ggplot2::element_rect(
          fill = bg,
          colour = NA
        ),

        plot.title = ggplot2::element_text(
          colour = fg,
          size = 16,
          hjust = 0.5
        ),

        plot.subtitle = ggplot2::element_text(
          colour = fg,
          size = 9,
          hjust = 0.5
        )
      )

    print(p)
  }


  # ------------------------------------------------------------
  # Close PDF
  # ------------------------------------------------------------

  grDevices::dev.off()
  device_open <- FALSE

  invisible(outfile)
}
