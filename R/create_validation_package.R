#' Create a Motus validation package
#'
#' Creates files for manual validation of Motus detections requiring review.
#'
#' The validation package contains:
#'
#' - `validation_profiles.pdf` -- detection histories for animals with at
#'   least one REVIEW run
#' - `validation_maps.pdf` -- geographic and temporal context for animals
#'   with at least one REVIEW run
#' - `validation_review.csv` -- one row per REVIEW run
#' - `validation_summary.csv` -- preparation and review summaries
#'
#' The profile PDF is organized by animal. Within each animal, panels
#' represent receiver/site-day combinations ordered chronologically.
#' PASS detections are retained to provide context around REVIEW runs.
#'
#' The map PDF provides geographic and temporal context for the same
#' animals, including deployment location, eligible detection sites, and
#' sites containing REVIEW runs.
#'
#' Review decisions are made at the run level. The CSV is arranged so that
#' runs appearing in the same PDF panel are contiguous, allowing reviewers
#' to bulk-fill KEEP or REJECT decisions when appropriate.
#'
#' @param x A `motus_clean` object returned by
#'   `prepare_motus_detections()`.
#' @param dir Directory in which validation files are created.
#' @param overwrite Logical. If `FALSE`, existing validation files are not
#'   overwritten.
#' @param panels_per_page Number of receiver-day panels per PDF page.
#'   Currently fixed at 8 to preserve a consistent 2-column by 4-row layout.
#' @param background PDF background. Either `"black"` for screen review
#'   or `"white"` for printing. Defaults to `"black"`.
#'
#' @return Invisibly returns a named list of created file paths.
#'
#' @export
create_validation_package <- function(
    x,
    dir = "motus_validation",
    overwrite = FALSE,
    panels_per_page = 8,
    background = c("black", "white")
) {

  # ------------------------------------------------------------
  # Validate input
  # ------------------------------------------------------------

  if (!inherits(x, "motus_clean")) {
    stop(
      "`x` must be a `motus_clean` object returned by ",
      "`prepare_motus_detections()`.",
      call. = FALSE
    )
  }

  if (
    length(panels_per_page) != 1 ||
    is.na(panels_per_page) ||
    panels_per_page != 8
  ) {
    stop(
      "`panels_per_page` is currently fixed at 8 ",
      "(2 columns x 4 rows).",
      call. = FALSE
    )
  }

  background <- match.arg(background)


  # ------------------------------------------------------------
  # Background-dependent plotting palette
  # ------------------------------------------------------------

  if (background == "black") {

    bg <- "black"
    fg <- "white"
    panel_bg <- "black"
    grid_major <- "grey35"
    strip_fill <- "grey30"
    strip_border <- "grey20"
    strip_text <- "white"
    pass_outline <- "#888888"
    unknown_run_color <- "#555555"

  } else {

    bg <- "white"
    fg <- "black"
    panel_bg <- "white"
    grid_major <- "grey80"
    strip_fill <- "grey85"
    strip_border <- "grey65"
    strip_text <- "black"
    pass_outline <- "#777777"
    unknown_run_color <- "#444444"
  }


  # ------------------------------------------------------------
  # Fixed plotting conventions
  # ------------------------------------------------------------

  run_length_levels <- c(
    "2",
    "3\u20135",
    "6\u201310",
    "11\u201320",
    ">20"
  )

  run_length_colors <- c(
    "2" = "#FFD92F",
    "3\u20135" = "#E31A1C",
    "6\u201310" = "#FDAE61",
    "11\u201320" = "#A6CEE3",
    ">20" = "#1F78B4"
  )

  qa_colors <- c(
    "PASS" = pass_outline,
    "REVIEW" = "#E31A1C"
  )


  # ------------------------------------------------------------
  # Output paths
  # ------------------------------------------------------------

  dir.create(
    dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  profile_file <- file.path(
    dir,
    "validation_profiles.pdf"
  )

  map_file <- file.path(
    dir,
    "validation_maps.pdf"
  )

  review_file <- file.path(
    dir,
    "validation_review.csv"
  )

  summary_file <- file.path(
    dir,
    "validation_summary.csv"
  )

  output_files <- c(
    profile_file,
    map_file,
    review_file,
    summary_file
  )

  if (!overwrite && any(file.exists(output_files))) {

    existing <- output_files[
      file.exists(output_files)
    ]

    stop(
      "Validation output already exists:\n",
      paste0(
        "  ",
        existing,
        collapse = "\n"
      ),
      "\n\nUse `overwrite = TRUE` only if you intend to replace it.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------
  # Plotting convenience fields used by BOTH the CSV and PDF
  # ------------------------------------------------------------

  plot_base <- x$detections |>
    dplyr::mutate(

      site_label = dplyr::case_when(

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

      panel_date = as.Date(
        ts,
        tz = "UTC"
      ),

      panel = paste(
        site_label,
        panel_date,
        sep = ": "
      ),

      antenna = dplyr::case_when(

        antBearing >= 315 |
          antBearing < 45 ~ "N",

        antBearing >= 45 &
          antBearing < 135 ~ "E",

        antBearing >= 135 &
          antBearing < 225 ~ "S",

        antBearing >= 225 &
          antBearing < 315 ~ "W",

        TRUE ~ "Omni / missing"
      ),

      rlClass = dplyr::case_when(

        runLen == 2 ~ "2",

        runLen >= 3 &
          runLen <= 5 ~ "3\u20135",

        runLen >= 6 &
          runLen <= 10 ~ "6\u201310",

        runLen >= 11 &
          runLen <= 20 ~ "11\u201320",

        runLen > 20 ~ ">20",

        TRUE ~ "Unknown"
      ),

      rlClass = factor(
        rlClass,
        levels = c(
          run_length_levels,
          "Unknown"
        )
      )
    )


  # ------------------------------------------------------------
  # Identify REVIEW runs
  # ------------------------------------------------------------

  review_runs <- plot_base |>
    dplyr::filter(
      qa_status == "REVIEW"
    ) |>
    dplyr::group_by(
      source_database,
      runID
    ) |>
    dplyr::summarise(

      motusTagID = dplyr::first(
        motusTagID
      ),

      mfgID = dplyr::first(
        mfgID
      ),

      speciesEN = dplyr::first(
        speciesEN
      ),

      tagDeployID = dplyr::first(
        tagDeployID
      ),

      recvDeployID = dplyr::first(
        recvDeployID
      ),

      recvDeployName = dplyr::first(
        recvDeployName
      ),

      panel = dplyr::first(
        panel
      ),

      panel_date = dplyr::first(
        panel_date
      ),

      port = dplyr::first(
        port
      ),

      antenna = dplyr::first(
        antenna
      ),

      run_start = min(
        ts,
        na.rm = TRUE
      ),

      runLen = dplyr::first(
        runLen
      ),

      longRun = dplyr::first(
        longRun
      ),

      runfreqsd = dplyr::first(
        runfreqsd
      ),

      pulseCount = dplyr::first(
        pulseCount
      ),

      n_bad_bursts = dplyr::first(
        n_bad_bursts
      ),

      prop_bad_bursts = dplyr::first(
        prop_bad_bursts
      ),

      review_reasons = dplyr::first(
        review_reasons
      ),

      .groups = "drop"
    )

  if (nrow(review_runs) == 0) {
    stop(
      "There are no runs with `qa_status == \"REVIEW\"`.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------
  # Stable animal identifier
  # ------------------------------------------------------------

  review_runs <- review_runs |>
    dplyr::mutate(
      animal_id = paste(
        source_database,
        motusTagID,
        tagDeployID,
        sep = "::"
      )
    )


  # ------------------------------------------------------------
  # Sort in the same logical order as the PDF
  #
  # Within an animal:
  #   panel/date -> port -> time
  #
  # This makes blocks of CSV rows correspond naturally to the
  # panels the reviewer is looking at.
  # ------------------------------------------------------------

  review_runs <- review_runs |>
    dplyr::arrange(
      motusTagID,
      panel_date,
      panel,
      port,
      run_start,
      source_database,
      runID
    ) |>
    dplyr::mutate(
      review_id = sprintf(
        "R%04d",
        dplyr::row_number()
      )
    )


  # ------------------------------------------------------------
  # Human-facing review CSV
  #
  # Everything needed for review comes first.
  #
  # Machine/audit fields deliberately come AFTER notes so they
  # do not clutter normal spreadsheet review.
  # ------------------------------------------------------------

  review_table <- review_runs |>
    dplyr::transmute(

      # Human-facing review fields
      review_id = review_id,

      motusTagID = motusTagID,

      mfgID = mfgID,

      speciesEN = speciesEN,

      panel = panel,

      port = port,

      antenna = antenna,

      time = format(
        run_start,
        "%H:%M:%S",
        tz = "UTC"
      ),

      runLen = runLen,

      longRun = longRun,

      runfreqsd = runfreqsd,

      pulseCount = pulseCount,

      n_bad_bursts = n_bad_bursts,

      prop_bad_bursts = prop_bad_bursts,

      review_reasons = review_reasons,

      decision = "",

      notes = "",


      # --------------------------------------------------------
      # Machine/audit fields
      #
      # Reviewers normally do not need to touch anything below
      # this point.
      # --------------------------------------------------------

      source_database = source_database,

      runID = runID,

      tagDeployID = tagDeployID,

      recvDeployID = recvDeployID,

      recvDeployName = recvDeployName
    )

  utils::write.csv(
    review_table,
    review_file,
    row.names = FALSE,
    na = ""
  )


  # ------------------------------------------------------------
  # Validation summary CSV
  # ------------------------------------------------------------

  overall_summary <- x$summary$overall |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "item",
      values_to = "value"
    ) |>
    dplyr::mutate(
      section = "overall",
      .before = 1
    )

  exclusion_summary <- x$summary$excluded |>
    dplyr::transmute(
      section = "excluded",
      item = reason,
      value = n_runs
    )

  review_summary <- x$summary$review |>
    dplyr::transmute(
      section = "review",
      item = reason,
      value = n_runs
    )

  validation_summary <- dplyr::bind_rows(
    overall_summary,
    exclusion_summary,
    review_summary
  )

  utils::write.csv(
    validation_summary,
    summary_file,
    row.names = FALSE,
    na = ""
  )


  # ------------------------------------------------------------
  # Create geographic validation maps
  # ------------------------------------------------------------

  create_validation_maps(
    x,
    outfile = map_file,
    background = background
  )


  # ------------------------------------------------------------
  # Prepare profile PDF plotting data
  #
  # Include ALL eligible detections for animals containing at
  # least one REVIEW run.
  # ------------------------------------------------------------

  animal_lookup <- review_runs |>
    dplyr::distinct(
      source_database,
      motusTagID,
      tagDeployID,
      animal_id
    )

  plot_data <- plot_base |>
    dplyr::inner_join(
      animal_lookup,
      by = c(
        "source_database",
        "motusTagID",
        "tagDeployID"
      ),
      relationship = "many-to-one"
    ) |>
    dplyr::rename(
      panel_label = panel,
      antBin = antenna
    )


  # ------------------------------------------------------------
  # Animal order follows review CSV order
  # ------------------------------------------------------------

  animal_order <- review_runs |>
    dplyr::distinct(
      animal_id,
      .keep_all = TRUE
    ) |>
    dplyr::pull(
      animal_id
    )


  # ------------------------------------------------------------
  # Build shared legend
  # ------------------------------------------------------------

  legend_run_data <- tibble::tibble(
    rlClass = factor(
      run_length_levels,
      levels = run_length_levels
    ),
    x = seq_along(
      run_length_levels
    ),
    y = 1
  )

  legend_status_data <- tibble::tibble(
    qa_status = factor(
      c(
        "PASS",
        "REVIEW"
      ),
      levels = c(
        "PASS",
        "REVIEW"
      )
    ),
    x = 1:2,
    y = 1
  )

  legend_plot <- ggplot2::ggplot() +

    ggplot2::geom_point(
      data = legend_run_data,
      ggplot2::aes(
        x = x,
        y = y,
        fill = rlClass
      ),
      shape = 21,
      size = 4,
      colour = fg,
      stroke = 0.8
    ) +

    ggplot2::geom_point(
      data = legend_status_data,
      ggplot2::aes(
        x = x,
        y = y,
        colour = qa_status
      ),
      shape = 21,
      size = 4,
      fill = bg,
      stroke = 1.2
    ) +

    ggplot2::scale_fill_manual(
      values = run_length_colors,
      limits = run_length_levels,
      breaks = run_length_levels,
      drop = FALSE,
      name = "Run length"
    ) +

    ggplot2::scale_colour_manual(
      values = qa_colors,
      limits = c(
        "PASS",
        "REVIEW"
      ),
      breaks = c(
        "PASS",
        "REVIEW"
      ),
      drop = FALSE,
      name = NULL
    ) +

    ggplot2::guides(

      fill = ggplot2::guide_legend(
        order = 1,
        nrow = 1
      ),

      colour = ggplot2::guide_legend(
        order = 2,
        nrow = 1
      )
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

      legend.position = "top",

      legend.direction = "horizontal",

      legend.box = "horizontal",

      legend.background = ggplot2::element_rect(
        fill = bg,
        colour = NA
      ),

      legend.key = ggplot2::element_rect(
        fill = bg,
        colour = NA
      ),

      legend.text = ggplot2::element_text(
        colour = fg,
        size = 9
      ),

      legend.title = ggplot2::element_text(
        colour = fg,
        size = 9
      )
    )

  legend_gtable <- ggplot2::ggplotGrob(
    legend_plot
  )

  legend_index <- which(
    grepl(
      "^guide-box",
      legend_gtable$layout$name
    )
  )

  legend_candidates <- legend_gtable$grobs[
    legend_index
  ]

  legend_nonempty <- which(
    !vapply(
      legend_candidates,
      inherits,
      logical(1),
      what = "zeroGrob"
    )
  )

  if (length(legend_nonempty) > 0) {

    legend_grob <- legend_candidates[
      legend_nonempty[1]
    ][[1]]

  } else {

    legend_grob <- grid::nullGrob()
  }


  # ------------------------------------------------------------
  # Helper: create one site/day panel
  # ------------------------------------------------------------

  make_panel_grob <- function(panel_data) {

    p <- ggplot2::ggplot(
      panel_data,
      ggplot2::aes(
        x = ts,
        y = sig
      )
    ) +

      ggplot2::geom_point(
        ggplot2::aes(
          shape = antBin,
          fill = rlClass,
          colour = qa_status
        ),
        size = 2.6,
        stroke = 1.1
      ) +

      ggplot2::facet_wrap(
        ~ panel_label,
        scales = "free"
      ) +

      ggplot2::scale_shape_manual(
        values = c(
          "N" = 24,
          "E" = 22,
          "S" = 25,
          "W" = 23,
          "Omni / missing" = 21
        ),
        guide = "none"
      ) +

      ggplot2::scale_fill_manual(
        values = c(
          run_length_colors,
          "Unknown" = unknown_run_color
        ),
        limits = c(
          run_length_levels,
          "Unknown"
        ),
        guide = "none"
      ) +

      ggplot2::scale_colour_manual(
        values = qa_colors,
        guide = "none"
      ) +

      ggplot2::scale_x_datetime(
        date_labels = "%H:%M:%S"
      ) +

      ggplot2::labs(
        x = NULL,
        y = NULL
      ) +

      ggplot2::theme_minimal() +

      ggplot2::theme(

        strip.background = ggplot2::element_rect(
          fill = strip_fill,
          colour = strip_border
        ),

        strip.text = ggplot2::element_text(
          colour = strip_text,
          size = 9
        ),

        panel.background = ggplot2::element_rect(
          fill = panel_bg,
          colour = strip_border
        ),

        plot.background = ggplot2::element_rect(
          fill = bg,
          colour = NA
        ),

        axis.text = ggplot2::element_text(
          colour = fg,
          size = 8
        ),

        axis.title = ggplot2::element_blank(),

        panel.grid.major = ggplot2::element_line(
          colour = grid_major,
          linewidth = 0.4
        ),

        panel.grid.minor = ggplot2::element_blank(),

        plot.margin = ggplot2::margin(
          2,
          3,
          2,
          3
        )
      )

    ggplot2::ggplotGrob(p)
  }


  # ------------------------------------------------------------
  # Create profile PDF
  # ------------------------------------------------------------

  grDevices::pdf(
    profile_file,
    width = 10,
    height = 13,
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


  # ------------------------------------------------------------
  # One animal at a time
  # ------------------------------------------------------------

  for (this_animal in animal_order) {

    animal_data <- plot_data |>
      dplyr::filter(
        animal_id == this_animal
      )


    # ----------------------------------------------------------
    # Header information
    # ----------------------------------------------------------

    species_name <- animal_data$speciesEN[
      which(
        !is.na(
          animal_data$speciesEN
        )
      )[1]
    ]

    if (
      length(species_name) == 0 ||
      is.na(species_name)
    ) {
      species_name <- "Unknown species"
    }

    mfg_id <- animal_data$mfgID[
      which(
        !is.na(
          animal_data$mfgID
        )
      )[1]
    ]

    motus_id <- animal_data$motusTagID[
      which(
        !is.na(
          animal_data$motusTagID
        )
      )[1]
    ]

    deploy_date <- animal_data$tagDeployStart[
      which(
        !is.na(
          animal_data$tagDeployStart
        )
      )[1]
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
      ifelse(
        length(motus_id) == 0 ||
          is.na(motus_id),
        "?",
        motus_id
      ),
      ")"
    )

    animal_subtitle <- if (
      length(deploy_date) > 0 &&
      !is.na(deploy_date)
    ) {

      paste(
        "Deployed:",
        format(
          deploy_date,
          "%d %b %Y",
          tz = "UTC"
        )
      )

    } else {

      "Deployment date unavailable"
    }


    # ----------------------------------------------------------
    # Chronological panel order
    # ----------------------------------------------------------

    panel_order <- animal_data |>
      dplyr::group_by(
        panel_label
      ) |>
      dplyr::summarise(
        first_detection = min(
          ts,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::arrange(
        first_detection
      ) |>
      dplyr::pull(
        panel_label
      )

    panel_pages <- split(
      panel_order,
      ceiling(
        seq_along(
          panel_order
        ) /
          panels_per_page
      )
    )


    # ----------------------------------------------------------
    # Draw pages
    # ----------------------------------------------------------

    for (page_panels in panel_pages) {

      grid::grid.newpage()

      grid::grid.rect(
        gp = grid::gpar(
          fill = bg,
          col = bg
        )
      )

      page_layout <- grid::grid.layout(
        nrow = 6,
        ncol = 2,

        heights = grid::unit.c(
          grid::unit(
            0.38,
            "in"
          ),
          grid::unit(
            0.34,
            "in"
          ),
          grid::unit(
            rep(
              1,
              4
            ),
            "null"
          )
        ),

        widths = grid::unit(
          c(
            1,
            1
          ),
          "null"
        )
      )

      grid::pushViewport(
        grid::viewport(
          layout = page_layout
        )
      )


      # --------------------------------------------------------
      # Page title
      # --------------------------------------------------------

      grid::grid.text(
        animal_title,
        vp = grid::viewport(
          layout.pos.row = 1,
          layout.pos.col = 1:2
        ),
        y = grid::unit(
          0.67,
          "npc"
        ),
        gp = grid::gpar(
          col = fg,
          fontsize = 16
        )
      )

      grid::grid.text(
        animal_subtitle,
        vp = grid::viewport(
          layout.pos.row = 1,
          layout.pos.col = 1:2
        ),
        y = grid::unit(
          0.18,
          "npc"
        ),
        gp = grid::gpar(
          col = fg,
          fontsize = 10
        )
      )


      # --------------------------------------------------------
      # Shared legend
      # --------------------------------------------------------

      grid::grid.draw(
        grid::editGrob(
          legend_grob,
          vp = grid::viewport(
            layout.pos.row = 2,
            layout.pos.col = 1:2
          )
        )
      )


      # --------------------------------------------------------
      # Plot up to eight actual panels
      # --------------------------------------------------------

      for (i in seq_along(page_panels)) {

        panel_name <- page_panels[i]

        panel_data <- animal_data |>
          dplyr::filter(
            panel_label == panel_name
          )

        panel_grob <- make_panel_grob(
          panel_data
        )

        plot_row <- 3 +
          ((i - 1) %/% 2)

        plot_col <- 1 +
          ((i - 1) %% 2)

        grid::grid.draw(
          grid::editGrob(
            panel_grob,
            vp = grid::viewport(
              layout.pos.row = plot_row,
              layout.pos.col = plot_col
            )
          )
        )
      }


      # --------------------------------------------------------
      # Shared y-axis label
      # --------------------------------------------------------

      grid::grid.text(
        "Signal strength",
        x = grid::unit(
          0.012,
          "npc"
        ),
        y = grid::unit(
          0.46,
          "npc"
        ),
        rot = 90,
        gp = grid::gpar(
          col = fg,
          fontsize = 11
        )
      )

      grid::popViewport()
    }
  }


  # ------------------------------------------------------------
  # Close profile PDF
  # ------------------------------------------------------------

  grDevices::dev.off()
  device_open <- FALSE


  # ------------------------------------------------------------
  # Return output paths
  # ------------------------------------------------------------

  files <- list(
    profiles = profile_file,
    maps = map_file,
    review = review_file,
    summary = summary_file
  )

  message(
    "Validation package created:\n",
    paste0(
      "  ",
      unlist(files),
      collapse = "\n"
    )
  )

  invisible(files)
}
