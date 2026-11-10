make_detection_map_fixture <- function() {
  ts <- as.POSIXct(
    c(
      "2024-02-29 23:30:00",
      "2024-02-29 23:45:00",
      "2024-02-29 23:55:00",
      "2024-03-01 01:00:00",
      "2024-03-01 00:30:00",
      "2024-06-01 12:00:00"
    ),
    tz = "UTC"
  )

  detections <- tibble::tibble(
    source_database = c(rep("db_a", 5), "db_b"),
    motusTagID = c(rep(101L, 5), 101L),
    tagDeployID = c(rep(1001L, 5), 1001L),
    mfgID = c(rep("M101", 5), "M101-B"),
    speciesEN = rep("Example bird", 6),
    runID = c(1L, 1L, 2L, 3L, 4L, 1L),
    hitID = seq_len(6),
    ts = ts,
    qa_status = c("PASS", "REVIEW", "PASS", "PASS", "PASS", "PASS"),
    recvDeployID = c(10L, 10L, 10L, 10L, 20L, 30L),
    recvDeployName = c(
      "Deploy A", "Deploy A", "Deploy A", "Deploy A", NA, ""
    ),
    recvSiteName = c(
      "Legacy A", "Legacy A", "Legacy A", "Legacy A", "Legacy B", ""
    ),
    recv = c(rep("Receiver A", 4), "Receiver B", "Receiver C"),
    recvDeployLat = c(rep(45, 4), 46, 47),
    recvDeployLon = c(rep(-75, 4), -74, -73),
    tagDepLat = c(NA, rep(44, 4), NA),
    tagDepLon = c(NA, rep(-76, 4), NA),
    tagDeployStart = as.POSIXct(
      rep("2024-02-01 00:00:00", 6),
      tz = "UTC"
    )
  )

  out <- list(
    detections = detections,
    rejected = detections[0, ],
    excluded = detections[0, ],
    decisions = tibble::tibble(),
    summary = list(),
    settings = list()
  )
  class(out) <- c("motus_validated", "list")
  out
}


test_that("create_detection_maps validates its input contract", {
  path <- tempfile(fileext = ".pdf")

  expect_error(
    create_detection_maps(list(), path),
    "motus_validated"
  )

  x <- make_detection_map_fixture()
  empty <- x
  empty$detections <- empty$detections[0, ]
  expect_error(
    create_detection_maps(empty, path),
    "no vetted detections"
  )

  missing <- x
  missing$detections$recvDeployLon <- NULL
  expect_error(
    create_detection_maps(missing, path),
    "missing required field"
  )

  bad_coords <- x
  bad_coords$detections$recvDeployLat[1] <- NA_real_
  expect_error(
    create_detection_maps(bad_coords, path),
    "missing or nonfinite receiver coordinates"
  )

  expect_error(
    create_detection_maps(x, path, color_by = "month"),
    "arg"
  )
})


test_that("station-day preparation collapses hits and retains separate dates", {
  prepared <- .prepare_detection_map_data(
    make_detection_map_fixture()$detections
  )
  events <- prepared$events

  same_day <- events |>
    dplyr::filter(
      source_database == "db_a",
      recvDeployID == 10L,
      date == as.Date("2024-02-29")
    )

  expect_equal(nrow(same_day), 1L)
  expect_equal(same_day$n_runs, 2L)
  expect_equal(same_day$n_hits, 3L)
  expect_equal(
    same_day$first_detection,
    as.POSIXct("2024-02-29 23:30:00", tz = "UTC")
  )
  expect_equal(
    same_day$last_detection,
    as.POSIXct("2024-02-29 23:55:00", tz = "UTC")
  )

  repeated_station <- events |>
    dplyr::filter(
      source_database == "db_a",
      recvDeployID == 10L
    ) |>
    dplyr::arrange(date)

  expect_equal(nrow(repeated_station), 2L)
  expect_equal(
    repeated_station$date,
    as.Date(c("2024-02-29", "2024-03-01"))
  )
  expect_equal(repeated_station$station_name, c("Deploy A", "Deploy A"))
  expect_equal(sum(events$n_hits), nrow(make_detection_map_fixture()$detections))
})


test_that("station-day dates use UTC and seasons use fixed boundaries", {
  x <- make_detection_map_fixture()
  one <- x$detections[1, ]
  one$ts <- as.POSIXct(
    "2024-01-01 20:30:00",
    tz = "America/New_York"
  )

  event <- .prepare_detection_map_data(one)$events
  expect_equal(event$date, as.Date("2024-01-02"))
  expect_equal(event$year, 2024L)
  expect_equal(as.character(event$season), "Winter")

  boundary_dates <- as.Date(
    c(
      "2024-02-29", "2024-03-01",
      "2024-05-31", "2024-06-01",
      "2024-08-31", "2024-09-01",
      "2024-11-30", "2024-12-01"
    )
  )
  expect_equal(
    as.character(.detection_map_season(boundary_dates)),
    c(
      "Winter", "Spring",
      "Spring", "Summer",
      "Summer", "Fall",
      "Fall", "Winter"
    )
  )
})


test_that("events are ordered chronologically within composite individuals", {
  events <- .prepare_detection_map_data(
    make_detection_map_fixture()$detections
  )$events

  expect_equal(dplyr::n_distinct(events$animal_id), 2L)
  expect_true(all(
    c("db_a::101::1001", "db_b::101::1001") %in% events$animal_id
  ))

  db_a <- events |>
    dplyr::filter(source_database == "db_a") |>
    dplyr::arrange(event_number)

  expect_equal(db_a$event_number, 1:3)
  expect_equal(db_a$recvDeployID, c(10L, 20L, 10L))
  expect_true(all(diff(as.numeric(db_a$first_detection)) > 0))
  expect_equal(as.character(db_a$season), c("Winter", "Spring", "Spring"))
})


test_that("deployment selection uses the first complete coordinate pair", {
  prepared <- .prepare_detection_map_data(
    make_detection_map_fixture()$detections
  )

  expect_equal(nrow(prepared$deployments), 1L)
  expect_equal(prepared$deployments$animal_id, "db_a::101::1001")
  expect_equal(prepared$deployments$lat, 44)
  expect_equal(prepared$deployments$lon, -76)
  expect_equal(
    prepared$deployments$deployment_date,
    as.Date("2024-02-01")
  )

  no_deployment <- make_detection_map_fixture()$detections
  no_deployment$tagDepLat <- NA_real_
  no_deployment$tagDepLon <- NA_real_
  expect_equal(
    nrow(.prepare_detection_map_data(no_deployment)$deployments),
    0L
  )
})


test_that("station labels summarize station-day dates and counts", {
  events <- .prepare_detection_map_data(
    make_detection_map_fixture()$detections
  )$events

  both <- .build_detection_map_labels(
    events,
    label_stations = TRUE,
    label_dates = TRUE
  )
  station_a <- both |>
    dplyr::filter(
      animal_id == "db_a::101::1001",
      recvDeployID == 10L
    )

  expect_equal(
    station_a$label,
    paste(
      "Deploy A",
      "2024-02-29 \u2013 2024-03-01",
      "2 station-days",
      sep = "\n"
    )
  )

  one_day <- both |>
    dplyr::filter(animal_id == "db_b::101::1001")
  expect_equal(
    one_day$label,
    paste(
      "Receiver 30",
      "2024-06-01",
      "1 station-day",
      sep = "\n"
    )
  )

  station_only <- .build_detection_map_labels(
    events,
    label_stations = TRUE,
    label_dates = FALSE
  )
  expect_equal(
    station_only$label[
      station_only$animal_id == "db_a::101::1001" &
        station_only$recvDeployID == 10L
    ],
    "Deploy A"
  )

  dates_only <- .build_detection_map_labels(
    events,
    label_stations = FALSE,
    label_dates = TRUE
  )
  dates_label <- dates_only$label[
    dates_only$animal_id == "db_a::101::1001" &
      dates_only$recvDeployID == 10L
  ]
  expect_equal(
    dates_label,
    paste(
      "2024-02-29 \u2013 2024-03-01",
      "2 station-days",
      sep = "\n"
    )
  )

  neither <- .build_detection_map_labels(
    events,
    label_stations = FALSE,
    label_dates = FALSE
  )
  expect_equal(nrow(neither), 0L)

  expect_equal(nrow(events), 4L)
  expect_equal(nrow(.build_detection_map_segments(events)), 2L)
})


test_that("segments connect successive events and handle single events", {
  events <- .prepare_detection_map_data(
    make_detection_map_fixture()$detections
  )$events
  segments <- .build_detection_map_segments(events)

  db_a <- segments |>
    dplyr::filter(animal_id == "db_a::101::1001")

  expect_equal(nrow(db_a), 2L)
  expect_equal(db_a$from_event_number, c(1L, 2L))
  expect_equal(db_a$to_event_number, c(2L, 3L))
  expect_equal(
    nrow(
      segments |>
        dplyr::filter(animal_id == "db_b::101::1001")
    ),
    0L
  )

  expect_null(.detection_map_arrow(FALSE))
  expect_s3_class(.detection_map_arrow(TRUE), "arrow")
})


test_that("map extent retains established minimum context and padding", {
  extent <- .detection_map_extent(-75, 45)

  expect_equal(extent$lon_span, 4)
  expect_equal(extent$lat_span, 4)
  expect_equal(diff(extent$xlim), 5.5)
  expect_equal(diff(extent$ylim), 5.5)
})


test_that("create_detection_maps writes a nonempty PDF", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggrepel")
  skip_if_not_installed("rnaturalearth")
  skip_if_not_installed("rnaturalearthdata")
  skip_if_not_installed("sf")

  path <- tempfile(fileext = ".pdf")
  on.exit(unlink(path), add = TRUE)

  result <- create_detection_maps(
    make_detection_map_fixture(),
    file = path,
    show_arrows = FALSE
  )

  expect_identical(result, path)
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
})
