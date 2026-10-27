# Download (when needed), validate, clean, and save the selected-station data.

if (!exists("set_project_root", mode = "function")) {
  script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  helper_path <- if (length(script_argument) > 0) {
    file.path(
      dirname(normalizePath(
        sub("^--file=", "", script_argument[[1]]),
        winslash = "/", mustWork = TRUE
      )),
      "helpers.R"
    )
  } else {
    file.path("R", "helpers.R")
  }
  source(helper_path, local = FALSE)
}
project_root <- set_project_root()
check_required_packages()
ensure_output_directories()

source_url <- "https://data.ny.gov/resource/wujg-7c2s.csv"
raw_path <- file.path(
  "data", "raw", "mta_subway_ridership_2024_selected_stations.csv"
)
clean_path <- file.path(
  "data", "processed", "mta_subway_ridership_2024_clean.csv"
)

# These 10 station complexes span the high- and low-ridership ends of the
# Jan. 2, 2024 snapshot. The same sample is followed through the full year.
station_ids <- c("611", "610", "607", "602", "164", "202", "199", "201", "200", "203")
station_filter <- paste0(
  "station_complex_id IN (",
  paste(sprintf("'%s'", station_ids), collapse = ","),
  ")"
)

download_source_data <- function(destination) {
  message("Downloading the official MTA extract. This may take a few minutes...")

  page_size <- 50000L
  all_pages <- list()
  options(timeout = max(300, getOption("timeout")))
  month_starts <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
  for (month_start in month_starts) {
    month_start <- as.Date(month_start, origin = "1970-01-01")
    month_end <- min(
      as.Date(format(month_start + 31, "%Y-%m-01")),
      as.Date("2025-01-01")
    )
    start_text <- paste0(month_start, "T00:00:00")
    end_text <- paste0(month_end, "T00:00:00")
    offset <- 0L

    repeat {
      query <- paste0(
        "$select=transit_timestamp,station_complex_id,station_complex,borough,",
        "payment_method,fare_class_category,ridership,transfers",
        "&$where=transit_timestamp%20%3E%3D%20'", start_text,
        "'%20AND%20transit_timestamp%20%3C%20'", end_text,
        "'%20AND%20", URLencode(station_filter, reserved = FALSE),
        "&$order=transit_timestamp,station_complex_id,payment_method,fare_class_category",
        "&$limit=", page_size, "&$offset=", offset
      )
      request_url <- paste0(source_url, "?", query)
      temporary_file <- tempfile(fileext = ".csv")
      on.exit(unlink(temporary_file), add = TRUE)

      download_result <- tryCatch(
        utils::download.file(
          request_url, temporary_file, mode = "wb", quiet = TRUE, method = "libcurl"
        ),
        error = function(error) {
          stop(
            "Could not download the MTA public-data extract from ", source_url,
            ". Check your internet connection and try again. Details: ",
            conditionMessage(error),
            call. = FALSE
          )
        }
      )
      if (!identical(download_result, 0L) || !file.exists(temporary_file)) {
        stop(
          "The MTA data download failed. The source is ", source_url,
          ". Check your internet connection and try again.",
          call. = FALSE
        )
      }

      page <- readr::read_csv(
        temporary_file, show_col_types = FALSE, progress = FALSE
      )
      if (nrow(page) == 0L) {
        break
      }
      all_pages[[length(all_pages) + 1L]] <- page
      message(
        "Downloaded ", nrow(page), " source rows for ",
        format(month_start, "%B %Y"), " (offset ", offset, ")."
      )
      if (nrow(page) < page_size) { 
        break
      }
      offset <- offset + page_size
    }
  }

  if (length(all_pages) == 0L) {
    stop(
      "The MTA source returned no rows for 2024 and the selected stations.",
      call. = FALSE
    )
  }

  raw_data <- dplyr::bind_rows(all_pages)
  readr::write_csv(raw_data, destination, na = "")
  invisible(raw_data)
}

if (!file.exists(raw_path)) {
  raw_data <- download_source_data(raw_path)
} else {
  raw_data <- readr::read_csv(raw_path, show_col_types = FALSE, progress = FALSE)
}

raw_data <- janitor::clean_names(raw_data)
required_columns <- c(
  "transit_timestamp", "station_complex_id", "station_complex",
  "borough", "payment_method", "fare_class_category", "ridership", "transfers"
)
missing_columns <- setdiff(required_columns, names(raw_data))
if (length(missing_columns) > 0) {
  stop(
    "The downloaded/source data is missing expected column(s): ",
    paste(missing_columns, collapse = ", "),
    ". See the dataset source and schema in README.md.",
    call. = FALSE
  )
}

cleaned <- raw_data |>
  dplyr::transmute(
    station_id = as.character(.data$station_complex_id),
    station = trimws(as.character(.data$station_complex)),
    borough = trimws(as.character(.data$borough)),
    payment_method = trimws(as.character(.data$payment_method)),
    fare_class_category = trimws(as.character(.data$fare_class_category)),
    passengers = clean_numeric_count(.data$ridership),
    transfers = clean_numeric_count(.data$transfers),
    timestamp = if (inherits(.data$transit_timestamp, "POSIXt")) {
      as.POSIXct(.data$transit_timestamp, tz = "UTC")
    } else {
      lubridate::ymd_hms(
        as.character(.data$transit_timestamp), tz = "UTC", quiet = TRUE
      )
    }
  ) |>
  dplyr::mutate(
    date = as.Date(.data$timestamp),
    hour = lubridate::hour(.data$timestamp),
    month = lubridate::month(.data$timestamp),
    month_name = factor(
      month.abb[.data$month],
      levels = month.abb,
      ordered = TRUE
    ),
    season = factor(
      dplyr::case_when(
        .data$month %in% c(12, 1, 2) ~ "Winter",
        .data$month %in% 3:5 ~ "Spring",
        .data$month %in% 6:8 ~ "Summer",
        TRUE ~ "Autumn"
      ),
      levels = c("Winter", "Spring", "Summer", "Autumn"),
      ordered = TRUE
    ),
    weekday = factor(
      weekdays(.data$date),
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                 "Friday", "Saturday", "Sunday"),
      ordered = TRUE
    ),
    day_type = ifelse(.data$weekday %in% c("Saturday", "Sunday"),
                      "Weekend", "Weekday"),
    day_type = factor(.data$day_type, levels = c("Weekday", "Weekend"))
  ) |>
  dplyr::distinct(
    .data$timestamp, .data$station_id, .data$payment_method,
    .data$fare_class_category,
    .keep_all = TRUE
  ) |>
  dplyr::filter(
    !is.na(.data$timestamp),
    .data$date >= as.Date("2024-01-01"),
    .data$date < as.Date("2025-01-01"),
    !is.na(.data$station_id),
    !is.na(.data$station),
    !is.na(.data$passengers),
    !is.na(.data$transfers),
    .data$passengers >= 0,
    .data$transfers >= 0
  ) |>
  dplyr::group_by(
    .data$timestamp, .data$station_id, .data$station, .data$borough,
    .data$date, .data$hour, .data$month, .data$month_name, .data$season,
    .data$weekday, .data$day_type
  ) |>
  dplyr::summarise(
    passengers = sum(.data$passengers),
    transfers = sum(.data$transfers),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    # Flag extreme records for review; preserve these valid nonnegative counts.
    passenger_outlier = .data$passengers >
      stats::quantile(.data$passengers, 0.99, na.rm = TRUE) +
      3 * stats::IQR(.data$passengers, na.rm = TRUE),
    holiday = .data$date %in% as.Date(c(
      "2024-01-01", "2024-01-15", "2024-02-19", "2024-05-27",
      "2024-06-19", "2024-07-04", "2024-09-02", "2024-10-14",
      "2024-11-11", "2024-11-28", "2024-12-25"
    )),
    holiday = factor(
      ifelse(.data$holiday, "Holiday", "Non-holiday"),
      levels = c("Non-holiday", "Holiday")
    )
  ) |>
  dplyr::select(
    timestamp, date, hour, month, month_name, season, weekday, day_type, holiday,
    station_id, station, borough, passengers, transfers, passenger_outlier
  ) |>
  dplyr::arrange(.data$timestamp, .data$station_id)

if (nrow(cleaned) == 0L) {
  stop("No valid records remain after cleaning the MTA extract.", call. = FALSE)
}

readr::write_csv(cleaned, clean_path, na = "")

expected_station_hours <- length(station_ids) * 24L * 366L
station_coverage <- cleaned |>
  dplyr::group_by(.data$station_id, .data$station, .data$borough) |>
  dplyr::summarise(
    observed_station_hours = dplyr::n(),
    expected_station_hours = 24L * 366L,
    station_hours_not_reported = .data$expected_station_hours -
      .data$observed_station_hours,
    coverage_percent = 100 * .data$observed_station_hours /
      .data$expected_station_hours,
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$station_id)
readr::write_csv(
  station_coverage,
  file.path("outputs", "tables", "station_coverage_summary.csv")
)

cleaning_summary <- tibble::tibble(
  measure = c(
    "Source fare-category rows", "Cleaned station-hour rows",
    "Raw rows consolidated or excluded",
    "Expected station-hour combinations in leap year",
    "Expected station-hours absent after cleaning (not imputed)",
    "Passenger values imputed", "Transfer values imputed",
    "Outlier records flagged (not removed)"
  ),
  value = c(
    nrow(raw_data),
    nrow(cleaned),
    nrow(raw_data) - nrow(cleaned),
    expected_station_hours,
    expected_station_hours - nrow(cleaned),
    0L,
    0L,
    sum(cleaned$passenger_outlier, na.rm = TRUE)
  )
)
readr::write_csv(
  cleaning_summary,
  file.path("outputs", "tables", "cleaning_summary.csv")
)

message(
  "Cleaning complete: ", nrow(cleaned), " records saved to ", clean_path
)
