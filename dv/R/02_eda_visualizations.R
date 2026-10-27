# Summarize ridership and create the five project visualizations.

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
invisible(set_project_root())
check_required_packages()
ensure_output_directories()

# Remove only extra chart files produced by earlier versions of this project.
previous_extra_plots <- c(
  "09_seasonal_demand.png",
  "10_regression_residuals.png", "11_regression_qq.png"
)
file.remove(file.path("outputs", "plots", previous_extra_plots[
  file.exists(file.path("outputs", "plots", previous_extra_plots))
]))

data <- read_clean_data()
daily_station <- data |>
  dplyr::group_by(
    .data$date, .data$station_id, .data$station, .data$borough,
    .data$weekday, .data$day_type, .data$holiday
  ) |>
  dplyr::summarise(
    passengers = sum(.data$passengers, na.rm = TRUE),
    transfers = sum(.data$transfers, na.rm = TRUE),
    observed_hours = dplyr::n(),
    .groups = "drop"
  )

complete_daily_station <- daily_station |>
  dplyr::filter(.data$observed_hours == 24L)

station_summary <- daily_station |>
  dplyr::group_by(.data$station_id, .data$station, .data$borough) |>
  dplyr::summarise(
    total_passengers = sum(.data$passengers),
    mean_daily_passengers = mean(.data$passengers),
    median_daily_passengers = median(.data$passengers),
    observed_station_hours = sum(.data$observed_hours),
    mean_passengers_per_observed_hour =
      sum(.data$passengers) / sum(.data$observed_hours),
    observed_days = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(.data$total_passengers))

daily_summary <- daily_station |>
  dplyr::group_by(.data$date, .data$weekday, .data$day_type, .data$holiday) |>
  dplyr::summarise(
    total_passengers = sum(.data$passengers),
    total_transfers = sum(.data$transfers),
    observed_station_hours = sum(.data$observed_hours),
    mean_passengers_per_observed_hour =
      sum(.data$passengers) / sum(.data$observed_hours),
    .groups = "drop"
  )

hourly_summary <- data |>
  dplyr::group_by(.data$hour) |>
  dplyr::summarise(
    total_passengers = sum(.data$passengers, na.rm = TRUE),
    observed_station_hours = dplyr::n(),
    mean_passengers_per_observed_hour =
      sum(.data$passengers, na.rm = TRUE) / dplyr::n(),
    .groups = "drop"
  )

monthly_summary <- data |>
  dplyr::group_by(.data$month, .data$month_name) |>
  dplyr::summarise(
    total_passengers = sum(.data$passengers, na.rm = TRUE),
    observed_station_hours = dplyr::n(),
    mean_passengers_per_station_hour = mean(.data$passengers, na.rm = TRUE),
    .groups = "drop"
  )

seasonal_summary <- data |>
  dplyr::group_by(.data$season) |>
  dplyr::summarise(
    total_passengers = sum(.data$passengers, na.rm = TRUE),
    observed_station_hours = dplyr::n(),
    mean_passengers_per_station_hour = mean(.data$passengers, na.rm = TRUE),
    .groups = "drop"
  )

weekday_hour_summary <- data |>
  dplyr::group_by(.data$weekday, .data$hour) |>
  dplyr::summarise(
    mean_passengers = mean(.data$passengers, na.rm = TRUE),
    .groups = "drop"
  )

holiday_summary <- daily_station |>
  dplyr::filter(.data$observed_hours == 24L) |>
  dplyr::group_by(.data$holiday) |>
  dplyr::summarise(
    mean_daily_station_passengers = mean(.data$passengers, na.rm = TRUE),
    median_daily_station_passengers = median(.data$passengers, na.rm = TRUE),
    observed_station_days = dplyr::n(),
    .groups = "drop"
  )

readr::write_csv(station_summary, "outputs/tables/station_summary.csv")
readr::write_csv(daily_summary, "outputs/tables/daily_summary.csv")
readr::write_csv(hourly_summary, "outputs/tables/hourly_summary.csv")
readr::write_csv(monthly_summary, "outputs/tables/monthly_summary.csv")
readr::write_csv(seasonal_summary, "outputs/tables/seasonal_summary.csv")
readr::write_csv(holiday_summary, "outputs/tables/holiday_summary.csv")

save_plot <- function(plot, filename, width = 10, height = 6) {
  ggplot2::ggsave(
    filename = file.path("outputs", "plots", filename),
    plot = plot, width = width, height = height, dpi = 300
  )
}

ggplot2::theme_set(ggplot2::theme_minimal(base_size = 12))

plot_station <- ggplot2::ggplot(
  station_summary,
  ggplot2::aes(
    x = stats::reorder(.data$station, .data$total_passengers),
    y = .data$total_passengers,
    fill = .data$borough
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Annual ridership by selected station",
    subtitle = "Ten station complexes selected from high and low Jan. 2, 2024 totals",
    x = NULL, y = "Total recorded passengers", fill = "Borough"
  )
save_plot(plot_station, "01_station_demand.png", height = 7)

plot_daily_trend <- ggplot2::ggplot(
  daily_summary,
  ggplot2::aes(x = .data$date, y = .data$mean_passengers_per_observed_hour)
) +
  ggplot2::geom_line(color = "#176B87", alpha = 0.55, linewidth = 0.35) +
  ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#D95F02") +
  ggplot2::labs(
    title = "Daily passenger-demand trend",
    subtitle = "Mean across reported station-hours; coverage varies by date",
    x = "Date", y = "Passengers per reported station-hour"
  )
save_plot(plot_daily_trend, "02_daily_demand_trend.png")

plot_hourly <- ggplot2::ggplot(
  hourly_summary,
  ggplot2::aes(x = .data$hour, y = .data$mean_passengers_per_observed_hour)
) +
  ggplot2::geom_col(fill = "#176B87") +
  ggplot2::scale_x_continuous(breaks = 0:23) +
  ggplot2::labs(
    title = "Passenger demand by hour of day",
    x = "Hour (24-hour clock)",
    y = "Mean passengers per reported station-hour"
  )
save_plot(plot_hourly, "03_hourly_demand.png")

plot_heatmap <- ggplot2::ggplot(
  weekday_hour_summary,
  ggplot2::aes(x = .data$hour, y = .data$weekday, fill = .data$mean_passengers)
) +
  ggplot2::geom_tile() +
  ggplot2::scale_x_continuous(breaks = 0:23) +
  ggplot2::scale_fill_viridis_c(name = "Mean passengers") +
  ggplot2::labs(
    title = "Mean passenger demand by weekday and hour",
    x = "Hour (24-hour clock)", y = "Day of week"
  )
save_plot(plot_heatmap, "04_weekday_hour_heatmap.png")

plot_weekday_weekend <- ggplot2::ggplot(
  complete_daily_station,
  ggplot2::aes(x = .data$day_type, y = .data$passengers, fill = .data$day_type)
) +
  ggplot2::geom_boxplot(outlier.alpha = 0.15) +
  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  ggplot2::scale_fill_manual(values = c("Weekday" = "#176B87", "Weekend" = "#E6A23C")) +
  ggplot2::labs(
    title = "Daily station ridership: weekdays and weekends",
    subtitle = "Only station-days with all 24 hourly records are included",
    x = NULL, y = "Passengers per station-day", fill = "Day type"
  ) +
  ggplot2::theme(legend.position = "none")
save_plot(plot_weekday_weekend, "05_weekday_weekend_boxplot.png")

plot_monthly <- ggplot2::ggplot(
  monthly_summary,
  ggplot2::aes(
    x = .data$month_name,
    y = .data$mean_passengers_per_station_hour,
    group = 1
  )
) +
  ggplot2::geom_line(color = "#176B87", linewidth = 1) +
  ggplot2::geom_point(color = "#D95F02", size = 2.5) +
  ggplot2::labs(
    title = "Average passenger demand by month",
    subtitle = "Mean passengers per reported station-hour",
    x = "Month", y = "Mean passengers per reported station-hour"
  )
save_plot(plot_monthly, "06_monthly_demand.png")

plot_holiday <- ggplot2::ggplot(
  complete_daily_station,
  ggplot2::aes(x = .data$holiday, y = .data$passengers, fill = .data$holiday)
) +
  ggplot2::geom_boxplot(outlier.alpha = 0.2) +
  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  ggplot2::scale_fill_manual(
    values = c("Non-holiday" = "#176B87", "Holiday" = "#D95F02")
  ) +
  ggplot2::labs(
    title = "Station ridership on holidays and other days",
    subtitle = "Complete 24-hour station-days only; holidays use the 2024 U.S. federal calendar",
    x = NULL, y = "Passengers per complete station-day"
  ) +
  ggplot2::theme(legend.position = "none")
save_plot(plot_holiday, "07_holiday_comparison.png", height = 7)

plot_relationship <- ggplot2::ggplot(
  data,
  ggplot2::aes(x = .data$transfers, y = .data$passengers)
) +
  ggplot2::geom_point(alpha = 0.16, size = 0.7, color = "#176B87") +
  ggplot2::geom_smooth(
    method = "lm", se = FALSE, color = "#D95F02", linewidth = 0.8
  ) +
  ggplot2::facet_wrap(ggplot2::vars(.data$station), scales = "free") +
  ggplot2::scale_x_continuous(labels = scales::label_comma()) +
  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  ggplot2::labs(
    title = "Hourly passengers and transfers by station",
    subtitle = "Facets use station-specific scales; association is not causation",
    x = "Recorded transfers per station-hour",
    y = "Recorded passengers per station-hour"
  )
save_plot(plot_relationship, "08_passengers_transfers_relationship.png", height = 8)

message("EDA complete: summary tables and eight charts saved under outputs/.")
