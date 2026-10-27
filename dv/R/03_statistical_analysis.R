# Run descriptive summaries, a paired weekday/weekend test, and a regression.

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

data <- read_clean_data()
daily_station <- data |>
  dplyr::group_by(
    .data$date, .data$station_id, .data$station, .data$weekday,
    .data$day_type, .data$holiday
  ) |>
  dplyr::summarise(
    passengers = sum(.data$passengers),
    observed_hours = dplyr::n(),
    .groups = "drop"
  )

descriptive_statistics <- data |>
  dplyr::summarise(
    records = dplyr::n(),
    stations = dplyr::n_distinct(.data$station_id),
    mean_passengers_per_station_hour = mean(.data$passengers),
    median_passengers_per_station_hour = median(.data$passengers),
    sd_passengers_per_station_hour = stats::sd(.data$passengers),
    minimum_passengers = min(.data$passengers),
    maximum_passengers = max(.data$passengers),
    mean_transfers_per_station_hour = mean(.data$transfers),
    passenger_transfer_correlation = stats::cor(
      .data$passengers, .data$transfers, use = "complete.obs"
    )
  )
readr::write_csv(
  descriptive_statistics,
  "outputs/tables/descriptive_statistics.csv"
)

# Pair each station's weekday average with its weekend average so large
# stations do not dominate solely because of their scale.
station_day_type <- daily_station |>
  dplyr::filter(.data$observed_hours == 24L) |>
  dplyr::group_by(.data$station_id, .data$station, .data$day_type) |>
  dplyr::summarise(
    mean_daily_passengers = mean(.data$passengers),
    .groups = "drop"
  )
paired_values <- station_day_type |>
  tidyr::pivot_wider(
    names_from = day_type,
    values_from = mean_daily_passengers
  ) |>
  tidyr::drop_na(Weekday, Weekend) |>
  dplyr::mutate(difference = .data$Weekday - .data$Weekend)

if (nrow(paired_values) < 3L) {
  stop(
    "Not enough stations with weekday and weekend observations for comparison.",
    call. = FALSE
  )
}

normality_test <- stats::shapiro.test(paired_values$difference)
weekday_weekend_test <- stats::wilcox.test(
  paired_values$Weekday,
  paired_values$Weekend,
  paired = TRUE,
  exact = FALSE,
  conf.int = TRUE
)
comparison_result <- tibble::tibble(
  method = "Paired Wilcoxon signed-rank test on station mean daily counts",
  paired_stations = nrow(paired_values),
  weekday_mean_across_stations = mean(paired_values$Weekday),
  weekend_mean_across_stations = mean(paired_values$Weekend),
  median_paired_difference_weekday_minus_weekend =
    stats::median(paired_values$difference),
  wilcoxon_statistic = unname(weekday_weekend_test$statistic),
  p_value = weekday_weekend_test$p.value,
  shapiro_wilk_statistic_for_differences = unname(normality_test$statistic),
  shapiro_p_value = normality_test$p.value,
  normality_note = ifelse(
    normality_test$p.value < 0.05,
    "Paired differences show evidence against normality; nonparametric test used.",
    "No strong evidence against normality; nonparametric test retained."
  )
)
readr::write_csv(
  comparison_result,
  "outputs/tables/weekday_weekend_test.csv"
)
readr::write_csv(paired_values, "outputs/tables/paired_station_day_means.csv")

# Log1p OLS helps with the right-skewed nonnegative count outcome. Factors
# adjust for station baseline, hour, and month; coefficients are associations.
regression_data <- data |>
  dplyr::mutate(
    log_passengers = log1p(.data$passengers),
    weekend = as.integer(.data$day_type == "Weekend"),
    holiday_indicator = as.integer(.data$holiday == "Holiday"),
    hour_factor = factor(.data$hour),
    month_factor = factor(.data$month),
    station_factor = factor(.data$station_id)
  )

model <- stats::lm(
  log_passengers ~ weekend + holiday_indicator + hour_factor +
    month_factor + station_factor,
  data = regression_data
)
model_tidy <- broom::tidy(model, conf.int = TRUE)
model_glance <- broom::glance(model)
readr::write_csv(model_tidy, "outputs/tables/regression_coefficients.csv")
readr::write_csv(model_glance, "outputs/tables/regression_model_summary.csv")

set.seed(17)
residuals <- stats::residuals(model)
sampled_residuals <- sample(residuals, size = min(5000L, length(residuals)))
residual_normality <- stats::shapiro.test(sampled_residuals)
fitted_quartile <- dplyr::ntile(stats::fitted(model), 4)
residual_variance_by_fitted_quartile <- tibble::tibble(
  fitted_quartile = fitted_quartile,
  residual = residuals
) |>
  dplyr::group_by(.data$fitted_quartile) |>
  dplyr::summarise(
    residual_sd = stats::sd(.data$residual),
    .groups = "drop"
  )
assumption_checks <- tibble::tibble(
  check = c(
    "Mean residual (should be near zero for OLS with intercept)",
    "Shapiro-Wilk test on reproducible sample of at most 5000 residuals",
    "Residual standard deviation, lowest fitted-value quartile",
    "Residual standard deviation, second fitted-value quartile",
    "Residual standard deviation, third fitted-value quartile",
    "Residual standard deviation, highest fitted-value quartile"
  ),
  value = c(
    mean(residuals),
    unname(residual_normality$statistic),
    residual_variance_by_fitted_quartile$residual_sd
  ),
  p_value = c(
    NA_real_,
    residual_normality$p.value,
    rep(NA_real_, 4)
  )
)
readr::write_csv(assumption_checks, "outputs/tables/regression_assumption_checks.csv")

message(
  "Statistical analysis complete. Weekday/weekend test p-value: ",
  signif(weekday_weekend_test$p.value, 4),
  ". Model tables and numeric assumption checks are saved under outputs/tables/."
)
