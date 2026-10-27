# Interactive dashboard for the MTA Project 17 analysis.

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop(
    "The dashboard needs the shiny package. Install it with ",
    "install.packages('shiny', repos = 'https://cloud.r-project.org').",
    call. = FALSE
  )
}

app_directory <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
project_directory <- if (file.exists(file.path(app_directory, "R", "01_data_cleaning.R"))) {
  app_directory
} else {
  normalizePath(file.path(app_directory, ".."), winslash = "/", mustWork = TRUE)
}
if (!file.exists(file.path(project_directory, "R", "01_data_cleaning.R"))) {
  stop(
    "Could not locate the project files. Launch the dashboard with ",
    "shiny::runApp('dashboard') from the project folder.",
    call. = FALSE
  )
}
setwd(project_directory)

source(file.path("R", "helpers.R"))
invisible(set_project_root())
check_required_packages()

clean_path <- file.path(
  "data", "processed", "mta_subway_ridership_2024_clean.csv"
)
if (!file.exists(clean_path)) {
  message("Cleaned data not found; running the data-cleaning script first.")
  source(file.path("R", "01_data_cleaning.R"), local = FALSE)
}

dashboard_data <- read_clean_data() |>
  dplyr::mutate(
    station_id = as.character(.data$station_id),
    station = as.character(.data$station),
    weekday = factor(
      .data$weekday,
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                 "Friday", "Saturday", "Sunday"),
      ordered = TRUE
    ),
    day_type = factor(.data$day_type, levels = c("Weekday", "Weekend"))
  )

station_choices <- dashboard_data |>
  dplyr::distinct(.data$station, .data$station_id) |>
  dplyr::arrange(.data$station)
station_labels <- stats::setNames(
  station_choices$station_id,
  station_choices$station
)
minimum_date <- min(dashboard_data$date)
maximum_date <- max(dashboard_data$date)

app_ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$title("NYC Transit Ridership Dashboard"),
    shiny::tags$style(shiny::HTML("
      body { background: #f2f5f9; color: #1e293b; font-family: 'Segoe UI', Arial, sans-serif; }
      .dashboard-header { background: linear-gradient(120deg, #0e3b5c, #176b87);
        color: white; padding: 26px 30px; margin: 0 -15px 20px; border-radius: 0 0 14px 14px; }
      .dashboard-header h1 { margin: 0 0 8px; font-size: 28px; font-weight: 700; }
      .dashboard-header p { margin: 0; color: #e0f2fe; }
      .sidebar-panel, .chart-card, .summary-card { background: white; border-radius: 12px;
        padding: 18px; box-shadow: 0 2px 10px rgba(15, 23, 42, .07); margin-bottom: 16px; }
      .sidebar-panel h3 { margin-top: 0; font-size: 18px; color: #0e3b5c; }
      .chart-card h3 { margin: 0 0 10px; font-size: 17px; color: #0e3b5c; }
      .summary-card { border-left: 4px solid #176b87; min-height: 94px; }
      .summary-label { color: #64748b; font-size: 12px; text-transform: uppercase;
        letter-spacing: .04em; }
      .summary-value { color: #0e3b5c; font-size: 24px; font-weight: 700;
        margin-top: 8px; }
      .small-note { color: #64748b; font-size: 12px; line-height: 1.5; }
      .btn-primary { background: #176b87; border-color: #176b87; }
      .btn-primary:hover { background: #0e3b5c; border-color: #0e3b5c; }
      .shiny-output-error-validation { color: #9a3412; }
      .table { background: white; }
      @media (max-width: 767px) {
        .dashboard-header { padding: 20px; }
        .dashboard-header h1 { font-size: 23px; }
      }
    "))
  ),
  shiny::div(
    class = "dashboard-header",
    shiny::h1("NYC Public Transit Ridership Dashboard"),
    shiny::p(
      "Explore 2024 MTA subway passenger and transfer patterns. ",
      "Use the filters to update all five charts and summary statistics."
    )
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::div(
        class = "sidebar-panel",
        shiny::h3("Dashboard filters"),
        shiny::dateRangeInput(
          "date_range", "Date range",
          start = minimum_date, end = maximum_date,
          min = minimum_date, max = maximum_date,
          format = "yyyy-mm-dd", weekstart = 1
        ),
        shiny::selectizeInput(
          "stations", "Station complexes",
          choices = station_labels, selected = unname(station_labels),
          multiple = TRUE,
          options = list(
            plugins = list("remove_button"),
            placeholder = "Choose one or more stations"
          )
        ),
        shiny::selectInput(
          "day_type", "Day type",
          choices = c("All days" = "All",
                      "Weekdays" = "Weekday",
                      "Weekends" = "Weekend"),
          selected = "All",
          selectize = FALSE
        ),
        shiny::radioButtons(
          "measure", "Measure",
          choices = c("Passengers" = "passengers", "Transfers" = "transfers"),
          selected = "passengers", inline = TRUE
        ),
        shiny::actionButton(
          "reset_filters", "Reset filters",
          class = "btn-primary", icon = shiny::icon("rotate-left")
        ),
        shiny::hr(),
        shiny::downloadButton("download_data", "Download filtered CSV"),
        shiny::hr(),
        shiny::p(
          class = "small-note",
          "Source: NYC MTA Subway Hourly Ridership, 2020-2024. ",
          "The dashboard uses ten selected stations, not the full subway network. ",
          "Unreported hours are not treated as zero."
        )
      )
    ),
    shiny::mainPanel(
      width = 9,
      shiny::uiOutput("summary_cards"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::div(
            class = "chart-card",
            shiny::h3("Station demand"),
            shiny::plotOutput("station_plot", height = "330px")
          )
        ),
        shiny::column(
          width = 6,
          shiny::div(
            class = "chart-card",
            shiny::h3("Demand over time"),
            shiny::plotOutput("trend_plot", height = "330px")
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::div(
            class = "chart-card",
            shiny::h3("Demand by hour"),
            shiny::plotOutput("hourly_plot", height = "330px")
          )
        ),
        shiny::column(
          width = 6,
          shiny::div(
            class = "chart-card",
            shiny::h3("Weekday and hour heat map"),
            shiny::plotOutput("heatmap_plot", height = "330px")
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::div(
            class = "chart-card",
            shiny::h3("Weekday versus weekend"),
            shiny::plotOutput("day_type_plot", height = "360px"),
            shiny::p(
              class = "small-note",
              "This comparison uses complete 24-hour station-days only. ",
              "The date and station filters apply; the day-type selector is ",
              "intentionally ignored here so both groups remain visible."
            )
          )
        )
      ),
      shiny::div(
        class = "chart-card",
        shiny::h3("Station summary"),
        shiny::tableOutput("station_table")
      )
    )
  )
)

app_server <- function(input, output, session) {
  observeEvent(input$reset_filters, {
    shiny::updateDateRangeInput(
      session, "date_range", start = minimum_date, end = maximum_date
    )
    shiny::updateSelectizeInput(
      session, "stations", selected = unname(station_labels)
    )
    shiny::updateSelectInput(session, "day_type", selected = "All")
    shiny::updateRadioButtons(session, "measure", selected = "passengers")
  })

  date_station_data <- shiny::reactive({
    shiny::req(input$date_range)
    shiny::validate(
      shiny::need(length(input$stations) > 0, "Select at least one station.")
    )
    dashboard_data |>
      dplyr::filter(
        .data$date >= input$date_range[[1]],
        .data$date <= input$date_range[[2]],
        .data$station_id %in% input$stations
      )
  })

  filtered_data <- shiny::reactive({
    data <- date_station_data()
    if (identical(input$day_type, "All")) {
      data
    } else {
      dplyr::filter(data, .data$day_type == input$day_type)
    }
  })

  metric_label <- shiny::reactive({
    if (identical(input$measure, "transfers")) "Transfers" else "Passengers"
  })

  metric_value <- function(data) {
    shiny::validate(
      shiny::need(nrow(data) > 0, "No records match these filters.")
    )
    sum(data[[input$measure]], na.rm = TRUE)
  }

  output$summary_cards <- shiny::renderUI({
    data <- filtered_data()
    coverage_data <- date_station_data()
    shiny::validate(
      shiny::need(nrow(data) > 0, "No records match these filters.")
    )
    expected <- length(input$stations) *
      length(seq(input$date_range[[1]], input$date_range[[2]], by = "day")) * 24
    coverage <- if (expected > 0) nrow(coverage_data) / expected else NA_real_
    peak <- data |>
      dplyr::group_by(.data$hour) |>
      dplyr::summarise(
        average = mean(.data[[input$measure]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::slice_max(.data$average, n = 1, with_ties = FALSE)

    shiny::fluidRow(
      shiny::column(
        3, shiny::div(
          class = "summary-card",
          shiny::div(class = "summary-label", paste("Total", metric_label())),
          shiny::div(class = "summary-value", scales::comma(metric_value(data)))
        )
      ),
      shiny::column(
        3, shiny::div(
          class = "summary-card",
          shiny::div(class = "summary-label", "Mean per reported station-hour"),
          shiny::div(
            class = "summary-value",
            scales::comma(round(mean(data[[input$measure]], na.rm = TRUE), 1))
          )
        )
      ),
      shiny::column(
        3, shiny::div(
          class = "summary-card",
          shiny::div(class = "summary-label", "Peak hour"),
          shiny::div(
            class = "summary-value",
            sprintf("%02d:00", peak$hour[[1]])
          )
        )
      ),
      shiny::column(
        3, shiny::div(
          class = "summary-card",
          shiny::div(class = "summary-label", "Reported-hour coverage"),
          shiny::div(class = "summary-value", scales::percent(coverage, accuracy = 0.1))
        )
      )
    )
  })

  output$station_plot <- shiny::renderPlot({
    data <- filtered_data()
    shiny::validate(shiny::need(nrow(data) > 0, "No matching data."))
    summary <- data |>
      dplyr::group_by(.data$station) |>
      dplyr::summarise(
        average = mean(.data[[input$measure]], na.rm = TRUE),
        .groups = "drop"
      )
    ggplot2::ggplot(
      summary,
      ggplot2::aes(
        x = stats::reorder(.data$station, .data$average),
        y = .data$average
      )
    ) +
      ggplot2::geom_col(fill = "#176B87") +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels = scales::label_comma()) +
      ggplot2::labs(
        x = NULL, y = paste("Mean", tolower(metric_label()), "per reported hour")
      ) +
      ggplot2::theme_minimal(base_size = 11)
  })

  output$trend_plot <- shiny::renderPlot({
    data <- filtered_data()
    shiny::validate(shiny::need(nrow(data) > 0, "No matching data."))
    summary <- data |>
      dplyr::group_by(.data$date) |>
      dplyr::summarise(
        average = mean(.data[[input$measure]], na.rm = TRUE),
        .groups = "drop"
      )
    plot <- ggplot2::ggplot(
      summary, ggplot2::aes(x = .data$date, y = .data$average)
    ) +
      ggplot2::scale_y_continuous(labels = scales::label_comma()) +
      ggplot2::labs(
        x = "Date", y = paste("Mean", tolower(metric_label()), "per reported hour")
      ) +
      ggplot2::theme_minimal(base_size = 11)
    if (nrow(summary) == 1L) {
      plot <- plot + ggplot2::geom_point(color = "#176B87", size = 2)
    } else {
      plot <- plot + ggplot2::geom_line(color = "#176B87", linewidth = 0.45)
    }
    if (nrow(summary) >= 3L) {
      plot <- plot +
        ggplot2::geom_smooth(
          method = "loess", se = FALSE, color = "#D95F02"
        )
    }
    plot
  })

  output$hourly_plot <- shiny::renderPlot({
    data <- filtered_data()
    shiny::validate(shiny::need(nrow(data) > 0, "No matching data."))
    summary <- data |>
      dplyr::group_by(.data$hour) |>
      dplyr::summarise(
        average = mean(.data[[input$measure]], na.rm = TRUE),
        .groups = "drop"
      )
    ggplot2::ggplot(summary, ggplot2::aes(x = .data$hour, y = .data$average)) +
      ggplot2::geom_col(fill = "#176B87") +
      ggplot2::scale_x_continuous(breaks = 0:23) +
      ggplot2::scale_y_continuous(labels = scales::label_comma()) +
      ggplot2::labs(
        x = "Hour (24-hour clock)",
        y = paste("Mean", tolower(metric_label()), "per reported hour")
      ) +
      ggplot2::theme_minimal(base_size = 11)
  })

  output$heatmap_plot <- shiny::renderPlot({
    data <- filtered_data()
    shiny::validate(shiny::need(nrow(data) > 0, "No matching data."))
    summary <- data |>
      dplyr::group_by(.data$weekday, .data$hour) |>
      dplyr::summarise(
        average = mean(.data[[input$measure]], na.rm = TRUE),
        .groups = "drop"
      )
    ggplot2::ggplot(
      summary,
      ggplot2::aes(x = .data$hour, y = .data$weekday, fill = .data$average)
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 23)) +
      ggplot2::scale_fill_viridis_c(labels = scales::label_comma()) +
      ggplot2::labs(x = "Hour", y = NULL, fill = metric_label()) +
      ggplot2::theme_minimal(base_size = 11)
  })

  output$day_type_plot <- shiny::renderPlot({
    data <- date_station_data()
    complete_days <- data |>
      dplyr::group_by(.data$date, .data$station_id, .data$station, .data$day_type) |>
      dplyr::summarise(
        observed_hours = dplyr::n(),
        value = sum(.data[[input$measure]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(.data$observed_hours == 24L)
    shiny::validate(
      shiny::need(
        nrow(complete_days) > 0,
        "No complete 24-hour station-days match this date and station selection."
      )
    )
    ggplot2::ggplot(
      complete_days,
      ggplot2::aes(x = .data$day_type, y = .data$value, fill = .data$day_type)
    ) +
      ggplot2::geom_boxplot(outlier.alpha = 0.18) +
      ggplot2::scale_y_continuous(labels = scales::label_comma()) +
      ggplot2::scale_fill_manual(
        values = c("Weekday" = "#176B87", "Weekend" = "#E6A23C")
      ) +
      ggplot2::labs(
        x = NULL, y = paste(metric_label(), "per complete station-day")
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "none")
  })

  output$station_table <- shiny::renderTable({
    data <- filtered_data()
    shiny::validate(shiny::need(nrow(data) > 0, "No matching data."))
    data |>
      dplyr::group_by(.data$station, .data$borough) |>
      dplyr::summarise(
        reported_station_hours = dplyr::n(),
        total = sum(.data[[input$measure]], na.rm = TRUE),
        mean_per_reported_hour = mean(.data[[input$measure]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$total)) |>
      dplyr::mutate(
        total = scales::comma(.data$total),
        mean_per_reported_hour = scales::comma(
          round(.data$mean_per_reported_hour, 1)
        )
      ) |>
      dplyr::rename(
        Station = station,
        Borough = borough,
        `Reported hours` = reported_station_hours,
        `Total selected measure` = total,
        `Mean per reported hour` = mean_per_reported_hour
      )
  }, striped = TRUE, bordered = FALSE, spacing = "s", width = "100%")

  output$download_data <- shiny::downloadHandler(
    filename = function() {
      paste0("mta_filtered_", input$measure, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(filtered_data(), file)
    }
  )
}

shiny::shinyApp(ui = app_ui, server = app_server)
