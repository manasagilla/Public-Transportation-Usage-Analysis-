# Shared helpers for running the project from the project root or an R script.

get_project_root <- function() {
  candidate <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (file.exists(file.path(candidate, "R", "01_data_cleaning.R"))) {
    return(candidate)
  }

  script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(script_argument) > 0) {
    script_path <- sub("^--file=", "", script_argument[[1]])
    script_directory <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
    candidate <- normalizePath(file.path(script_directory, ".."), winslash = "/", mustWork = TRUE)
    if (file.exists(file.path(candidate, "R", "01_data_cleaning.R"))) {
      return(candidate)
    }
  }

  stop(
    "Could not find the project root. Open a terminal in the project folder ",
    "or run R/01_data_cleaning.R from the project folder.",
    call. = FALSE
  )
}

set_project_root <- function() {
  project_root <- get_project_root()
  setwd(project_root)
  project_root
}

check_required_packages <- function() {
  required_packages <- c(
    "tidyverse", "ggplot2", "lubridate", "janitor", "broom",
    "knitr", "rmarkdown"
  )
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      "Missing required R package(s): ", paste(missing_packages, collapse = ", "),
      ". Install them in R with:\ninstall.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "), repos = \"https://cloud.r-project.org\")",
      call. = FALSE
    )
  }
}

configure_pandoc <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    return(invisible(NULL))
  }

  bundled_path <- Sys.getenv("RSTUDIO_PANDOC", unset = "")
  if (!nzchar(bundled_path)) {
    bundled_path <- file.path(
      Sys.getenv("ProgramFiles", unset = "C:/Program Files"),
      "RStudio", "resources", "app", "bin", "quarto", "bin", "tools"
    )
  }

  pandoc_executable <- if (.Platform$OS.type == "windows") {
    file.path(bundled_path, "pandoc.exe")
  } else {
    file.path(bundled_path, "pandoc")
  }
  if (nzchar(bundled_path) && file.exists(pandoc_executable)) {
    rmarkdown::find_pandoc(dir = bundled_path, cache = FALSE)
  }
  invisible(NULL)
}

ensure_output_directories <- function() {
  directories <- c("data/raw", "data/processed", "outputs/plots", "outputs/tables")
  for (directory in directories) {
    if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE)) {
      stop("Could not create output directory: ", directory, call. = FALSE)
    }
  }
}

clean_numeric_count <- function(x) {
  value <- suppressWarnings(as.numeric(as.character(x)))
  value[!is.finite(value) | value < 0] <- NA_real_
  value
}

read_clean_data <- function() {
  path <- file.path("data", "processed", "mta_subway_ridership_2024_clean.csv")
  if (!file.exists(path)) {
    stop(
      "The cleaned dataset is missing. Run R/01_data_cleaning.R first.",
      call. = FALSE
    )
  }

  readr::read_csv(path, show_col_types = FALSE)
}
