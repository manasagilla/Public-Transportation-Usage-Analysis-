# Run this file from the VS Code terminal to rebuild all project outputs.

source(file.path("R", "helpers.R"))
invisible(set_project_root())

source(file.path("R", "01_data_cleaning.R"))
source(file.path("R", "02_eda_visualizations.R"))
source(file.path("R", "03_statistical_analysis.R"))

check_required_packages()
configure_pandoc()
rmarkdown::render(
  input = "report.Rmd",
  output_file = "report.html",
  envir = new.env(parent = globalenv()),
  quiet = TRUE
)

message("Project finished. Open report.html to view the analysis.")
