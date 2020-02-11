# for the deployment of the shinyApp

packages <- c("shiny", "shinydashboard", "shinyjs", "shinyWidgets", "shinycssloaders",
  "rAmCharts", "pipeR",  "DT", "lubridate", "dplyr", "rjson", "dbplyr", "RSQLite",
  "leaflet", "rgdal", "rgeos")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(
    setdiff(packages, rownames(installed.packages())),
    dependencies = T,
    repos='https://cloud.r-project.org/'
  )
}
