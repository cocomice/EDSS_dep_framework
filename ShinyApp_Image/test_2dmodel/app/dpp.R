# for the deployment of the shinyApp

packages <- c("shiny", "shinyBS", "shinyjs", "shinyWidgets", "leaflet",
              "reshape2", "raster", "maptools", "fields", "rAmCharts", "pipeR",
              "formattable", "rgeos", "readxl" )

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(
    setdiff(packages, rownames(installed.packages())),
    dependencies = T,
    repos='https://cloud.r-project.org/'
  )
}
