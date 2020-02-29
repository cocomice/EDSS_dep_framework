# for the deployment of the shinyApp

packages <- c("shiny", "ncdf", "mnormt", "MASS", "zoo",
              "maps", "mapproj", "psych", "mnormt", "fields", "plotrix",
              "chron", "leaflet", "shinyBS", "devtools", "rsconnect")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(
    setdiff(packages, rownames(installed.packages())),
    dependencies = T,
    repos='https://cloud.r-project.org/'
  )
}

devtools::install_github("trestletech/ShinyDash")
