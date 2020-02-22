# dpp.R declares all the packages needed to run your Shiny app. They will be
# installed during the building process of the Docker image for your Shiny app


# please list all the packages as a string list, e.g., c("shiny", "ggplot2", ...)
packages <- c("shiny")


if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(
    setdiff(packages, rownames(installed.packages())),
    dependencies = T,
    repos='https://cloud.r-project.org/'
  )
}
