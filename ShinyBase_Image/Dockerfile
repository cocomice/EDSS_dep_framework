FROM openanalytics/r-base

MAINTAINER Yu Li "likymice@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y sudo

# dependency libraries
RUN apt-get install -y libgeos-dev libgdal-dev libproj-dev gfortran libgfortran3

# R packages for shinyApp
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinythemes', 'shinyjs', 'shinyWidgets','shinycssloaders'), repos='https://cloud.r-project.org/')"
