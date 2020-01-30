# for the deployment of the shinyApp

packages <- c("shiny", "shinydashboard", "shinythemes",
 "shinyBS", "shinyjs", "shinyWidgets", "shinycssloaders",
"leaflet", "rAmCharts", "reshape2", "raster", "maptools", "fields",
"pipeR", "rgdal", "rgeos", "DT", "lubridate", "zoo", "rhandsontable")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), dependencies = T)
}

RVer <- version 

if (RVer$major == 3 && RVer$minor >= 5){
	install.packages("BiocManager")
	BiocManager::install("rhdf5")
}else{
	source("http://bioconductor.org/biocLite.R")
	biocLite("rhdf5")
}

source("dict_create.R")
