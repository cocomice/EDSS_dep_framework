rm(list = ls()) # clear the workspace for a clean start

# ==== %% General App Configuration %% ====

loc <- function(os, language = "english") {
  switch(language,
    english = ifelse(os == "Windows", "English_United States.1252", "en_US.UTF-8"),
    chinese = ifelse(os == "Windows", "Chinese", "zh_CN.utf-8")
  )
}

# Set locale language encoding for Chinese
Sys.setlocale(category = "LC_ALL", loc(Sys.info()[["sysname"]], "chinese"))

# Turn off warning message (-1 - 'off' | 0 - 'on')
options(warn = -1)

# ==== %% Load source functions and libraries %% ====

library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(leaflet)
library(reshape2)
library(raster)
library(maptools)
library(fields)
library(rAmCharts)
library(formattable)
library(pipeR)
library(rgeos)
library(readxl)

source("R_sources/fun_mfl_runner.R")
source("R_sources/fun_RES_eval.R")
source("R_sources/fun_loader.R")
source("R_sources/fun_calc_recharge.R")
source("R_sources/fun_reset_handles.R")
source("R_sources/fun_readMF_output.R")
source("R_sources/fun_preprocess.R")
source("R_sources/ReadModflowBinary.R")
source("R_sources/approxExtrap.R")


# ==== %% load database %% ====

load("R_data/ShinyApp_char_dict.RData")

# ==== %% Load default settings %% ====

default_dist <- 8 # default district index
data.BS <- fun_loader()
data.handles <- fun_reset_handles(data.BS, default_dist)
default_nrYrs <- data.BS$t_steps_default

# configure the marker icon image
mapIcon <- makeIcon(
  iconUrl = "www/marker_red.png",
  iconWidth = 22, iconHeight = 40,
  iconAnchorX = 11, iconAnchorY = 40
)

