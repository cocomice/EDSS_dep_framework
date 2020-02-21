# global.R

rm(list = ls())

# Set locale language encoding in order to display characters correctly
loc <- function(os, language = "english") {
  switch(language,
    english = ifelse(os == "Windows", "English_United States.1252", "en_US.UTF-8"),
    chinese = ifelse(os == "Windows", "Chinese", "zh_CN.utf-8")
  )
}

Sys.setlocale(category = "LC_ALL", loc(Sys.info()[["sysname"]], "chinese"))

# ==== %% Load functions and libraries %% ====
source("R_sources/IC.R") # irrigation calculator module
source("R_sources/Glob_Utils.R") # utility modules

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)

library(rgdal)
library(rgeos)
library(leaflet)
library(rAmCharts)
library(pipeR)
library(DT)
library(lubridate)
library(dplyr)
library(dbplyr)


# ==== %% Load default data %% ====

load("R_data/ShinyApp_char_dict.RData")

sys_date <- Sys.Date() # get current date
currYr <- year(sys_date)

# language switch: 1-Chinese | 2-English (default)
idx_lang <- 2

# ==== %% General Information %% ====

itemNames <- c(
  "maize",
  "summer-vegetation",
  "wheat",
  "winter-vegetation"
)

crop_types <- list(
  cn = c(
    "<U+82F9><U+679C>", "<U+68C9><U+82B1>", "<U+9EC4><U+74DC>", "<U+7389><U+7C73>", "<U+5C0F><U+7C73>",
    "<U+6843><U+5B50>", "<U+82B1><U+751F>", "<U+68A8>", "<U+79CB><U+571F><U+8C46>", "<U+6625><U+571F><U+8C46>",
    "<U+9AD8><U+7CB1>", "<U+5927><U+8C46>", "<U+751C><U+74DC>", "<U+7EA2><U+85AF>",
    "<U+897F><U+7EA2><U+67FF>", "<U+852C><U+83DC>", "<U+7EFF><U+8C46>", "<U+8461><U+8404>", "<U+51AC><U+5C0F><U+9EA6>"
  ),
  en = c(
    "Apples", "Cotton", "Cucumber", "Maize", "Millet",
    "Peach", "Peanut", "Pears", "Potato (Autumn)", "Potato (Spring)",
    "Sorghum (Grain)", "Soy Bean", "Sweet Melon", "Sweet Potato",
    "Tomato", "Vegetables", "Vigna Radiata", "Grape", "Winter Wheat"
  )
)

irrNorm_param <- readRDS("R_data/module_IC/irrNrom_param.rds")
cropParam <- readRDS("R_data/module_IC/cropParam.rds")

crop_id_ini <- 4 # UI_IC_Tab$BT_str$selector_cropChoice1[[idx_lang]][[1]]
startdate_ini <- as.character(cropParam$DATERANGESTART[crop_id_ini])
enddate_ini <- as.character(cropParam$DATERANGEEND[crop_id_ini])
