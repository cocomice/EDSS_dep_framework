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
library(RMySQL)


# ==== %% Load default data %% ====

load("R_data/ShinyApp_char_dict.RData")

sys_date <- Sys.Date() # get current date
currYr <- year(sys_date)

# language switch: 1-Chinese | 2-English (default)
idx_lang <- 2

db.info <- data.frame(
  user='root',
  password='example',
  dbname='edss_db',
  host='db',
  port = 3306,
  stringsAsFactors = F
)

# ==== %% General Information %% ====

itemNames <- c(
  "maize",
  "summer-vegetation",
  "wheat",
  "winter-vegetation"
)

crop_types <- list(
  cn = c(
    "苹果树", "棉花", "黄瓜", "玉米", "小米",
    "桃树", "花生", "梨树", "秋土豆", "春土豆",
    "高粱", "大豆", "甜瓜", "红薯",
    "番茄", "蔬菜", "豆类", "葡萄", "冬小麦"
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
