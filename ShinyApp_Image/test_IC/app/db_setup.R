# install.packages(c("dbplyr", "RSQLite"))
library(dplyr)
library(dbplyr)

pt_data <- readRDS("R_data/module_db/monthlyPt.rds")

pt_data$Pt[169:180] <- pt_data$Pt[1:12]

new_pt_data <- data.frame(
  year = as.integer(substr(pt_data$date_val, 1, 4)),
  month = as.integer(substr(pt_data$date_val, 6, 7)),
  pt = pt_data$Pt
)

# create sqlite database
my_db_file <- "R_data/module_db/test_db.sqlite"
src_sqlite(my_db_file, create = TRUE)

# save data into database
con <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)
DBI::dbWriteTable(con, "meteo", new_pt_data)
DBI::dbDisconnect(con)


# read data  --------------------------------------------------------------

my_db_file <- "R_data/module_db/test_db.sqlite"
con <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)

tmp_db <- tbl(con, "meteo") %>%
  # filter(year>2003 & year<2006) %>%
  collect()
DBI::dbDisconnect(con)


# manipulate data
# db_tmp <- data.frame(
#   date = paste(as.character(tmp_db$year), as.character(tmp_db$month), sep = "-"),
#   val  = tmp_db$pt
# )



# prepare data in MySQL ---------------------------------------------------

library(dplyr)
library(dbplyr)
library(RMySQL)

pt_data <- readRDS("R_data/module_db/monthlyPt.rds")

pt_data$Pt[169:180] <- pt_data$Pt[1:12]

new_pt_data <- data.frame(
  year = as.integer(substr(pt_data$date_val, 1, 4)),
  month = as.integer(substr(pt_data$date_val, 6, 7)),
  pt = pt_data$Pt
)


# save data into database
mydb = DBI::dbConnect(MySQL(), user='root', password='example', dbname='edss_db', host='127.0.0.1')

DBI::dbWriteTable(mydb, "meteo", new_pt_data)
DBI::dbDisconnect(mydb)

# check if R can read the data from the database 
con = DBI::dbConnect(MySQL(), user='root', password='example', dbname='edss_db', host='127.0.0.1')
tmp_db <- tbl(con, "meteo") %>% collect()
DBI::dbDisconnect(con)
