glob_util.getFile_Pt <- function(db.info, startYr, endYr) {
  con <- DBI::dbConnect(MySQL(), user=db.info$user, password=db.info$password, dbname=db.info$dbname, host=db.info$host, port = db.info$port)

  tmp_db <- tbl(con, "meteo") %>%
    filter(year >= startYr & year <= endYr) %>%
    collect()

  DBI::dbDisconnect(con)

  df <- data.frame(
    date = paste(as.character(tmp_db$year), as.character(tmp_db$month), sep = "-"),
    val = tmp_db$pt
  )

  colnames(df) <- c(
    "date",
    "prcp[mm]"
  )

  outFile <- "R_data/tmp/monthlyPt.csv"
  write.csv(df, outFile,
    row.names = F,
    quote = F,
    na = "-9999"
  )

  return(outFile)
}
