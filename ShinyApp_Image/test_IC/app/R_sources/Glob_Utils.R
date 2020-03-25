glob_util.getFile_Pt <- function(startYr, endYr) {
  con <- DBI::dbConnect(MySQL(), user='root', password='example', dbname='edss_db', host='db')

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
