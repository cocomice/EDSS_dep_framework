
fun_mfl_runner <- function(data.BS, data.RES, data.handles) {

  # Adjust the matrix orientation. Equal to 'fliplr(data.RES$mat_rech_input)'
  x <- data.RES$mat_rech_input[, rev(seq(ncol(data.RES$mat_rech_input)))]
  t_steps <- data.handles$t_steps

  ## ==== Write data to riv6.dat ====
  inf_sec1 <- 0.14 * data.RES$riv_WB$In$Q_Heihe
  inf_sec2 <- 0.17 * data.RES$riv_WB$In$Q_Heihe
  inf_sec3 <- 0.31 * data.RES$riv_WB$In$Q_Liyuan

  txtRaw <- readLines("model/riv6_org.dat")
  idx_riv <- scan("R_data/idx_river.csv")

  ctr <- 0

  totLines <- length(txtRaw)

  for (i in seq(1, totLines)) {
    strList <- strsplit(txtRaw[i], "\t")[[1]]

    if (length(strList) == 7) {
      ctr <- ctr + 1

      if (idx_riv[ctr] == 1) {
        newVal <- inf_sec1 / 365 / data.BS$riv$cells_sec1
      } else if (idx_riv[ctr] == 2) {
        newVal <- inf_sec2 / 365 / data.BS$riv$cells_sec2
      } else if (idx_riv[ctr] == 3) {
        newVal <- inf_sec3 / 365 / data.BS$riv$cells_sec3
      } else {
        next
      }

      strList[6] <- format(newVal, nsmall = 2) # modify flow rate
      txtRaw[i] <- paste(strList, collapse = "\t")
    }
  }

  write(txtRaw, file = "model/riv6.dat", ncolumns = 1)
  
  ## ==== Write data to rch6.dat ====

  filename <- data.BS$path_rch_file

  if (file.exists(filename)) {
    file.remove(filename)
  }

  cat(
    paste(
      "# RECHARGE file for MODFLOW-2000 by Yu Li\n",
      "1        0\n",
      "1        0\n",
      "        18         1(1G14.0)                    -1  RECHARGE array\n", 
     sep=""
     ),
    file = filename,
    append = T
  )

  write(
    sprintf("%14.8G", as.vector(x)),
    file = filename,
    ncolumns = 1,
    append = T,
    sep = " "
  )
  
  
  ## ==== Call Modflow executable ====
  OS_info <- Sys.info()

  if (tolower(OS_info[1]) %in% "windows") {
    system("model/mf2005_Win.exe model/model_ss.nam")
    system("model/mf2005_Win.exe model/model_tr.nam")
  } else {
    system("model/mf2005_Unix.exe model/model_ss.nam &")
    system("model/mf2005_Unix.exe model/model_tr.nam")
  }

  return(NULL)
}