# YU LI

fun_readMF_output <- function(data.BS, data.RES, data.handles) {
  tStepsDefault <- data.BS$t_steps_default
  nrCol <- data.BS$NC
  nrRow <- data.BS$NR
  t_steps <- data.handles$t_steps # here it is the same as tStepsDefault
  iniHead <- data.BS$iniHead

  inHeadFile_tr <- data.BS$path_head_tr_file
  inHeadFile_ss <- data.BS$path_head_ss_file

  inBudgtFile <- data.BS$path_budget_ss_file

  ## ==== Read transient head and calculate drawdown ====
  
  mat_dlevel_array <- array(0, dim=c(nrCol, nrRow, t_steps))
  rwData <- ReadModflowBinary(inHeadFile_tr, "array")
  
  for (i in 1:t_steps) {
    tmpData <- rwData[[i]]$d
    tmpData[tmpData < -999] <- NaN
    tmpData[tmpData > 1e10] <- NaN

    # calculate the time-varying drawdown
    mat_dlevel_array[, , i] <- t(apply(tmpData, 2, rev)) - iniHead
  }

  data.RES$mat_dlevel_array <- mat_dlevel_array
  data.RES$mat_dlevel <- mat_dlevel_array[, , t_steps]

  ## ==== Read steady state head and calculate drawdown ====

  rwData <- ReadModflowBinary(inHeadFile_ss, "array")

  tmpData <- rwData[[1]]$d
  tmpData[tmpData < -999] <- NaN
  tmpData[tmpData > 1e10] <- NaN

  # calculate the drawdown
  data.RES$mat_dlevel_ss <- t(apply(tmpData, 2, rev)) - iniHead

  ## ==== Read groundwater budget ====

  rwData <- ReadModflowBinary(inBudgtFile, "flow")
  nrElements <- length(rwData)

  # idx_drain_last = 0
  idx_et_last <- 0

  ctr_drain <- 1
  ctr_leak <- 1

  for (i in 1:nrElements) {
    if (rwData[[i]]$desc == "drains") {
      data.RES$riv_WB$In$Drain_from_GW[ctr_drain] <- -365 * sum(rwData[[i]]$d[[1]])
      ctr_drain <- ctr_drain + 1
    } else if (rwData[[i]]$desc == "et") {
      idx_et_last <- i
    } else if (rwData[[i]]$desc == "river leakage") {
      data.RES$riv_WB$Out$Drain_to_GW[ctr_leak] <- 365 * sum(rwData[[i]]$d[[1]])
      ctr_leak <- ctr_leak + 1
    } else {
      next
    }
  }

  return(data.RES)
}