
fun_RES_eval <- function(data.BS, data.RES, data.handles) {
  no_yrs <- data.handles$t_sim / 365
  data.RES <- fun_readMF_output(data.BS, data.RES, data.handles)

  data.RES$riv_WB$Out$Irr_SW <- sum(data.RES$irrigation_results[[4]]) # SW irrigation

  # calculate river water balance [steady state mode]
  data.RES$riv_WB$Out$Env_flow[1] <- data.RES$riv_WB$In$Q_Heihe +
    data.RES$riv_WB$In$Q_Liyuan +
    data.RES$riv_WB$In$Drain_from_GW[1] -
    data.RES$riv_WB$Out$Drain_to_GW[1] -
    data.RES$riv_WB$Out$Irr_SW
  
  return(data.RES)
} # end of "fun_RES_eval.R"