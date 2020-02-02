
fun_calc_recharge <- function(data.BS, data.handles) {
  no_yrs <- data.handles$t_sim / 365

  # Initialize data.RES structure
  data.RES <- list(

    # Output matrix at final time step
    mat_dlevel = matrix(),
    mat_dlevel_ss = matrix(),

    # 3-D matrix for drawdown matrix at each time step
    mat_dlevel_array = NULL,

    riv_WB = list(
      In = list(
        Q_Heihe = 0,
        Q_Liyuan = 0,
        Drain_from_GW = numeric(length = no_yrs)
      ),
      Out = list(
        Irr_SW = 0,
        Drain_to_GW = numeric(length = no_yrs),
        Env_flow = numeric(length = no_yrs)
      )
    ),

    aq_WB = list(
      In = list(
        Q_bdry = 0
      ),
      Out = list(
        ET = 0,
        Q_pump = 0
      )
    )
  )

  # == Calculate boundary component ==
  f_bdry <- data.handles$f_bdry_mod / data.BS$Q_bdry_ref
  rech_bdry_ref <- data.BS$mat_wells_bdry / data.BS$area_cell
  rech_bdry_mod <- f_bdry * rech_bdry_ref

  # == Compute reference total water demand and fraction of SW usage ==
  q_sum_ref <- data.BS$irr$Q_GW_ref + data.BS$irr$Q_SW_ref
  f_SW_ref <- data.BS$irr$Q_SW_ref / q_sum_ref

  # Modulate total water demand by areal change
  f_area_mod <- data.handles$f_area / data.BS$irr$area_irr_ref
  q_sum_mod <- f_area_mod * q_sum_ref

  # Modulate total water demand by efficiency change
  f_irr_req_mod <- data.handles$f_irr_req / data.BS$irr$f_irr_req_ref
  q_sum_mod <- f_irr_req_mod * q_sum_mod

  # Modulate percentage between surface and groundwate usage
  f_SW_mod <- data.handles$f_SW_perc

  # Reintegrate modified single district data
  Q_SW_mod <- f_SW_mod * q_sum_mod
  Q_GW_mod <- (1 - f_SW_mod) * q_sum_mod

  # == Calculate pump component ==
  rech_pump <- data.BS$mat_fields

  Q_pump <- Q_GW_mod / (data.BS$area_cell * data.BS$irr$area_cells * 365)

  for (i in c(1:21)) rech_pump[data.BS$mat_fields == i] <- -Q_pump[i]

  # == Calculate irrigation return flow component ==
  rech_seep <- data.BS$mat_fields
  Q_seep <- ((data.BS$irr$f_seep_GW * Q_GW_mod + data.BS$irr$f_seep_SW * Q_SW_mod)
  / (data.BS$area_cell * data.BS$irr$area_cells * 365))

  for (i in c(1:21)) rech_seep[data.BS$mat_fields == i] <- Q_seep[i]

  # == Calculate net recharge ==
  data.RES$mat_rech_input <- rech_pump + rech_seep + rech_bdry_mod

  # == Summarize irrigation results ==
  area_new <- data.handles$f_area

  data.RES$irrigation_results <- data.frame(
    "Nr" = c(1:data.BS$n_dists),
    "AreaSim" = area_new,
    "AreaChange" = area_new - data.BS$irr$area_irr_ref,
    "SWIrr" = Q_SW_mod,
    "SWIrrChange" = Q_SW_mod - f_SW_ref * q_sum_ref,
    "GWIrr" = Q_GW_mod,
    "GWIrrChange" = Q_GW_mod - (1 - f_SW_ref) * q_sum_ref,
    check.names = F
  )

  data.RES$aq_WB$In$Q_bdry <- 365 * sum(rech_bdry_mod) * data.BS$area_cell
  data.RES$aq_WB$Out$Q_pump <- -365 * sum(rech_pump) * data.BS$area_cell

  data.RES$riv_WB$In$Q_Heihe <- data.handles$f_riv_mod
  data.RES$riv_WB$In$Q_Liyuan <- 0.14 * data.RES$riv_WB$In$Q_Heihe
  
  return(data.RES)
} # end of "fun_calc_recharge.R"