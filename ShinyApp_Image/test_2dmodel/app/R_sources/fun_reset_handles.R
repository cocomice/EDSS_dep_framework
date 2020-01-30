
fun_reset_handles <- function(data.BS, n_dist_default) {
  data.handles <- list(
    f_bdry_mod = data.BS$Q_bdry_ref,
    f_riv_mod = data.BS$riv$Q_Heihe,
    f_area = data.BS$irr$area_irr_ref,
    f_SW_perc = data.BS$irr$f_sw_perc_ref,
    f_irr_req = data.BS$irr$f_irr_req_ref,
    f_n_dist = n_dist_default,
    t_sim = data.BS$t_sim_default,
    t_steps = data.BS$t_steps_default,
    water_savePerc = 0
  )

  return(data.handles)
}