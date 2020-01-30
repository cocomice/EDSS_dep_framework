
fun_loader <- function() {
  data.BS <- list(
    NC = 156,
    NR = 129,
    t_sim_default = 365 * 10,
    t_steps_default = 10, # original is 25
    X0 = 515000,
    Y0 = 4280000,
    area_cell = 1000^2,
    n_dists = 21,

    # Path to output files
    path_head_tr_file = "output/head_tr.dat",
    # path_budget_tr_file  = "output/budget_tr.dat",

    path_head_ss_file = "output/head_ss.dat",
    path_budget_ss_file = "output/budget_ss.dat",

    # Path to input files
    path_rch_file = "model/rch6.dat",

    riv = list(
      cells_sec1 = 0,
      cells_sec2 = 0,
      cells_sec3 = 0,
      Q_Heihe = 15.8e8
    )
  )

  data.BS$x <- seq(
    data.BS$X0 + 500,
    data.BS$X0 + 500 + (data.BS$NC - 1) * 1000, 1000
  )

  data.BS$y <- seq(
    data.BS$Y0 + 500,
    data.BS$Y0 + 500 + 1000 * (data.BS$NR - 1), 1000
  )

  data.BS$irr <- list(
    f_seep_GW = 0.2,
    f_seep_SW = 0.5, # 30% from channel loss and 20% from seepage
    Q_GW_ref = NULL,
    Q_SW_ref = NULL,
    area_cells = NULL,
    area_irr_ref = NULL,
    f_sw_perc_ref = NULL,
    f_irr_req_ref = NULL
  )

  ## ==== Load input data ====

  # Load initial head
  data1 <- as.matrix(
    read.csv("model/bas6.dat",
      skip = 1424,
      header = FALSE,
      sep = ""
    )
  )
  data2 <- as.vector(t(data1))
  data3 <- matrix(data2, ncol = 129)
  data3 <- data3[1:156, ]
  data3[data3 < -999] <- NaN
  data.BS$iniHead <- t(apply(data3, 1, rev))

  # Load top elevation mask
  data1 <- read.table("R_data/mask_top_ele.dat",
    header = FALSE,
    skip = 1,
    colClasses = "double"
  )

  data2 <- data.matrix(data1, rownames.force = NA)
  data3 <- t(apply(data2, 2, rev))
  data3[data3 == 0] <- NaN
  data.BS$mat_top_el <- data3

  # Load irrigation district mask
  data1 <- read.table("R_data/mask_districts.dat",
    header = FALSE,
    skip = 1,
    colClasses = "double"
  )

  data2 <- data.matrix(data1, rownames.force = NA)
  data3 <- t(apply(data2, 2, rev))
  data.BS$mat_fields <- data3

  # Load boundary flux
  data1 <- read.table("R_data/mask_bdry.dat",
    header = FALSE,
    skip = 1,
    colClasses = "double"
  )

  data2 <- data.matrix(data1, rownames.force = NA)
  data3 <- t(apply(data2, 2, rev))
  data.BS$mat_wells_bdry <- data3


  # Load river sections
  data1 <- read.table("R_data/mask_river.dat",
    header = FALSE,
    skip = 1,
    colClasses = "double"
  )

  data2 <- data.matrix(data1, rownames.force = NA)
  data3 <- t(apply(data2, 2, rev))
  data.BS$mat_river <- data3

  data.BS$riv$cells_sec1 <- length(data.BS$mat_river[data.BS$mat_river == 1])
  data.BS$riv$cells_sec2 <- length(data.BS$mat_river[data.BS$mat_river == 2])
  data.BS$riv$cells_sec3 <- length(data.BS$mat_river[data.BS$mat_river == 3])

  # Load shape files
  data.BS$shape_irr_area_pol <- readShapePoly("R_data/Shape_irr_area/irr_area.shp")
  data.BS$shape_bdry_pol <- readShapePoly("R_data/Shape_bdry/bdry.shp")
  data.BS$shape_irr_chanels <- readShapeLines("R_data/Shape_irr_channels/irr_channels.shp")

  # Count cells per district
  for (i in c(1:21)) {
    data.BS$irr$area_cells[i] <- length(data.BS$mat_fields[data.BS$mat_fields == i])
  }

  ## ==== Calculate default boundary flux ====
  data.BS$Q_bdry_ref <- 365 * sum(data.BS$mat_wells_bdry)

  ## ==== Read reference data ====

  # load reference groundwater usage [m3/year]
  data.BS$irr$Q_GW_ref <- read.table("R_data/Q_GW_ref.dat")[[1]]

  # Load reference surface water usage [m3/year]
  data.BS$irr$Q_SW_ref <- read.table("R_data/Q_SW_ref.dat")[[1]]

  # Load reference irrigation area [mu]
  data.BS$irr$area_irr_ref <- read.table("R_data/Irr_area_ref.dat")[[1]]

  ## ==== Calculate the percentage of GW and SW usage ====

  data.BS$irr$f_sw_perc_ref <- data.BS$irr$Q_SW_ref /
    (data.BS$irr$Q_GW_ref + data.BS$irr$Q_SW_ref)

  data.BS$irr$f_irr_req_ref <- (data.BS$irr$Q_GW_ref + data.BS$irr$Q_SW_ref) /
    data.BS$irr$area_irr_ref

  return(data.BS)
} # end of "fun_loader.R"