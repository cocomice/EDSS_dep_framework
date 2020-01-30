fun_preprocess <- function(inFilename) {
  eff_flood_irr <- 0.6 # water use efficiency of flooding irrigation
  eff_pipe_irr  <- 0.7 # water use efficiency of pipe irrigation
  eff_drip_irr  <- 0.9 # water use efficiency of drip irrigation

  # df <- read.csv2(inFilename,
  #   header = F, skip = 1, encoding = "UTF-8",
  #   sep = ",", stringsAsFactors = F
  # )
  
  df <- read_xls(inFilename, sheet = 1, range = "a1:m22")

  # crop water demand (i.e., here we use ET value) can be computed or
  # given by input file
  crop_waterDemand <- read.csv2("R_data/crop_wtDemand_norm.dat", header = T, sep = ",")[, 2]

  # get irrigated area [1e4 mu] per each crop. The sum of crop area may
  # not equal to the sum of area with different irrigation methods (irr_mtd_area)
  crop_area <- as.matrix(sapply(df[7:ncol(df)], as.numeric))
  Tot_cropDemand <- rowSums(crop_area) * crop_waterDemand # [1e4 m3]

  # get area with different irrigaiton methods
  irr_mtd_area <- as.matrix(sapply(df[4:6], as.numeric))
  irr_mtd_perc <- irr_mtd_area / rowSums(irr_mtd_area) # [-]

  # get surface water and groundwater allocation
  water_right <- as.matrix(sapply(df[2:3], as.numeric))
  water_use_perc <- water_right / rowSums(water_right) # [-]
  colnames(water_use_perc) <- c("SW", "GW")

  # total water demand accounting for irrigation efficiency
  Tot_waterDemand <- Tot_cropDemand * irr_mtd_perc[, 1] / eff_flood_irr +
    Tot_cropDemand * irr_mtd_perc[, 2] / eff_pipe_irr +
    Tot_cropDemand * irr_mtd_perc[, 3] / eff_drip_irr # [1e4 m3]

  # return output. here the total area is given by the sum of "irr_mtd_area"
  OutVal <- data.frame(
    Tot_watDemand = Tot_waterDemand, # [1e4 m3],
    Tot_watNorm = rowSums(water_right),
    irr_area = rowSums(irr_mtd_area) * 1e4,
    water_use_perc = water_use_perc
  )

  return(OutVal)
}