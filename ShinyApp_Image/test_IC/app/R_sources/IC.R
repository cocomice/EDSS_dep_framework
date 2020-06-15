

# Main Functions ----------------------------------------------------------

monthdays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)


IC.monthly.radiation <- function(clim, loc) {
  # based on http://www.fao.org/docrep/X0490E/x0490e07.htm#radiation
  # TODO: Check if either SUNH or SRAD is available
  days <- vector(mode = "numeric", length = 12)
  radiation <- vector(mode = "numeric", length = 12)
  for (doy in 1:365) {
    month <- doy2month(doy)
    dr <- 1 + 0.033 * cos(2 * pi / 365 * doy) ## 0.03
    j <- loc$LAT / 360 * 2 * pi
    d <- 0.409 * sin(2 * pi / 365 * doy - 1.39)
    ws <- acos(-tan(j) * tan(d))
    Ra <- 24 * 60 / pi * 0.0820 * dr * (ws * sin(j) * sin(d) + cos(j) * cos(d) * sin(ws)) # MJ/m^2/day
    if (is.null(clim$SRAD)) {
      N <- 24 / pi * ws
      as <- 0.25
      bs <- 0.5
      n <- clim$SUNH[month]
      Rs <- (as + bs * n / N) * Ra # MJ/m^2/day
    } else {
      Rs <- clim$SRAD[month]
    }
    a <- 0.23
    Rns <- (1 - a) * Rs
    Rso <- (0.75 + 2 * 10**-5 * loc$ALT) * Ra # MJ/m^2/day
    tmax <- clim$TMAX[month]
    tmin <- clim$TMIN[month]
    rh <- clim$HUM[month]
    esmax <- 0.6108 * exp(17.27 * tmax / (tmax + 237.3))
    esmin <- 0.6108 * exp(17.27 * tmin / (tmin + 237.3))
    es <- (esmax + esmin) / 2
    ea <- rh / 100 * es # kPa
    Rnl <- 4.903 * 10**-9 * ((tmax + 273.15)**4 + (tmin + 273.15)**4) / 2 * (0.34 - 0.14 * sqrt(ea)) * (1.35 * Rs / Rso - 0.35)
    Rn <- Rns - Rnl # MJ/m^2/day
    days[month] <- days[month] + 1
    radiation[month] <- radiation[month] + Rn
  }
  return(radiation / days)
}


# Calculate ET0
IC.monthly.PenmanFAO <- function(clim, loc) {
  ET0 <- vector(mode = "numeric", length = 12)
  for (month in 1:12) {
    tmax <- clim$TMAX[month]
    tmin <- clim$TMIN[month]

    mp <- month - 1
    mf <- month + 1
    if (month == 1) {
      mp <- 12
    }
    else if (month == 12) {
      mf <- 1
    }

    tmax.previous <- clim$TMAX[mp]
    tmin.previous <- clim$TMIN[mp]

    tmax.following <- clim$TMAX[mf]
    tmin.following <- clim$TMIN[mf]

    rh <- clim$HUM[month]
    u2 <- clim$WND[month] # m/s
    Rn <- clim$RAD[month]

    tmean <- (tmax + tmin) / 2 # C°
    tmean.previous <- (tmax.previous + tmin.previous) / 2
    tmean.following <- (tmax.following + tmin.following) / 2

    etmx <- 0.611 * exp((17.27 * tmax) / (tmax + 237.3))
    etmn <- 0.611 * exp((17.27 * tmin) / (tmin + 237.3))
    es <- (etmx + etmn) / 2
    ea <- rh / ((50 / etmn) + (50 / etmx))
    es.minus.ea <- es - ea


    # es.minus.ea <- (1-rh/100)*es  # kPa

    slope <- 2504 * exp((17.27 * tmean) / (tmean + 237.3)) / (tmean + 237.3)**2 # kPa/C°

    G <- 0.07 * (tmean.following - tmean.previous) # MJ/m^2/day

    P <- 101.3 * ((293 - 0.0065 * loc$ALT) / 293)**5.26

    lambda <- 2.501 - 2.361e-3 * tmean
    y <- 1.63e-3 * P / lambda

    ET0[month] <- 0.9 * (0.408 * slope * (Rn - G) + y * 900 / (tmean + 273) * u2 * (es.minus.ea)) / (slope + y * (1 + 0.34 * u2)) # mm/day
  }
  return(ET0 * monthdays)
}



IC.Kc_by_day <- function(crop, day) {
  # TODO: adjust KC by climate (wind and RH)
  # TODO: smoother interpolation
  anchor_days <- c(0, crop$DAYSINIT, crop$DAYSINIT + crop$DAYSDEVELOP - 1, crop$DAYSINIT + crop$DAYSDEVELOP + crop$DAYSMID - 1, crop$DAYSINIT + crop$DAYSDEVELOP + crop$DAYSMID + crop$DAYSLATE)
  values <- c(crop$KCINIT, crop$KCINIT, crop$KCMID, crop$KCMID, crop$KCEND)
  interp <- approxfun(anchor_days, values)
  result <- interp(day)
  if (is.na(result)) {
    return(0)
  } else {
    return(interp(day))
  }
}

IC.Zroot_by_day <- function(crop, day) {
  anchor_days <- c(0, crop$DAYSINIT + crop$DAYSDEVELOP, crop$DAYSINIT + crop$DAYSDEVELOP + crop$DAYSMID + crop$DAYSLATE)
  values <- c(crop$ZROOTINIT, crop$ZROOTMAX, crop$ZROOTMAX)
  interp <- approxfun(anchor_days, values)
  result <- interp(day)
  if (is.na(result)) {
    return(0)
  } else {
    return(interp(day))
  }
}



IC.monthly.generate_randomrainfall <- function(climate) {
  # Randomly generate rainfall within the daily amount of the previous and next month while preserving the monthly mean and number of wetdays <- floor(climate$WETD)
  prcp_monthly <- data.frame(matrix(ncol = 0, nrow = 12))
  n <- floor(climate$WETD)
  prcp_monthly$dailyamout <- climate$PRCP / n
  prcp_monthly$days <- n

  out <- data.frame(matrix(0, ncol = 1, nrow = 365))
  names(out) <- "prcp"
  for (month in 1:12) {
    monthp <- month - 1
    monthn <- month + 1
    if (month == 1) {
      monthp <- 12
    } else if (month == 12) {
      monthn <- 1
    }
    min_precip <- min(prcp_monthly$dailyamout[monthp], prcp_monthly$dailyamout[month], prcp_monthly$dailyamout[monthn])
    max_precip <- max(prcp_monthly$dailyamout[monthp], prcp_monthly$dailyamout[month], prcp_monthly$dailyamout[monthn])

    # Randomly generate rainfall events within bounds and while preserving mean.
    events <- runif(prcp_monthly$days[month], min_precip, max_precip)
    events <- events * prcp_monthly$dailyamout[month] / mean(events)

    # Randomly choose days when rainfall occurs
    firstdate <- as.Date(sprintf("2003-%02d-01", month))
    firstdoy <- unclass(as.POSIXlt(firstdate))$yday + 1
    maxdoy <- firstdoy + days_in_month(firstdate) - 1
    days <- sample(firstdoy:maxdoy, prcp_monthly$days[month], replace = FALSE)
    out[days, "prcp"] <- events
  }
  return(out)
}

IC.monthly.generate_rainfall <- function(climate, PRCP) {
  # equally generate rainfall within the daily amount of the previous and next month while preserving the monthly mean and number of wetdays <- floor(climate$WETD)

  out <- data.frame(matrix(0, ncol = 1, nrow = 365))
  names(out) <- "prcp"
  for (month in 1:12) {
    firstdate <- as.Date(sprintf("2003-%02d-01", month))
    firstdoy <- unclass(as.POSIXlt(firstdate))$yday + 1
    maxdoy <- firstdoy + days_in_month(firstdate) - 1
    monthdays <- maxdoy - firstdoy + 1
    n <- max(floor(climate$WETD[month]), 1)
    # n <- max(monthdays,1)
    if (n > monthdays) {
      n <- monthdays
    }
    events <- rep(PRCP[month] / n, n)
    days <- floor(seq(firstdoy + (monthdays / n - 1), maxdoy, length.out = n)) ## equally distributing rainfall events during month
    out[days, "prcp"] <- events
  }
  return(out)
}


IC.daily.runoff <- function(prcp) {

  # CNII <- soil$CNAMC2
  # f <- Dr/TAW
  # CNI <- -16.91+1.348*CNII-0.01379*CNII**2+0.0001172*CNII**3  ##Curve Number approach, Chapter 3 Aquacrop Version 6: Calculation procedures: http://www.fao.org/3/a-br248e.pdf
  # CNIII <- 2.5838+1.9449*CNII-0.014216*CNII**2+0.000045829*CNII**3
  #
  # wCNIII <- max(1-f*2,0) ##Interpolation of AMC classes 1 to 3. Assumes Field Capacity for CNIII,  wilting point for CNI, halfway between is CNII
  # wCNII <- min(2*f,2-2*f)
  # wCNI <- max(-1+f*2,0)
  #
  # CN <- wCNI*CNI + wCNII*CNII + wCNIII*CNIII
  # S <- 254*(100/CN-1)
  # if (prcp>0.2*S) {
  #   RO <- (prcp-0.2*S)**2/(prcp+S-0.2*S)
  # } else {
  #   RO <- 0
  # }
  # return(RO)
  return(0)
}

IC.monthly.soil_water_balance <- function(crop, soil, climate, Pt, plantingdate, irrigation_method = "threshold", irrigation_threshold = 1) {
  # irrigation_method:
  # "threshold" --> irrigation_threshold is fraction of RAW. Defined the moment when Soil Water storage (SWS) is completely refilled
  # "minimum" --> irrigation occurs at RAW=0 / irrigation_threshold=1 and covers ETc for one day. Daily irrigation an minimum SWS
  # "maximum" --> irrigation occurs everytime SWS is below 100%. Irrigation is terminated, such that at harvest SWS is depleted.
  # "monthly" --> SWS is refilled to 100% end of month. If irrigation_threshold is reached, irrigation occurs also during month.

  # for monthly climate data
  days <- seasondays(crop, plantingdate)
  df <- data.frame(matrix(ncol = 13, nrow = length(days)))
  names(df) <- c("DOY", "Kc", "Ks", "Dr", "RAW", "ETo", "ETc", "ETa", "PRCP", "PRCPeff", "DP", "RO", "Water_Deficit")
  rainfall <- IC.monthly.generate_rainfall(climate, Pt)
  Dr <- 0 # in fraction
  pdef <- crop$PDRY
  Ks <- 1
  ETc <- 0

  for (i in 1:length(days)) {
    day <- days[i]
    doy <- unclass(as.POSIXlt(day))$yday + 1
    if (doy>365) doy <- doy - 365
    month <- doy2month(doy)

    water_deficit <- 0

    # ET calculation
    ET <- climate$ET[month] / monthdays[month]
    Kc <- IC.Kc_by_day(crop, day - as.Date(plantingdate))
    ETc <- Kc * ET
    ETa <- Ks * ETc
    Dr <- Dr + ETa # water deficit after ET

    # Soil Water
    TAW <- 1000 * (soil$QFC - soil$QWP) * IC.Zroot_by_day(crop, day - as.Date(plantingdate)) # m3/m3 * m *1000mm/m= mm
    p <- pdef # min(max(pdef + 0.04*(5 - ETc),0.1),0.8)
    RAW <- p * TAW

    # rainfall event
    PRCP <- rainfall$prcp[doy]
    if (PRCP > 0) {
      RO <- IC.daily.runoff(PRCP)
    } else {
      RO <- 0
    }
    DP <- -(min(Dr - (PRCP - RO), 0))
    PRCPeff <- PRCP - RO - DP
    Dr <- Dr - PRCPeff

    # Irrigation
    if (any(irrigation_method == c("threshold", "monthly")) && Dr > irrigation_threshold * RAW) { # Minimum Methode
      water_deficit <- Dr
      Dr <- 0
    } else if (irrigation_method == "minimum" && Dr > RAW) {
      water_deficit <- Dr - RAW
      Dr <- RAW
    } else if (irrigation_method == "maximum") {
      water_deficit <- Dr
      Dr <- 0
    } else if (irrigation_method == "monthly") {
      if (month != doy2month(doy + 1) || i == length(days)) {
        water_deficit <- Dr
        Dr <- 0
      }
    }

    df[i, ] <- c(doy, Kc, Ks, Dr, RAW, ET, ETc, ETa, PRCP, PRCPeff, DP, RO, water_deficit)
  }

  return(df)
}


# Utility Functions -------------------------------------------------------

extract_climData <- function(site_id, db_con) {
  load(db_con)

  site_idx <- which(metaInfo$site_id == site_id)
  df <- data_list[[site_idx]]

  df <- df %>% mutate(month = month(date_val), year = year(date_val))

  colNames <- c("MONTH", "WETD", "PRCP", "PRCP_25", "PRCP_75", "TMAX", "TMIN", "HUM", "SUNH", "WND")
  df_out <- data.frame(matrix(ncol = length(colNames), nrow = 12))
  colnames(df_out) <- colNames
  df_out$MONTH <- seq(1, 12)

  for  (mon in seq(1, 12)) {
    idx <- which(df$month == mon)
    nrYrs <- length(unique(df$year[idx]))
    pt_tmp <- df[idx, ] %>%
      group_by(year, month) %>%
      summarise(pt = sum(pt, na.rm = T))

    df_out$WETD[mon] <- sum(df$pt[idx] > 0, na.rm = T) / nrYrs # wet days in a month
    df_out$PRCP[mon] <- quantile(pt_tmp$pt, 0.5) # median monthly precipitation
    df_out$PRCP_25[mon] <- quantile(pt_tmp$pt, 0.25) # monthly precipitation (25th quantile)
    df_out$PRCP_75[mon] <- quantile(pt_tmp$pt, 0.75) # monthly precipitation (75th quantile)
    df_out$TMAX[mon] <- mean(df$tasmax[idx], na.rm = T) # monthly max. temperature
    df_out$TMIN[mon] <- mean(df$tasmin[idx], na.rm = T) # monthly min. temperature
    df_out$HUM[mon] <- mean(df$hu[idx], na.rm = T) # monthly humidity
    df_out$SUNH[mon] <- mean(df$sun_hour[idx], na.rm = T) # monthly sunshine hour
    df_out$WND[mon] <- mean(df$wind_speed[idx], na.rm = T) # monthly wind speed
  }

  attr(df_out, "lon") <- metaInfo$lng
  attr(df_out, "lat") <- metaInfo$lat
  attr(df_out, "alt") <- metaInfo$alt

  return(df_out)
}


data.day2month <- function(dataframe) {
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  variablenames <- colnames(dataframe)[colnames(dataframe) != "DOY"]
  aggregated_data <- data.frame(matrix(0, ncol = length(variablenames) + 2, nrow = 12))
  names(aggregated_data) <- c("MONTH", "CROPDAYS", variablenames)
  days <- 0
  month <- doy2month(dataframe[1, "DOY"])
  for (type in variablenames) {
    for (i in rownames(dataframe)) {
      doy <- dataframe[i, "DOY"]
      if (doy2month(doy) != month) {
        aggregated_data[month, "CROPDAYS"] <- days
        days <- 0
      }
      month <- doy2month(doy)
      aggregated_data[month, type] <- aggregated_data[month, type] + dataframe[i, type]
      days <- days + 1
    }
  }
  aggregated_data$MONTH <- months
  for (i in variablenames) {
    if (colSums(is.na(aggregated_data))[i] > 0) {
      aggregated_data[, i] <- c(1:length(months)) * NA
    }
  }
  return(aggregated_data)
}

seasonlength <- function(crop) {
  return(crop$DAYSINIT + crop$DAYSDEVELOP + crop$DAYSMID + crop$DAYSLATE)
}

seasondays <- function(crop, plantingdate) {
  seq(as.Date(plantingdate), by = "day", length.out = seasonlength(crop))
}

doy2month <- function(doy) {
  date <- as.Date(doy, format = "%j", origin = "1.1.0") - 1
  month(date)
}

interpolate_climate <- function(vec_monthly, doy) {
  approxfun(vec_monthly)
}

mean_noNA <- function(x) {
  return(mean(x, na.rm = T))
}

sum_noNA <- function(x) {
  return(sum(x, na.rm = T))
}
