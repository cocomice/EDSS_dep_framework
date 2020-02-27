library(shiny)
library(leaflet)
library(ggplot2)
library(maps)
library(shinyBS)
library(rsconnect)
library(mnormt)

source("BASIN_CLIMATE_AVERAGING.r")
source("CLIMATE_GENERATOR_WEB.r")

source("SCE_ALGORITHM.r")
source("ABCDE_SCE_CALIBRATE.r")
source("ABCDE_QEST.r")
source("BASIN_CLIMATE_AVERAGING.r")

# From a future version of Shiny
bindEvent <- function(eventExpr, callback, env=parent.frame(), quoted=FALSE) {
  eventFunc <- exprToFunction(eventExpr, env, quoted)

  initialized <- FALSE
  invisible(observe({
    eventVal <- eventFunc()
    if (!initialized)
      initialized <<- TRUE
    else
      isolate(callback())
  }))
}

shinyServer(function(input, output,session) {
  # THIS SHOULD HELP TRACK PROGRESS WHILE THINGS ARE RUNNING ON WEBSITE ########
  output$intense <- reactivePrint(function() {
    if(input$conditionedPanels==2){
      Sys.sleep(11)
      return("Finished")
    }else({return(NULL)})
    options(shiny.deprecation.messages=FALSE)
  })

  ######################### DAILY WEATHER GENERATOR ############################
  # Create reactive values object to store our markers, so we can show
  # their values in a table.
  values <- reactiveValues(markers = NULL)

  # Create the map; this is not the "real" map, but rather a proxy
  # object that lets us control the leaflet map on the page.
  map <- createLeafletMap(session, "map")
  bindEvent(input$map_click, function() {
    values$selectedCity <- NULL
    if (!input$addMarkerOnClick) return()
    map$addMarker(input$map_click$lat, input$map_click$lng, NULL)
    values$markers <- rbind(data.frame(lat=input$map_click$lat,
                                       long=input$map_click$lng),
                                       values$markers)
  })

  # Clear markers
  bindEvent(input$clearMarkers, function() {
    map$clearMarkers()
    values$markers <- NULL
  })

  # Makes a table of the latitude and longitude based on where
  # user clicked on the map
  output$markers <- renderTable({
    if (is.null(values$markers)) return(NULL)

    data <- values$markers
    colnames(data) <- c("Latitude", "Longitude")
    return(data)
  }, include.rownames = FALSE)

  bindEvent(input$randomLocation, function() {
    map$setView(runif(1, 29.4, 47),
                runif(1, -119, -74),
                as.integer(runif(1, 6, 9)))
  })

  ######################### WEATHER GENERATOR PLOT #############################
  output$plot_wgen <- renderPlot({
    observe({
      if (input$alert_anchor == 0) return()
      isolate({
    	  createAlert(session, "alert_anchor1", alertId="aa1",
  	  		title = "Choose Location",
  	  		content = "Welcome to ViRTUE, a vulnerability and climate risk assessment tool for water supply utilities in the Northeast U.S. To use this tool, move through the tabs (1. Choose Location -> 6. Climate Risk) and follow the instructions in each tab.",
  	  		style = "success",
  	  		dismiss = TRUE,
  	  		append = FALSE
    	  )
    	  createAlert(session, "alert_anchor2", alertId="aa2",
  	  		title = "Instructions",
  	  		content = "Choose the location of your reservoir by clicking on the box above, 'Add marker on click.' Then click on the map in the location of interest. Do not proceed to the second tab ('Generate Streamflow') until figures appear to the right, indicating the completion of this step of the analysis.",
  	  		style = "info",
  	  		dismiss = TRUE,
  	  		append = FALSE
    	  )
    	})
    })

    if (is.null(values$markers)) return(NULL)

    # Load climate data from file
    num_year <- 62 # Figure out a way to remove this eventually
    filelist <- list.files(pattern="data_")
    x <- as.numeric(substring(filelist, 6, 12))
    y <- as.numeric(substring(filelist, 14, 21))
    lata <- substring(filelist[which.min(abs(x-input$map_click$lat))], 6, 12)
    lona <- substring(filelist[which.min(abs(y-input$map_click$lng))], 14, 21)
    FILES <- filelist[intersect(which(substring(filelist,6,12) == lata),
                                which(substring(filelist,14,21) == lona))]
    climateData <- read.table(paste(FILES, sep=""))

    # climate data plots
    output$test <- renderPlot({
      if (is.null(values$markers)) return(NULL)

      clim_data <- read.table(paste(FILES,sep=""))
	    colnames(clim_data) <- c("Year", "Month", "Day", "Precip", "TMax","Tmin","TAvg")
	    ANNUAL_PRCP_VAL <- aggregate(clim_data[,4],by=list(clim_data[,2],clim_data[,1]),FUN=sum)
		  max_temp <-	aggregate(clim_data[,5],by=list(clim_data[,2],clim_data[,1]),FUN=mean)
		  min_temp <- aggregate(clim_data[,6],by=list(clim_data[,2],clim_data[,1]),FUN=mean)
    	ANNUAL_TEMP_VAL <- (max_temp+min_temp)/2

			par(mfrow=c(1,2))
			plot(ANNUAL_PRCP_VAL[,3],type="l",ylab="Avg. Monthly Precipitation (mm)",xlab="Year",col="blue",lwd=2,main="Average Monthly Precipitation (mm)",xaxt="n")
			axis(1,at=seq(1, 744,by = 12),labels=unique(clim_data[,1]))
			plot(ANNUAL_TEMP_VAL[,3],type="l",ylab="Avg. Monthly Temperature (C)",xlab="Year",col="red",lwd=2,main="Average Monthly Temperature (C)",xaxt="n")
			axis(1,at=seq(1, 744,by = 12),labels=unique(clim_data[,1]))

			# download file button
  	  output$downloadData <- downloadHandler(
	  	  filename = function() {
		      paste("climate-", Sys.Date(), input$filetype1, sep=".")
  		  },
  		  content = function(file) {
    			if (input$filetype1 == "png")
    		    png(file,height=400,width=900)
    			else
    				pdf(file,height=5,width=8)

  		    par(mfrow=c(1,2))
    			plot(ANNUAL_PRCP_VAL[,3],type="l",ylab="Avg. Monthly Precipitation (mm)",xlab="Year",col="blue",lwd=2,main="Average Monthly Precipitation (mm)",xaxt="n")
    			axis(1,at=seq(1, 744,by = 12),labels=unique(clim_data[,1]))
    			plot(ANNUAL_TEMP_VAL[,3],type="l",ylab="Avg. Monthly Temperature (C)",xlab="Year",col="red",lwd=2,main="Average Monthly Temperature (C)",xaxt="n")
    			axis(1,at=seq(1, 744,by = 12),labels=unique(clim_data[,1]))
    		  dev.off()
    		}
    	)

      return()
    })

	  # Run weather generator
	  mean_prcp_change_percent <- unique(rep(seq(0.75,1.25,by=.05),11))
	  mean_temp_change_celsius <- unique(sort(rep(seq(0,5,by=0.5),11)))
	  RUN_CLIM_GEN <- CLIMATE_GENERATOR(mean_prcp_change_percent,mean_temp_change_celsius,num_year,climateData)
  })

  ############################## HYDROLOGY MODEL ###############################
  output$plot_hydrograph <- renderPlot({
  	observe({
      if (input$info_hydro == 0) return()

      isolate({
        createAlert(session, "info_hydro1", alertId = "ih1",
        	title = "Generate Streamflow",
        	content = "This tab calibrates a rainfall-runoff model to fit the uploaded historic flows. There is also an option to run a demonstration of a water supply system in Massachusetts.",
        	style = "success",
        	dismiss = TRUE,
        	append = FALSE
        )
        createAlert(session, "info_hydro2", alertId = "ih2",
        	title = "Instructions",
        	content = "Under 'Hydrologic Data:' above you can choose to 'Upload Historic Flows' or run the tool's 'Demo.' If you upload historic flows, see instructions below for the proper formatting. Input the drainage area of your reservoir in the box provided and then click 'Choose File' to browse your computer for the text file you have created.",
        	style = "info",
        	dismiss = TRUE,
        	append = FALSE
        )
        createAlert(session, "info_hydro3", alertId = "ih3",
         	title = "Historic Flows Format",
         	content = "Flows should be uploaded as a text file with 5 columns: Date (e.g. 1/1/1950) Year (e.g. 1950) Month (e.g. 1) Day (e.g. 1) INFLOW (e.g. 2110). Flows should be uploaded in cubic feet per second (cfs). Dates must fall within the range of 1949 to 2009, and must start on January 1st (1/1/...).",
         	style = "warning",
         	dismiss = TRUE,
         	append = FALSE
        )
      })
  	})

    output$ui <- renderUI({
      if(is.null(input$input_type)) return()

      if(input$input_type == "Upload Historic Flows"){
          fileInput("file4", "Historic Flows",
                    multiple = FALSE,
                    accept = c("text", "text/plain"))
      } else if (input$input_type=="Demo") {
          # print("Demo")
      }
    })

    num_year <- 62 #Figure out a way to remove this eventually
    filelist <- list.files(pattern="data_")
    x <- as.numeric(substring(filelist,6,12))
    y <- as.numeric(substring(filelist,14,21))
    lata <- substring(filelist[which.min(abs(x-input$map_click$lat))],6,12)
    lona <- substring(filelist[which.min(abs(y-input$map_click$lng))],14,21)
    FILES <- filelist[intersect(which(substring(filelist,6,12) == lata),which(substring(filelist,14,21) == lona))]
    climateData <- read.table(paste(FILES,sep=""))

    basin_area <- input$basin
    BasinLat <- input$map_click$lat #Springfield Latitude...should be average...not high or low

    if (input$input_type == "Upload Historic Flows") {
      FLOWS_HISTORIC <- input$file4
      if(is.null(FLOWS_HISTORIC))
        return(NULL)
      FLOWS <- read.table(FLOWS_HISTORIC$datapath,header=TRUE) #Historic Flows
    } else {
      FLOWS <- read.table("WESTFIELD_FLOWS_HYDROLOGIC_MODEL_FOR_CALIBRATION.txt", header=TRUE)
    }

    CLIMATE_DATA <- BASIN_CLIMATE_AVERAGING(climateData)
    PRCP <- CLIMATE_DATA[,3]
    TMAX <- CLIMATE_DATA[,4]
    TMIN <- CLIMATE_DATA[,5]
    TEMP <- (TMAX+TMIN)/2
    Tdif <- TMAX - TMIN

    num_year <- length(unique(CLIMATE_DATA[,2]))
    days_in_month <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),num_year)
    JulianDay <- rep(c(15,46,74,105,135,166,196,227,258,288,319,349),num_year)
    cfs_to_mm_per_month <- ((304.8/1)^3)*86400*days_in_month/(basin_area*2589988110336) #THIS IS ACTUALLY cfs_to_mm_per_month, huge number is mi^2 to mm^2
    mm_per_month_to_cfs <- 1/cfs_to_mm_per_month

  	#Hargreaves Method
    Hargreaves <- function(Tavg,Tdif,BasinLat,JulianDay,days_in_month) {
	   dr <-  (1+0.033*cos(2*pi/365*JulianDay))
	   phi <- pi/180*BasinLat
	   delta <- 0.409*sin((2*pi/365*JulianDay)-1.39)
	   ws <- acos(-tan(phi)*tan(delta))
	   Rs <- ((24*60/pi)*0.082*dr*(ws*sin(phi)*sin(delta)+cos(phi)*cos(delta)*sin(ws)))*0.408
	   PET <- 0.0023*Rs*(Tavg+17.8)*sqrt(Tdif)*days_in_month
   	}
   	PE <- Hargreaves(Tavg=TEMP,Tdif=Tdif,BasinLat=BasinLat,JulianDay=JulianDay,days_in_month=days_in_month)

   	MONTHLY_FLOWS <- aggregate(FLOWS[,5],by=list(FLOWS[,3],FLOWS[,2]),FUN=mean)
   	MONTHLY_FLOWS <- subset(MONTHLY_FLOWS,MONTHLY_FLOWS[,2] <= max(CLIMATE_DATA[,2]))
   	hydro_start <- which(CLIMATE_DATA[,1]==MONTHLY_FLOWS[1,1] & CLIMATE_DATA[,2]==MONTHLY_FLOWS[1,2])
   	hydro_end <- which(CLIMATE_DATA[,1]==MONTHLY_FLOWS[dim(MONTHLY_FLOWS)[1],1] & CLIMATE_DATA[,2]==MONTHLY_FLOWS[dim(MONTHLY_FLOWS)[1],2])
   	Q_mm <- MONTHLY_FLOWS[,3]*cfs_to_mm_per_month[hydro_start:hydro_end]
   	PRCP_HYDRO <- PRCP[hydro_start:hydro_end]
   	TEMP_HYDRO <- TEMP[hydro_start:hydro_end]
   	PE_HYDRO <- PE[hydro_start:hydro_end]

   	start_par <- c(0.98,500,0.5,0.5,0.5,5,0,0,0)
   	lowerb <- c(.9,50,0,0,0,-10,0,0,0)
   	upperb <- c(.99999,950,1,1,1,10,1000,1000,1000)
   	x1 <- 1
   	x2 <- 12*5
   	result <- SCEoptim(ABCDE_SCE_CALIBRATE,start_par,P=PRCP_HYDRO[x1:x2],PE=PE_HYDRO[x1:x2],T=TEMP_HYDRO[x1:x2],Qobs=Q_mm[x1:x2],calib_num=length(x1:x2),lower=lowerb,upper=upperb)
   	FINAL_PARM <- result$par
   	PREDICTED_FLOW <- ABCDE_QEST(parameters=FINAL_PARM,P=PRCP_HYDRO,PE=PE_HYDRO,T=TEMP_HYDRO,calib_num=length(PRCP_HYDRO))
   	nash <- round(1-sum((Q_mm-PREDICTED_FLOW)^2)/sum((Q_mm-mean(Q_mm))^2),2)
   	TIME1 <- length(Q_mm)/12
   	TIME <- rep(1949:(1949+(TIME1-1)),each=12)

   	par(mfrow=c(1,2), oma=c(2,0,2,0))
   	plot(Q_mm[1:length(Q_mm)],type="l",ylab="Streamflow (mm per month)",xlab="Time (months)",xaxt = "n",cex.lab=1)
   	lines(PREDICTED_FLOW[1:length(Q_mm)],col="red",xaxt = "n")
   	axis(1, at=1:length(Q_mm),labels=TIME,tick=FALSE)
   	title(paste("Hydrograph","\nNash Sutcliffe: ",nash,sep=""))

   	log_Q <- log10(Q_mm)
   	log_pred <- log10(PREDICTED_FLOW)
   	sortt <- sort(log_Q,decreasing=TRUE)
   	m <- rank(sortt,ties.method="random")
   	EP <- 1-(m/length(log_Q))
   	plot(EP,sortt,type="l",xlab="Exceedance Probability",ylab="Streamflow (mm per month)")
   	lines(EP,sort(log_pred,decreasing=TRUE),col="red")
   	title(paste("Comparison of Flow Duration Curves"))
	  legend("topright",c("Historic","Modeled"),col=c("black","red"),lty=c(1,1),lwd=c(2.5,2.5))

   	mtext("Calibrated Model Performance",outer=TRUE,col="red",cex=2)

    output$downloadHydro <- downloadHandler(
    	filename = function() {
    		paste("hydro-", Sys.Date(), input$filetype2, sep=".")
    	},
    	content = function(file) {
    		if (input$filetype2 == "png")
    			png(file,height=400,width=900)
    		else
    			pdf(file,height=5,width=8)

       	par(mfrow=c(1,2), oma=c(2,0,2,0))
       	plot(Q_mm[1:length(Q_mm)],type="l",ylab="Streamflow (mm per month)",xlab="Time (months)",xaxt = "n",cex.lab=1)
       	lines(PREDICTED_FLOW[1:length(Q_mm)],col="red",xaxt = "n")
       	axis(1, at=1:length(Q_mm),labels=TIME,tick=FALSE)
       	title(paste("Hydrograph","\nNash Sutcliffe: ",nash,sep=""))

       	plot(EP,sortt,type="l",xlab="Exceedance Probability",ylab="Streamflow (mm per month)")
       	lines(EP,sort(log_pred,decreasing=TRUE),col="red")
       	title(paste("Comparison of Flow Duration Curves"))
    	  legend("topright",c("Historic","Modeled"),col=c("black","red"),lty=c(1,1),lwd=c(2.5,2.5))

       	mtext("Calibrated Model Performance",outer=TRUE,col="red",cex=2)
       	dev.off()
    	}
    )

   	PREDICTED_FLOW_FINAL <- ABCDE_QEST(parameters=FINAL_PARM,P=PRCP,PE=PE,T=TEMP,calib_num=length(PRCP))

  	########################## CREATE FINAL INFLOW SCENARIOS ###################

  	prcp <- rep(seq(0.75,1.25,by=0.05),11)
  	temp <- sort(rep(seq(0,5,by=0.5),11))
  	total_number_changes <- length(prcp)

  	for (i in 1:total_number_changes){
  		k <- paste("PRCP_MEAN_CHANGES_",prcp[i],"_TEMP_CHANGES_",temp[i],sep="")
  		TEMP <- read.table(k)[,4]
  		PRCP <- read.table(k)[,3]

  		num_year <- length(Q_mm)/12
  		days_in_month <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),num_year)
  		JulianDay <- rep(c(15,46,74,105,135,166,196,227,258,288,319,349),num_year)

  		#Hargreaves Method
  		PE <- Hargreaves(Tavg=TEMP,Tdif=Tdif,BasinLat=BasinLat,JulianDay=JulianDay,days_in_month=days_in_month)
  		PREDICTED_FLOW_FINAL <- ABCDE_QEST(parameters=FINAL_PARM,P=PRCP,PE=PE,T=TEMP,calib_num=length(PRCP))
  		kk <- paste("TEMP_",temp[i],"_PRCP_",prcp[i],sep="")
  		flow_writeout <- paste("INFLOWS_",kk,".txt",sep="")
  		pp <- paste(flow_writeout,sep="")
  		write.table(PREDICTED_FLOW_FINAL,pp)
  	}
  })

  ############################### SIMULATION MODEL #############################
  output$plot <- renderPlot({
  	observe({
  	 if (input$info_sys == 0)
  	 	 return()

  	 isolate({
  		  createAlert(session, "info_sys1",alertId="is1",
		  		title = "Water Supply Performance",
		  		content= "In tabs 1 & 2, time series of precipitation and temperature were used to drive a hydrologic model which generated sequences of streamflow. These streamflow sequences are now used to explore water supply performance.",
		  		style = "success",
		  		dismiss=TRUE,
		  		append=FALSE
  		  )
  		  createAlert(session, "info_sys2",alertId="is2",
		  		title = "Instructions",
		  		content = "Under 'Water Supply Demands:' above you can choose to 'Upload Water Supply Demands' or run the tool's 'Demo.' If you upload water supply demands, see instructions below for the proper formatting. Then click 'Choose File' to browse your computer for the text file you have created.",
		  		style = "info",
		  		dismiss=TRUE,
		  		append=FALSE
  		  )
  		  createAlert(session, "info_sys3",alertId="is3",
		   		title = "Water Supply Demands Format",
		   		content = "Water supply demands should be uploaded as a text file with 3 columns: MONTH (e.g. 1) DAY (e.g. 1) WITHDRAWAL (e.g. 36). Demands should be uploaded in millions of gallons per day (mgd). Daily demands for 1 full year (except month/day 2/29) should be included in the text file.",
		   		style = "warning",
		   		dismiss=TRUE,
		   		append=FALSE
  		  )
        createAlert(session, "info_sys4",alertId="is4",
          title = "Hedge",
          content = "Clicking the 'Hedge' box introduces a hedging rule, designed to prevent a large, single shortage event from occurring by only releasing a percentage of total water supply demands when storages drop below specified thresholds. Storage thresholds differ throughout the year and are currently modeled after the Springfield Water and Sewer Commission's Drought Management Plan.",
          style = "warning",
          dismiss=TRUE,
          append=FALSE
        )
  	 	})
  	})

    output$ui2 <- renderUI({
      if(is.null(input$input_type2))
        return()

      if(input$input_type2 == "Upload Water Supply Demands") {
        fileInput("file2", "Water Supply Demands",
                  multiple = FALSE,
                  accept = c("text", "text/plain"))
      } else if (input$input_type2 == "Demo") {
        # print("Demo")
      }
    })

    if (input$input_type2 == "Upload Water Supply Demands") {
      WS <- input$file2
      if(is.null(WS))
        return(NULL)
      num_year <- 62
      WATER_SUPPLY <- read.table(WS$datapath,header=TRUE)
      WATER_SUPPLY_MONTHLY <- rep((aggregate(WATER_SUPPLY[,3],by=list(WATER_SUPPLY[,1]),FUN=sum))[,2],num_year)
      WATER_SUPPLY <- WATER_SUPPLY_MONTHLY
    } else {
      num_year <- 62 #Come back and fix this to make more general...
      WATER_SUPPLY <- read.table("WATER_SUPPLY_DEMAND_SPRINGFIELD.txt",header=TRUE)
      WATER_SUPPLY_MONTHLY <- rep((aggregate(WATER_SUPPLY[,3],by=list(WATER_SUPPLY[,1]),FUN=sum))[,2],num_year)
      WATER_SUPPLY <- WATER_SUPPLY_MONTHLY
    }

    TEMPP <- as.numeric(input$temperature)
    PRCPP <- as.numeric(input$precipitation)

    filename <- paste("INFLOWS_TEMP_",TEMPP,"_PRCP_",PRCPP,".txt",sep="")
    filename_BASE <- paste("INFLOWS_TEMP_",0,"_PRCP_",1,".txt",sep="")

    INFLOW_SIM <- read.table(paste(filename,sep=""),header=TRUE)
    INFLOW_BASE <- read.table(paste(filename_BASE,sep=""),header=TRUE)

    ############################ SIMULATION MODEL ##############################
    STORAGE_CAPACITY <- input$cc    # Capacity of reservoir...this can change
    total_number_changes <- length(list.files(pattern="INFLOWS_")) #number of climate changes...this can change

    START_YEAR_SIM <- 2014
    END_YEAR_SIM <- START_YEAR_SIM + num_year - 1
    DATE_SIM <- seq(as.Date(paste(START_YEAR_SIM,"-01-01",sep="")),as.Date(paste(END_YEAR_SIM,"-12-01",sep="")),by="month")
    MONTH_SIM <- as.numeric(format(DATE_SIM,"%m"))
    YEAR_SIM <- as.numeric(format(DATE_SIM,"%Y"))
    days_in_month <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),num_year)

    INFLOWS <- INFLOW_SIM[,1]*(2.59*10^12)*(input$da)*(2.642*10^-7)*(10^-6) #(mm/month)*(mm^2/miles^2)*(DA of reservoir)*(gallons/mm^3)*(MG/gallons)
    INFLOWS_BASE <- INFLOW_BASE[,1]*(2.59*10^12)*(input$da)*(2.642*10^-7)*(10^-6) #(mm/month)*(mm^2/miles^2)*(DA of reservoir)*(gallons/mm^3)*(MG/gallons)
    sim_length <- length(INFLOWS)

    MINIMUM_FLOW <- rep(0,sim_length) #Minimum flows for the system...this can change

    #TO INTERACT WITH SLIDER BARS
    WATER_SUPPLY_SIM <- (1+input$demand)*WATER_SUPPLY
    CAPACITY_SIM <- (1+(input$additional_storage))*input$cc
    MINIMUM_FLOW_SIM <- MINIMUM_FLOW+input$additional_minimum_flow

    if (input$Standard == FALSE) {
      #RUN SIMULATION
      RESERVOIR_STORAGE <- array(0,c(sim_length))			# Reservoir Storage (mg)
      RELEASES <- array(0,c(sim_length))					    # Reservoir Release (mg)
      SHORTFALL <- array(NA, c(sim_length))
      SHORTFALL_MAGNITUDE <- array(NA, c(sim_length))
      SHORTFALL_MAGNITUDE_PERC <- array(NA, c(sim_length))

      RESERVOIR_STORAGE[1] <- 0.85*input$cc
      SHORTFALL[1] <- 0
      SHORTFALL_MAGNITUDE[1] <- 0
      SHORTFALL_MAGNITUDE_PERC[1] <- 0

      for (i in 2:sim_length){
        if (RESERVOIR_STORAGE[i-1]+INFLOWS[i] < WATER_SUPPLY_SIM[i] + MINIMUM_FLOW_SIM[i]) {
          RELEASES[i] <- RESERVOIR_STORAGE[i-1]+INFLOWS[i]
          SHORTFALL[i] <- 1
          SHORTFALL_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES[i]
        } else if (STORAGE_CAPACITY > RESERVOIR_STORAGE[i-1]+INFLOWS[i]-WATER_SUPPLY_SIM[i]+ MINIMUM_FLOW_SIM[i]) {
          RELEASES[i] <- WATER_SUPPLY_SIM[i]+ MINIMUM_FLOW_SIM[i]
          SHORTFALL[i] <- 0
          SHORTFALL_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES[i]
        } else {
          RELEASES[i] <- max((RESERVOIR_STORAGE[i-1] + INFLOWS[i] - STORAGE_CAPACITY),(WATER_SUPPLY_SIM[i]+ MINIMUM_FLOW_SIM[i]))
          SHORTFALL[i] <- 0
          SHORTFALL_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES[i]
        }
        RESERVOIR_STORAGE[i] <- RESERVOIR_STORAGE[i-1] + INFLOWS[i] - RELEASES[i] #Calculate Storage
      }

      #PERFORMANCE METRICS#
      POR_RELIABILITY <- 1 - sum(SHORTFALL)/sim_length

      #RUN SIMULATION BASE
      RESERVOIR_STORAGE_BASE <- array(0,c(sim_length))				#Reservoir Storage (mg)
      RELEASES_BASE <- array(0,c(sim_length))					    #Reservoir Release (mg)
      SHORTFALL_BASE <- array(NA, c(sim_length))
      SHORTFALL_BASE_MAGNITUDE <- array(NA, c(sim_length))
      SHORTFALL_BASE_MAGNITUDE_PERC <- array(NA, c(sim_length))

      RESERVOIR_STORAGE_BASE[1] <- 0.85*input$cc
      SHORTFALL_BASE[1] <- 0
      SHORTFALL_BASE_MAGNITUDE[1] <- 0
      SHORTFALL_BASE_MAGNITUDE_PERC[1] <- 0

      for (i in 2:sim_length){
        if (RESERVOIR_STORAGE_BASE[i-1]+INFLOWS_BASE[i] < WATER_SUPPLY_SIM[i] + MINIMUM_FLOW_SIM[i]) {
          RELEASES_BASE[i] <- RESERVOIR_STORAGE_BASE[i-1]+INFLOWS_BASE[i]
          SHORTFALL_BASE[i] <- 1
          SHORTFALL_BASE_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES_BASE[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_BASE_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES_BASE[i]
        } else if (STORAGE_CAPACITY > RESERVOIR_STORAGE_BASE[i-1]+INFLOWS_BASE[i]-WATER_SUPPLY_SIM[i]+ MINIMUM_FLOW_SIM[i]) {
          RELEASES_BASE[i] <- WATER_SUPPLY_SIM[i]+ MINIMUM_FLOW_SIM[i]
          SHORTFALL_BASE[i] <- 0
          SHORTFALL_BASE_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES_BASE[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_BASE_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES_BASE[i]
        } else {
          RELEASES_BASE[i] <- max((RESERVOIR_STORAGE_BASE[i-1] + INFLOWS_BASE[i] - STORAGE_CAPACITY),(WATER_SUPPLY_SIM[i]+ MINIMUM_FLOW_SIM[i]))
          SHORTFALL_BASE[i] <- 0
          SHORTFALL_BASE_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES_BASE[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_BASE_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES_BASE[i]
        }
        RESERVOIR_STORAGE_BASE[i] <- RESERVOIR_STORAGE_BASE[i-1] + INFLOWS_BASE[i] - RELEASES_BASE[i] #Calculate Storage
      }

      #PERFORMANCE METRICS#
      POR_RELIABILITY_BASE <- 1 - sum(SHORTFALL_BASE)/sim_length

    } else {
      #HEDGE
      isolate({source("Hedge.r",local=TRUE)})
      HEDGE <- Hedge(num_year)

      STOR_NORM <- HEDGE[[1]]
      STOR_MOD <- HEDGE[[2]]

      ################################################################################################################
      C1 <- 3179
      C2 <- 6358
      C3 <- 15895

      RESERVOIR_STORAGE <- array(0,c(sim_length))				#Cobble Mountain Storage (mg)
      RELEASES <- array(0,c(sim_length))					    #Cobble Mountain Release (mg); Just for water supply
      SPILL <- array(0,c(sim_length))				            #Cobble Mountain Spill (mg);

      SHORTFALL <- array(NA, c(sim_length))
      SHORTFALL_MAGNITUDE <- array(NA, c(sim_length))
      SHORTFALL_MAGNITUDE_PERC <- array(NA, c(sim_length))
      SHORTFALL_COST <- array(NA, c(sim_length))

      #initial conditions
      RESERVOIR_STORAGE[1] <- 0.85*STORAGE_CAPACITY
      SHORTFALL[1] <- 0
      SHORTFALL_MAGNITUDE[1] <- 0
      SHORTFALL_MAGNITUDE_PERC[1] <- 0
      SHORTFALL_COST[1] <- 0

      #start simulation
      for (i in 2:sim_length){

        #Now simulate Cobble Mountain release and, if applicable, shortfall costs
        RELEASES[i] <- WATER_SUPPLY_SIM[i]
        if (RESERVOIR_STORAGE[i-1]>=STOR_NORM[i-1]) {
          RELEASES[i] <- WATER_SUPPLY_SIM[i]
          SHORTFALL[i] <- 0
          SHORTFALL_COST[i] <- 0
          SHORTFALL_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES[i]
        } else if (RESERVOIR_STORAGE[i-1] < STOR_NORM[i-1] & RESERVOIR_STORAGE[i-1]>=STOR_MOD[i-1]) {
          RELEASES[i] <- .95*WATER_SUPPLY_SIM[i]
          SHORTFALL[i] <- 0
          SHORTFALL_COST[i] <- C1*3
          SHORTFALL_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES[i]
        } else if (RESERVOIR_STORAGE[i-1]<STOR_MOD[i-1] & (RESERVOIR_STORAGE[i-1] + INFLOWS[i]) >= (WATER_SUPPLY_SIM[i]-6)) {
          RELEASES[i] <- .85*WATER_SUPPLY_SIM[i]
          SHORTFALL[i] <- 1
          SHORTFALL_COST[i] <- C1*3 + C2*3
          SHORTFALL_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES[i]
        } else {
          RELEASES[i] <- RESERVOIR_STORAGE[i-1] + INFLOWS[i]
          SHORTFALL[i] <- 1
          SHORTFALL_COST[i] <- C1*3 + C2*3 + C3*((WATER_SUPPLY_SIM[i]-6)- RELEASES[i])
          SHORTFALL_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES[i]
        }

        #define spill
        if (RESERVOIR_STORAGE[i-1] + INFLOWS[i] - RELEASES[i] > STORAGE_CAPACITY){
          SPILL[i] <- (RESERVOIR_STORAGE[i-1] + INFLOWS[i] - RELEASES[i])- STORAGE_CAPACITY
        } else {
          SPILL[i] <- 0
        }

        #calculate storage
        RESERVOIR_STORAGE[i] <- RESERVOIR_STORAGE[i-1] - RELEASES[i] + INFLOWS[i] - SPILL[i]

      }

      POR_RELIABILITY <- 1 - sum(SHORTFALL)/sim_length
      TOTAL_RELEASES <- RELEASES + SPILL

      ##HEDGE FOR BASE CASE
      RESERVOIR_STORAGE_BASE <- array(0,c(sim_length))				#Cobble Mountain Storage (mg)
      RELEASES_BASE <- array(0,c(sim_length))					    #Cobble Mountain Release (mg); Just for water supply
      SPILL_BASE <- array(0,c(sim_length))				            #Cobble Mountain Spill (mg);

      SHORTFALL_BASE <- array(NA, c(sim_length))
      SHORTFALL_BASE_MAGNITUDE <- array(NA, c(sim_length))
      SHORTFALL_BASE_MAGNITUDE_PERC <- array(NA, c(sim_length))
      SHORTFALL_COST_BASE <- array(NA, c(sim_length))

      #initial conditions
      RESERVOIR_STORAGE_BASE[1] <- 0.85*STORAGE_CAPACITY
      SHORTFALL_BASE[1] <- 0
      SHORTFALL_BASE_MAGNITUDE[1] <- 0
      SHORTFALL_BASE_MAGNITUDE_PERC[1] <- 0
      SHORTFALL_COST_BASE[1] <- 0

      #start simulation
      for (i in 2:sim_length){

        #Now simulate Cobble Mountain release and, if applicable, shortfall costs
        RELEASES_BASE[i] <- WATER_SUPPLY_SIM[i]
        if (RESERVOIR_STORAGE_BASE[i-1]>=STOR_NORM[i-1]) {
          RELEASES_BASE[i] <- WATER_SUPPLY_SIM[i]
          SHORTFALL_BASE[i] <- 0
          SHORTFALL_COST_BASE[i] <- 0
          SHORTFALL_BASE_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES_BASE[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_BASE_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES_BASE[i]
        } else if (RESERVOIR_STORAGE_BASE[i-1] < STOR_NORM[i-1] & RESERVOIR_STORAGE_BASE[i-1]>=STOR_MOD[i-1]) {
          RELEASES_BASE[i] <- .9*WATER_SUPPLY_SIM[i]
          SHORTFALL_BASE[i] <- 0
          SHORTFALL_COST_BASE[i] <- C1*3
          SHORTFALL_BASE_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES_BASE[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_BASE_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES_BASE[i]
        } else if (RESERVOIR_STORAGE_BASE[i-1]<STOR_MOD[i-1] & (RESERVOIR_STORAGE_BASE[i-1] + INFLOWS_BASE[i]) >= (WATER_SUPPLY_SIM[i]-6)) {
          RELEASES_BASE[i] <- .8*WATER_SUPPLY_SIM[i]
          SHORTFALL_BASE[i] <- 1
          SHORTFALL_COST_BASE[i] <- C1*3 + C2*3
          SHORTFALL_BASE_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES_BASE[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_BASE_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES_BASE[i]
        } else {
          RELEASES_BASE[i] <- RESERVOIR_STORAGE_BASE[i-1] + INFLOWS_BASE[i]
          SHORTFALL_BASE[i] <- 1
          SHORTFALL_COST_BASE[i] <- C1*3 + C2*3 + C3*((WATER_SUPPLY_SIM[i]-6)- RELEASES_BASE[i])
          SHORTFALL_BASE_MAGNITUDE_PERC[i] <- (WATER_SUPPLY_SIM[i] - RELEASES_BASE[i])/WATER_SUPPLY_SIM[i]
          SHORTFALL_BASE_MAGNITUDE[i] <- WATER_SUPPLY_SIM[i] - RELEASES_BASE[i]
        }

        #define spill
        if (RESERVOIR_STORAGE_BASE[i-1] + INFLOWS_BASE[i] - RELEASES_BASE[i] > STORAGE_CAPACITY){
          SPILL_BASE[i] <- (RESERVOIR_STORAGE_BASE[i-1] + INFLOWS_BASE[i] - RELEASES_BASE[i])- STORAGE_CAPACITY
        } else {
          SPILL_BASE[i] <- 0
        }

        #calculate storage
        RESERVOIR_STORAGE_BASE[i] <- RESERVOIR_STORAGE_BASE[i-1] - RELEASES_BASE[i] + INFLOWS_BASE[i] - SPILL_BASE[i]

      }

      POR_RELIABILITY_BASE <- 1 - sum(SHORTFALL_BASE)/sim_length
      TOTAL_RELEASES_BASE <- RELEASES_BASE + SPILL_BASE

    }

    ######################## COMPUTING MAXIMUM AND MINIMUM FLOWS ###############
    prcp <- rep(seq(0.75,1.25,by=0.05),11)
    temp <- sort(rep(seq(0,5,by=0.5),11))
    total_number_changes <- length(prcp)

    MAX_FLOW <- array(0,12)
    MIN_FLOW <- array(10^10,12)
    for (i in 1:total_number_changes) {
      cur_flow_name <- paste("INFLOWS_TEMP_",temp[i],"_PRCP_",prcp[i],".txt",sep="")
      READ_IN <- paste(cur_flow_name,sep="")
      CUR_FLOW <- read.table(READ_IN,header=TRUE)[,1]
      CUR_MONTHLY_FLOWS <- aggregate(CUR_FLOW,FUN=mean,by=list(MONTH_SIM))[,2]
      for (j in 1:12) {
        MAX_FLOW[j] <- max(MAX_FLOW[j],CUR_MONTHLY_FLOWS[j])
        MIN_FLOW[j] <- min(MIN_FLOW[j],CUR_MONTHLY_FLOWS[j])
      }
    }
    MAX_FLOW <- MAX_FLOW*(2.59*10^12)*(input$da)*(2.642*10^-7)*(10^-6) #(mm/month)*(mm^2/miles^2)*(DA of reservoir)*(gallons/mm^3)*(MG/gallons)
    MIN_FLOW <- MIN_FLOW*(2.59*10^12)*(input$da)*(2.642*10^-7)*(10^-6) #(mm/month)*(mm^2/miles^2)*(DA of reservoir)*(gallons/mm^3)*(MG/gallons)
    ########################## COMPUTING ANNUAL VALUES #########################
    ANNUAL_SHORTFALL <- matrix(SHORTFALL, ncol=num_year, byrow=FALSE)
    ANNUAL_RELEASES <- matrix(RELEASES, ncol=num_year, byrow=FALSE)
    ANNUAL_STORAGE <- matrix(RESERVOIR_STORAGE, ncol=num_year, byrow=FALSE)
    ANNUAL_STORAGE_BASE <- matrix(RESERVOIR_STORAGE_BASE, ncol=num_year, byrow=FALSE)
    ANNUAL_DEMAND <- matrix(WATER_SUPPLY_SIM, ncol=num_year, byrow=FALSE)
    MEAN_ANNUAL_RELEASES <- array(NA,c(num_year))
    MEAN_ANNUAL_DEMAND <- array(NA,c(num_year))
#    CUMULATIVE_ANNUAL_SHORTFALL <- array(NA,c(num_year))
    MINIMUM_ANNUAL_STORAGE <- array(NA,c(num_year))
    MINIMUM_ANNUAL_STORAGE_BASE <- array(NA,c(num_year))
#    ANNUAL_SHORTFALL_TOTAL <- array(NA,c(num_year))
    PERCENT_OF_CAPACITY_ANNUAL_STORAGE <- array(NA,c(num_year))
    PERCENT_OF_CAPACITY_ANNUAL_STORAGE_BASE <- array(NA,c(num_year))

    for (i in 1:num_year){
      MEAN_ANNUAL_DEMAND[i] <- mean(ANNUAL_DEMAND[,i])
      MEAN_ANNUAL_RELEASES[i] <- mean(ANNUAL_RELEASES[,i])
      MINIMUM_ANNUAL_STORAGE[i] <- min(ANNUAL_STORAGE[,i])
      MINIMUM_ANNUAL_STORAGE_BASE[i] <- min(ANNUAL_STORAGE_BASE[,i])
      PERCENT_OF_CAPACITY_ANNUAL_STORAGE[i] <- ((min(ANNUAL_STORAGE[,i]))/CAPACITY_SIM)*100
      PERCENT_OF_CAPACITY_ANNUAL_STORAGE_BASE[i] <- ((min(ANNUAL_STORAGE_BASE[,i]))/CAPACITY_SIM)*100
    }

    #EXTENDED MONTHLY ANALYSIS
    PRESENT_MEAN_MONTHLY_FLOW <- aggregate(INFLOWS,by=list(MONTH_SIM),FUN=mean)
    HIST_MEAN_MONTHLY_FLOW <- aggregate(INFLOWS_BASE,by=list(MONTH_SIM),FUN=mean)

    t_change <- cbind(YEAR_SIM,MONTH_SIM,INFLOWS)
    CHANGING_ANNUAL_MONTHLY_STORAGE <- aggregate(t_change[,3],by=list(t_change[,2]),FUN=mean,na.rm=TRUE)
    FOR_PLOTTING_FLOWS <- t_change[which(t_change[,2]==input$storage_month),3]

    p_change <- cbind(YEAR_SIM,MONTH_SIM,INFLOWS_BASE)
    MONTHLY_CUR_STORAGE <- aggregate(p_change[,3],by=list(p_change[,2]),FUN=mean,na.rm=TRUE)
    FOR_PLOTTING_FLOWS_BASE <- p_change[which(p_change[,2]==input$storage_month),3]

    tt_change <- cbind(YEAR_SIM,MONTH_SIM,RESERVOIR_STORAGE)
    CHANGING_ANNUAL_MONTHLY_STORAGE <- aggregate(tt_change[,3],by=list(tt_change[,2]),FUN=mean,na.rm=TRUE)
    FOR_PLOTTING_STORAGES <- tt_change[which(tt_change[,2]==input$storage_month),3]

    pp_change <- cbind(YEAR_SIM,MONTH_SIM,RESERVOIR_STORAGE_BASE)
    MONTHLY_CUR_STORAGE <- aggregate(pp_change[,3],by=list(pp_change[,2]),FUN=mean,na.rm=TRUE)
    FOR_PLOTTING_STORAGE_BASE <- pp_change[which(pp_change[,2]==input$storage_month),3]

    YEARS <- unique(YEAR_SIM)
    MONTH_LIST <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

    par(mfrow=c(3,2))
    dial <- function(x, min=0, max=1){
      old.par <- par(pty="s", lend=1)
      on.exit(par(old.par))
      plot(0,0, pch=16, cex=5, ann=FALSE, xaxt="n", yaxt="n", xlab="",
           ylab="",xlim=c(-1,1), ylim=c(-1,1) )
      title(paste("WATER SUPPLY RELIABILITY\n",round(POR_RELIABILITY,2)),cex.main=2)
      tmp.theta <- seq( -1/3*pi, 4/3*pi, length=11 )
      segments( 0.9 * cos(tmp.theta), 0.9 * sin(tmp.theta),
                cos(tmp.theta), sin(tmp.theta) )
      tmp.theta <- seq(4/3*pi, 3/4*pi, length=100)
      lines( cos(tmp.theta), sin(tmp.theta), col="red", lwd=6 )
      tmp.theta <- seq(3/4*pi, -1/3*pi, length=100)
      lines( cos(tmp.theta), sin(tmp.theta), col="yellow", lwd=6 )
      tmp.theta <- seq(((((1-input$rel)*300)*0.0174532925)+5/3)*pi, 5/3*pi, length=100) #Conversion from degrees to radians = 0.0174532925
      lines( cos(tmp.theta), sin(tmp.theta), col="green", lwd=6 )
      tmp.theta <- seq(0,2*pi, length=360)
      lines( 1.025*cos(tmp.theta), 1.025*sin(tmp.theta) )
      tmp.theta <- (max-x)/(max-min) * 5/3*pi - pi/3
      polygon( c(.05, 1, 0.05) * cos( tmp.theta + c(-pi/2,0,pi/2) ),
               c(0.05, 1, 0.05) * sin( tmp.theta + c(-pi/2,0,pi/2) ),
               col="black")
    }
    dial(as.numeric(substr(toString(POR_RELIABILITY),1,4)))

    plot(YEARS,PERCENT_OF_CAPACITY_ANNUAL_STORAGE,type="l",col="red",lwd=3,ylab="% Capacity (Annual Storage)",main=" STORAGE: % CAPACITY",ylim=c(0,100),cex.main=2,cex.axis=1.5,cex.lab=1.5)
    lines(YEARS,PERCENT_OF_CAPACITY_ANNUAL_STORAGE_BASE,lwd=3,col="black")
    legend("bottomleft",c("BASE","CC"),lwd=c(2,2),lty=c(1,1),pch=c(-1,-1),col=c("black","red"))

    tt <- paste(MONTH_LIST[input$storage_month],"STORAGE=",round(CHANGING_ANNUAL_MONTHLY_STORAGE[input$storage_month,2],2),"\nBASE=",round(MONTHLY_CUR_STORAGE[input$storage_month,2],2),sep=" ")
    plot(YEARS,FOR_PLOTTING_STORAGES,type="l",ylab="STORAGE (MG)",col="red",lwd=3,main=tt,ylim=c(0,input$cc),cex.main=2,cex.axis=1.5,cex.lab=1.5)
    lines(YEARS,FOR_PLOTTING_STORAGE_BASE,lwd=3,col="black")
    legend("bottomleft",c("BASE","CC"),lwd=c(2,2),lty=c(1,1),pch=c(-1,-1),col=c("black","red"))

    plot(HIST_MEAN_MONTHLY_FLOW[,2],type="l",col="black",lwd=5,xlab="MONTH",ylab="INFLOW (MGM)",ylim=c(min(MIN_FLOW),max(MAX_FLOW)),main="AVERAGE MONTHLY INFLOW",cex.main=2,cex.axis=1.5,cex.lab=1.5) #THE FOLLOWING LINES PLOT MONTHLY VALUES
    polygon(c(1:12,rev(1:12)),c(MIN_FLOW,rev(MAX_FLOW)),col="grey")
    lines(PRESENT_MEAN_MONTHLY_FLOW[,2],col="blue",lwd=5)
    lines(HIST_MEAN_MONTHLY_FLOW[,2],col="black",lwd=2)

    output$downloadSim <- downloadHandler(
		  filename = function() {
		    paste("system-", Sys.Date(), input$filetype3, sep=".")
		  },
      content = function(file) {
  			if (input$filetype3 == "png")
  		    png(file,height=900,width=900)
  			else
  				pdf(file,height=7,width=7)

        num_year <- 62
        START_YEAR_SIM <- 2014
        END_YEAR_SIM <- START_YEAR_SIM + num_year - 1
        DATE_SIM <- seq(as.Date(paste(START_YEAR_SIM,"-01-01",sep="")),as.Date(paste(END_YEAR_SIM,"-12-01",sep="")),by="month")
        MONTH_SIM <- as.numeric(format(DATE_SIM,"%m"))
        YEAR_SIM <- as.numeric(format(DATE_SIM,"%Y"))
        days_in_month <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),num_year)

  		  par(mfrow=c(3,2))
  		  dial <- function(x, min=0, max=1) {
  	      old.par <- par(pty="s", lend=1)
  	      on.exit(par(old.par))
  	      plot(0,0, pch=16, cex=3, ann=FALSE, xaxt="n", yaxt="n", xlab="",
  	           ylab="",xlim=c(-1,1), ylim=c(-1,1) )
  	      title(paste("WATER SUPPLY RELIABILITY\n",round(POR_RELIABILITY,2)),cex.main=1.5)
  	      tmp.theta <- seq( -1/3*pi, 4/3*pi, length=11 )
  	      segments( 0.9 * cos(tmp.theta), 0.9 * sin(tmp.theta),
  	                cos(tmp.theta), sin(tmp.theta) )
  	      tmp.theta <- seq(4/3*pi, 3/4*pi, length=100)
  	      lines( cos(tmp.theta), sin(tmp.theta), col="red", lwd=6 )
  	      tmp.theta <- seq(3/4*pi, -1/3*pi, length=100)
  	      lines( cos(tmp.theta), sin(tmp.theta), col="yellow", lwd=6 )
  	      tmp.theta <- seq(((((1-input$rel)*300)*0.0174532925)+5/3)*pi, 5/3*pi, length=100) #Conversion from degrees to radians = 0.0174532925
  	      lines( cos(tmp.theta), sin(tmp.theta), col="green", lwd=6 )
  	      tmp.theta <- seq(0,2*pi, length=360)
  	      lines( 1.025*cos(tmp.theta), 1.025*sin(tmp.theta) )
  	      tmp.theta <- (max-x)/(max-min) * 5/3*pi - pi/3
  	      polygon( c(.05, 1, 0.05) * cos( tmp.theta + c(-pi/2,0,pi/2) ),
  	               c(0.05, 1, 0.05) * sin( tmp.theta + c(-pi/2,0,pi/2) ),
  	               col="black")
  	    }
  	    dial(as.numeric(substr(toString(POR_RELIABILITY),1,4)))

        plot(YEARS,PERCENT_OF_CAPACITY_ANNUAL_STORAGE,type="l",col="red",lwd=3,ylab="% Capacity (Annual Storage)",main=" STORAGE: % CAPACITY",ylim=c(0,100),cex.main=1.5,cex.axis=1.5,cex.lab=1)
  	    lines(YEARS,PERCENT_OF_CAPACITY_ANNUAL_STORAGE_BASE,lwd=3,col="black")
  	    legend("bottomleft",c("BASE","CC"),lwd=c(2,2),lty=c(1,1),pch=c(-1,-1),col=c("black","red"))

  	    tt <- paste(MONTH_LIST[input$storage_month],"STORAGE=",round(CHANGING_ANNUAL_MONTHLY_STORAGE[input$storage_month,2],2),"\nBASE=",round(MONTHLY_CUR_STORAGE[input$storage_month,2],2),sep=" ")
  	    plot(YEARS,FOR_PLOTTING_STORAGES,type="l",ylab="STORAGE (MG)",col="red",lwd=3,main=tt,ylim=c(0,input$cc),cex.main=1.5,cex.axis=1.5,cex.lab=1)
  	    lines(YEARS,FOR_PLOTTING_STORAGE_BASE,lwd=3,col="black")
  	    legend("bottomleft",c("BASE","CC"),lwd=c(2,2),lty=c(1,1),pch=c(-1,-1),col=c("black","red"))

  	    plot(HIST_MEAN_MONTHLY_FLOW[,2],type="l",col="black",lwd=5,xlab="MONTH",ylab="INFLOW (MGM)",ylim=c(min(MIN_FLOW),max(MAX_FLOW)),main="AVERAGE MONTHLY INFLOW",cex.main=1.5,cex.axis=1.5,cex.lab=1) #THE FOLLOWING LINES PLOT MONTHLY VALUES
  	    polygon(c(1:12,rev(1:12)),c(MIN_FLOW,rev(MAX_FLOW)),col="grey")
  	    lines(PRESENT_MEAN_MONTHLY_FLOW[,2],col="blue",lwd=5)
  	    lines(HIST_MEAN_MONTHLY_FLOW[,2],col="black",lwd=2)

  	    dev.off()
		  }
		)
  })

  ##################### GCM PROJECTIONS ########################################
  output$plot_gcms <- renderPlot({
    createAlert(session, "info_gcm1", alertId = "gcm1",
        title = "Instructions",
        content = "No input is required here. Wait approximately 10 seconds for a plot to appear. Click on the plot if you would like more information. Distributions of precipitation (%) and temperature (Degrees C) changes are based on an ensemble of GCM projections (RCP 4.5) from the World Climate Research Programme's (WCRP's) Coupled Model Intercomparison Projection Phase 5 (CMIP5) multi-model dataset.",
        style = "info",
        dismiss=TRUE,
        append=FALSE
    )

    load("DELTA_TEMP.RData")
    load("DELTA_PRCP.RData")
    x <- as.numeric(substring(colnames(DELTA_TEMP),1,5))
    y <- as.numeric(substring(rownames(DELTA_TEMP),1,6))
    lata <- which.min(abs(x-(input$map_click$lat)))
    lona <- which.min(abs(y-(input$map_click$lng)))
    DELTA_TEMP <- DELTA_TEMP[lona,lata,]
    DELTA_PRCP <- DELTA_PRCP[lona,lata,]
    par(mfrow=c(1,2))
    hist(DELTA_PRCP,col="dodgerblue",xlab="PRECIPITATION CHANGE (%)",main="Histogram of Delta Changes in Precipitation")
    hist(DELTA_TEMP,col="red",xlab="TEMPERATURE CHANGE (C)",main="Histogram of Delta Changes in Temperature")

    output$downloadGCM <- downloadHandler(
		  filename = function() {
		    paste("GCM-", Sys.Date(), input$filetype4, sep=".")
		  },
		  content = function(file) {
  			if (input$filetype4 == "png")
  		    	png(file,height=400,width=900)
  			else
  				pdf(file,height=5,width=9)

    		par(mfrow=c(1,2))
    		hist(DELTA_PRCP,col="dodgerblue",xlab="PRECIPITATION CHANGE (%)",main="Histogram of Delta Changes in Precipitation")
    		hist(DELTA_TEMP,col="red",xlab="TEMPERATURE CHANGE (C)",main="Histogram of Delta Changes in Temperature")

    		dev.off()
    	}
		)
  })

  ######################### CLIMATE RESPONSE SURFACE ###########################
  output$summary <- renderPlot({
    createAlert(session, "info_summary1", alertId="sum1",
      title = "Instructions",
      content = "No input is required here. Wait approximately 10 seconds for a plot to appear. Click on the plot if you would like more information.",
      style = "info",
      dismiss=TRUE,
      append=FALSE
    )

	  STORAGE_CAPACITY <- input$cc

    if (input$input_type2=="Upload Water Supply Demands") {
      WS <- input$file2
      if(is.null(WS))
        return(NULL)
      num_year <- 62
      WATER_SUPPLY <- read.table(WS$datapath,header=TRUE)
      WATER_SUPPLY_MONTHLY <- rep((aggregate(WATER_SUPPLY[,3],by=list(WATER_SUPPLY[,1]),FUN=sum))[,2],num_year)
      WATER_SUPPLY <- WATER_SUPPLY_MONTHLY
    } else {
      num_year <- 62
      WATER_SUPPLY <- read.table("WATER_SUPPLY_DEMAND_SPRINGFIELD.txt",header=TRUE)
      WATER_SUPPLY_MONTHLY <- rep((aggregate(WATER_SUPPLY[,3],by=list(WATER_SUPPLY[,1]),FUN=sum))[,2],num_year)
      WATER_SUPPLY <- WATER_SUPPLY_MONTHLY
    }

  	num_files <- length(list.files(pattern="INFLOWS_"))
  	filenames <- list.files(pattern="INFLOWS_")

  	PRCPP <- unique(rep(seq(0.75,1.25,by=.05),11))
  	TEMPP <- unique(sort(rep(seq(0,5,by=0.5),11)))

    if (input$Standard == FALSE) {
    	POR_RELIABILITY <- array(NA,c(length(TEMPP),length(PRCPP)))
    	for (s in 1:length(TEMPP)) {
    		for (m in 1:length(PRCPP)) {
		    	filename <- paste("INFLOWS_TEMP_",TEMPP[s],"_PRCP_",PRCPP[m],".txt",sep="")
		    	filename_BASE <- paste("INFLOWS_TEMP_",0,"_PRCP_",1,".txt",sep="")

		    	INFLOW_SIM <- read.table(paste(filename,sep=""),header=TRUE)
		    	INFLOW_BASE <- read.table(paste(filename_BASE,sep=""),header=TRUE)

    		 	INFLOWS <- INFLOW_SIM[,1]*(2.59*10^12)*(input$da)*(2.642*10^-7)*(10^-6) #(mm/month)*(mm^2/miles^2)*(DA of reservoir)*(gallons/mm^3)*(MG/gallons)
		    	INFLOWS_BASE <- INFLOW_BASE[,1]*(2.59*10^12)*(input$da)*(2.642*10^-7)*(10^-6) #(mm/month)*(mm^2/miles^2)*(DA of reservoir)*(gallons/mm^3)*(MG/gallons)
		    	sim_length <- length(INFLOWS)

    			#RUN SIMULATION
    			RESERVOIR_STORAGE <- array(0,c(sim_length))				#Reservoir Storage (mg)
    			RELEASES <- array(0,c(sim_length))					    #Reservoir Release (mg)
    			SHORTFALL <- array(NA, c(sim_length))
    			RESERVOIR_STORAGE[1] <- 0.85*input$cc
    			SHORTFALL[1] <- 0

    			for (i in 2:sim_length){
    				if (RESERVOIR_STORAGE[i-1]+INFLOWS[i] < WATER_SUPPLY[i]) {
    				  RELEASES[i] <- RESERVOIR_STORAGE[i-1]+INFLOWS[i]
    				  SHORTFALL[i] <- 1
    				} else if (STORAGE_CAPACITY > RESERVOIR_STORAGE[i-1]+INFLOWS[i]-WATER_SUPPLY[i]) {
    				  RELEASES[i] <- WATER_SUPPLY[i]
    				  SHORTFALL[i] <- 0
    				} else {
    				  RELEASES[i] <- max((RESERVOIR_STORAGE[i-1] + INFLOWS[i] - STORAGE_CAPACITY),WATER_SUPPLY[i])
    				  SHORTFALL[i] <- 0
    				}
    				RESERVOIR_STORAGE[i] <- RESERVOIR_STORAGE[i-1] + INFLOWS[i] - RELEASES[i] #Calculate Storage
    			}

  			  #PERFORMANCE METRICS#
  			  POR_RELIABILITY[s,m] <- 1 - sum(SHORTFALL)/sim_length
        }
      }
		} else {
      #HEDGE
      isolate({source("Hedge.r",local=TRUE)})
      HEDGE <- Hedge(num_year)

      STOR_NORM <- HEDGE[[1]]
      STOR_MOD <- HEDGE[[2]]

      ################################################################################################################
      C1 <- 3179
      C2 <- 6358
      C3 <- 15895

      POR_RELIABILITY <- array(NA,c(length(TEMPP),length(PRCPP)))
    	for (s in 1:length(TEMPP)) {
    		for (m in 1:length(PRCPP)) {
  		    filename <- paste("INFLOWS_TEMP_",TEMPP[s],"_PRCP_",PRCPP[m],".txt",sep="")
  		    filename_BASE <- paste("INFLOWS_TEMP_",0,"_PRCP_",1,".txt",sep="")

  		    INFLOW_SIM <- read.table(paste(filename,sep=""),header=TRUE)
  		    INFLOW_BASE <- read.table(paste(filename_BASE,sep=""),header=TRUE)

  		    INFLOWS <- INFLOW_SIM[,1]*(2.59*10^12)*(input$da)*(2.642*10^-7)*(10^-6) #(mm/month)*(mm^2/miles^2)*(DA of reservoir)*(gallons/mm^3)*(MG/gallons)
  		    INFLOWS_BASE <- INFLOW_BASE[,1]*(2.59*10^12)*(input$da)*(2.642*10^-7)*(10^-6) #(mm/month)*(mm^2/miles^2)*(DA of reservoir)*(gallons/mm^3)*(MG/gallons)
  		    sim_length <- length(INFLOWS)

  		    RESERVOIR_STORAGE <- array(0,c(sim_length))				#Cobble Mountain Storage (mg)
  		    RELEASES <- array(0,c(sim_length))					#Cobble Mountain Release (mg); Just for water supply
  		    SPILL <- array(0,c(sim_length))				#Cobble Mountain Spill (mg);

  		    SHORTFALL <- array(NA, c(sim_length))
  		    SHORTFALL_COST <- array(NA, c(sim_length))

  		    #initial conditions
  		    RESERVOIR_STORAGE[1] <- 0.85*STORAGE_CAPACITY
  		    SHORTFALL[1] <- 0
  		    SHORTFALL_COST[1] <- 0

		      for (i in 2:sim_length){
		        #Now simulate Cobble Mountain release and, if applicable, shortfall costs
		        RELEASES[i] <- WATER_SUPPLY[i]
		        if (RESERVOIR_STORAGE[i-1]>=STOR_NORM[i-1]) {
		          RELEASES[i] <- WATER_SUPPLY[i]
		          SHORTFALL[i] <- 0
		          SHORTFALL_COST[i] <- 0
		        } else if (RESERVOIR_STORAGE[i-1] < STOR_NORM[i-1] & RESERVOIR_STORAGE[i-1]>=STOR_MOD[i-1]) {
		          RELEASES[i] <- .95*WATER_SUPPLY[i]
		          SHORTFALL[i] <- 0
		          SHORTFALL_COST[i] <- C1*3
		        } else if (RESERVOIR_STORAGE[i-1]<STOR_MOD[i-1] & (RESERVOIR_STORAGE[i-1] + INFLOWS[i]) >= (WATER_SUPPLY[i]-6)) {
		          RELEASES[i] <- .85*WATER_SUPPLY[i]
		          SHORTFALL[i] <- 1
		          SHORTFALL_COST[i] <- C1*3 + C2*3
		        } else {
		          RELEASES[i] <- RESERVOIR_STORAGE[i-1] + INFLOWS[i]
		          SHORTFALL[i] <- 1
		          SHORTFALL_COST[i] <- C1*3 + C2*3 + C3*((WATER_SUPPLY[i]-6)- RELEASES[i])
		        }

		        #define spill
		        if (RESERVOIR_STORAGE[i-1] + INFLOWS[i] - RELEASES[i] > STORAGE_CAPACITY){
		          SPILL[i] <- (RESERVOIR_STORAGE[i-1] + INFLOWS[i] - RELEASES[i])- STORAGE_CAPACITY
		        } else {
		          SPILL[i] <- 0
		        }

		        #calculate storage
		        RESERVOIR_STORAGE[i] <- RESERVOIR_STORAGE[i-1] - RELEASES[i] + INFLOWS[i] - SPILL[i]
		      }

		      POR_RELIABILITY[s,m] <- 1 - sum(SHORTFALL)/sim_length
		      TOTAL_RELEASES <- RELEASES + SPILL
    	  }
      }
    }


    ######################## PLOT CLIMATE RESPONSE SURFACE #####################
    load("DELTA_TEMP.RData")
    load("DELTA_PRCP.RData")
    x <- as.numeric(substring(colnames(DELTA_TEMP),1,5))
    y <- as.numeric(substring(rownames(DELTA_TEMP),1,6))
    lata <- which.min(abs(x-(input$map_click$lat)))
    lona <- which.min(abs(y-(input$map_click$lng)))
    DELTA_TEMP <- DELTA_TEMP[lona,lata,]
    DELTA_PRCP <- DELTA_PRCP[lona,lata,]

    #DEFINE PARAMETERS OF MULTIVARIATE NORMAL#
  	m1 <- mean(as.vector(DELTA_TEMP))
  	m2 <- mean(as.vector(DELTA_PRCP))
  	covv <- cov(cbind(as.vector(DELTA_TEMP),as.vector(DELTA_PRCP)))

  	#THIS FINDS ALL OF THE PROBABILITIES FOR THE 'GRIDS'#
  	temp_vals <- c(seq(0,5,0.5))
  	prcp_vals <- c(seq(-0.25,0.25,0.05))
  	prob <- array(NA, c(length(temp_vals),length(prcp_vals)))
  	for (i in 1:length(temp_vals)) {
  		for (j in 1:length(prcp_vals)) {
  			aa <- c(temp_vals[i],(prcp_vals[j]+1))
  			prob[i,j] <- dmnorm(aa,mean=c(m1,m2),varcov=covv)
  		}
  	}

    mean_prcp_change_percent <- unique(rep(seq(0.75,1.25,by=.05),11))*100
    mean_temp_change_celsius <- unique(sort(rep(seq(0,5,by=0.5),11)))

    #FINAL PLOT
		tt <- "WATER SUPPLY RELIABILITY"
		CONTOUR_VARIABLE <- POR_RELIABILITY
		TRESHOLD <- input$rel
		zlim <- range(CONTOUR_VARIABLE, finite = TRUE)
		n <- 20
		level <- pretty(zlim, n)
		w <- which(abs(level-TRESHOLD)==min(abs(level-TRESHOLD)))
		level[w] <- TRESHOLD
		color1 = colorRampPalette(c("red","white"))(w-1)
		color2 = colorRampPalette(c("#6868FF","blue"))(length(level)-w)
		color = c(color1,color2)
		all_one <- length(which((CONTOUR_VARIABLE!=1)==FALSE))

		if (all_one==121){
  		image.plot(mean_temp_change_celsius,mean_prcp_change_percent,CONTOUR_VARIABLE,main = tt,zlim=c(0,1),legend.lab="Performance Metric",col=color,xlab="TEMPERATURE CHANGE (C)",ylab="PRECIPITATION CHANGE (%)")
  		grid(11,11,col="black")
	  } else {
  		tt <- "WATER SUPPLY RELIABILITY"
  		n <- 20
  		lwd = 5
  		TRESHOLD <- input$rel #mean(MONTHLY_FLOWS[,3]*cfs_to_mm_per_month[1:732])
  		xgrid <- mean_prcp_change_percent
  		ygrid <- mean_temp_change_celsius
  		xlabel <- "Precipitation Mean (% Change)"
  		ylabel <- "Temperature Mean Change (C)"
  		CONTOUR_VARIABLE <- POR_RELIABILITY
      zlim <- c(0,1)
  		level <- pretty(zlim, n)
  		## Not taking into account threshold ##
  		color = colorRampPalette(c("red","white","blue"))(length(level)-1)
  		## Not taking into account threshold END ##
  		## Using threshold ##
  		w <- which(abs(level-TRESHOLD)==min(abs(level-TRESHOLD)))
  		level[w] <- TRESHOLD
  		color1 = colorRampPalette(c("red","white"))(w-1)
  		color2 = colorRampPalette(c("#6868FF","blue"))(length(level)-w)
  		color = c(color1,color2)
  		## Using threshold END ##
  		par(font.lab=2,font.axis=2)
  		filled.contour(ygrid,xgrid,CONTOUR_VARIABLE,ylab=xlabel,xlab=ylabel, main = tt,col=color,zlim = zlim,levels = level,
    		plot.axes = {
    		  points(DELTA_TEMP,DELTA_PRCP*100,pch=16);
      		contour(ygrid, xgrid, CONTOUR_VARIABLE,
      		        level=c(TRESHOLD), lty=(c("solid")),
      		        add=TRUE,lwd=lwd,drawlabels=FALSE);
      		axis(1);
    		  axis(2)
    		}
    	)
  	}

  	output$downloadSummary <- downloadHandler(
  		filename = function() {
		    paste("Summary-", Sys.Date(), input$filetype5, sep=".")
		  },
  		content = function(file) {
  			if (input$filetype5 == "png")
  		    	png(file,height=700,width=700)
  			else
  				pdf(file,height=8,width=8)

    		tt <- "WATER SUPPLY RELIABILITY"
    		CONTOUR_VARIABLE <- POR_RELIABILITY
    		TRESHOLD <- input$rel
    		zlim <- range(CONTOUR_VARIABLE, finite = TRUE)
    		n <- 20
    		level <- pretty(zlim, n)
    		w <- which(abs(level-TRESHOLD)==min(abs(level-TRESHOLD)))
    		level[w] <- TRESHOLD
    		color1 = colorRampPalette(c("red","white"))(w-1)
    		color2 = colorRampPalette(c("#6868FF","blue"))(length(level)-w)
    		color = c(color1,color2)
    		all_one <- length(which((CONTOUR_VARIABLE!=1)==FALSE))
      	if (all_one==121){
      		image.plot(mean_temp_change_celsius,mean_prcp_change_percent,CONTOUR_VARIABLE,main = tt,zlim=c(0,1),legend.lab="Performance Metric",col=color,xlab="TEMPERATURE CHANGE (C)",ylab="PRECIPITATION CHANGE (%)")
      		grid(11,11,col="black")
      	} else {
      		tt <- "WATER SUPPLY RELIABILITY"
      		n <- 20
      		lwd = 5
      		TRESHOLD <- input$rel #mean(MONTHLY_FLOWS[,3]*cfs_to_mm_per_month[1:732])
      		xgrid <- mean_prcp_change_percent
      		ygrid <- mean_temp_change_celsius
      		xlabel <- "Precipitation Mean (% Change)"
      		ylabel <- "Temperature Mean Change (C)"
      		CONTOUR_VARIABLE <- POR_RELIABILITY
      		#zlim <- range(CONTOUR_VARIABLE, finite = TRUE)
          zlim <- c(0,1)
      		level <- pretty(zlim, n)
      		## Not taking into account threshold ##
      		color = colorRampPalette(c("red","white","blue"))(length(level)-1)
      		## Not taking into account threshold END ##
      		## Using threshold ##
      		w <- which(abs(level-TRESHOLD)==min(abs(level-TRESHOLD)))
      		level[w] <- TRESHOLD
      		color1 = colorRampPalette(c("red","white"))(w-1)
      		color2 = colorRampPalette(c("#6868FF","blue"))(length(level)-w)
      		color = c(color1,color2)
      		## Using threshold END ##
      		par(font.lab=2,font.axis=2)
      		filled.contour(ygrid,xgrid,CONTOUR_VARIABLE,ylab=xlabel,xlab=ylabel, main = tt,col=color,zlim = zlim,levels = level,
      		  plot.axes = {
      		    points(DELTA_TEMP,DELTA_PRCP*100,pch=16);
      		    contour(ygrid,xgrid,CONTOUR_VARIABLE,level=c(TRESHOLD),lty=(c("solid")),
      		            add=TRUE,lwd=lwd,drawlabels=FALSE);
      		    axis(1);
      		    axis(2)
      		  }
      		)
      	}
    		dev.off()
    	}
    )
  })

  ####################### PROBABILITY OF PROBLEMATIC CONDITIONS ################
  output$probability <- renderPlot({
    createAlert(session, "info_prob1",alertId="prob1",
      title = "Instructions",
      content="No input is required here. Wait approximately 10 seconds for a plot to appear. Click on the plot if you would like more information.",
      style="info",
      dismiss=TRUE,
      append=FALSE
    )

  	STORAGE_CAPACITY <- input$cc

    if (input$input_type2=="Upload Water Supply Demands"){
      WS <- input$file2
      if(is.null(WS))
        return(NULL)
      num_year <- 62
      WATER_SUPPLY <- read.table(WS$datapath,header=TRUE)
      WATER_SUPPLY_MONTHLY <- rep((aggregate(WATER_SUPPLY[,3],by=list(WATER_SUPPLY[,1]),FUN=sum))[,2],num_year)
      WATER_SUPPLY <- WATER_SUPPLY_MONTHLY
    } else {
      num_year <- 62
      WATER_SUPPLY <- read.table("WATER_SUPPLY_DEMAND_SPRINGFIELD.txt",header=TRUE)
      WATER_SUPPLY_MONTHLY <- rep((aggregate(WATER_SUPPLY[,3],by=list(WATER_SUPPLY[,1]),FUN=sum))[,2],num_year)
      WATER_SUPPLY <- WATER_SUPPLY_MONTHLY
    }

    num_files <- length(list.files(pattern="INFLOWS_"))
	  filenames <- list.files(pattern="INFLOWS_")

  	PRCPP <- unique(rep(seq(0.75,1.25,by=.05),11))
  	TEMPP <- unique(sort(rep(seq(0,5,by=0.5),11)))

    if (input$Standard == FALSE) {
    	POR_RELIABILITY <- array(NA,c(length(TEMPP),length(PRCPP)))
    	for (s in 1:length(TEMPP)) {
    		for (m in 1:length(PRCPP)) {
  				filename <- paste("INFLOWS_TEMP_",TEMPP[s],"_PRCP_",PRCPP[m],".txt",sep="")
  				filename_BASE <- paste("INFLOWS_TEMP_",0,"_PRCP_",1,".txt",sep="")

  				INFLOW_SIM <- read.table(paste(filename,sep=""),header=TRUE)
  				INFLOW_BASE <- read.table(paste(filename_BASE,sep=""),header=TRUE)

  		 		INFLOWS <- INFLOW_SIM[,1]*(2.59*10^12)*(input$da)*(2.642*10^-7)*(10^-6) #(mm/month)*(mm^2/miles^2)*(DA of reservoir)*(gallons/mm^3)*(MG/gallons)
  				INFLOWS_BASE <- INFLOW_BASE[,1]*(2.59*10^12)*(input$da)*(2.642*10^-7)*(10^-6) #(mm/month)*(mm^2/miles^2)*(DA of reservoir)*(gallons/mm^3)*(MG/gallons)
  				sim_length <- length(INFLOWS)

    			#RUN SIMULATION
    			RESERVOIR_STORAGE <- array(0,c(sim_length))				#Reservoir Storage (mg)
    			RELEASES <- array(0,c(sim_length))					    #Reservoir Release (mg)
    			SHORTFALL <- array(NA, c(sim_length))
    			RESERVOIR_STORAGE[1] <- 0.85*input$cc
    			SHORTFALL[1] <- 0

    			for (i in 2:sim_length) {
    				if (RESERVOIR_STORAGE[i-1]+INFLOWS[i] < WATER_SUPPLY[i]) {
    				  RELEASES[i] <- RESERVOIR_STORAGE[i-1]+INFLOWS[i]
    				  SHORTFALL[i] <- 1
    				} else if (STORAGE_CAPACITY > RESERVOIR_STORAGE[i-1]+INFLOWS[i]-WATER_SUPPLY[i]) {
    				  RELEASES[i] <- WATER_SUPPLY[i]
    				  SHORTFALL[i] <- 0
    				} else {
    				  RELEASES[i] <- max((RESERVOIR_STORAGE[i-1] + INFLOWS[i] - STORAGE_CAPACITY),WATER_SUPPLY[i])
    				  SHORTFALL[i] <- 0
    				}
    				RESERVOIR_STORAGE[i] <- RESERVOIR_STORAGE[i-1] + INFLOWS[i] - RELEASES[i] #Calculate Storage
    			}

  			  #PERFORMANCE METRICS#
  			  POR_RELIABILITY[s,m] <- 1 - sum(SHORTFALL)/sim_length
  	  	}
    	}
	  } else {
      # HEDGE
      isolate({source("Hedge.r",local=TRUE)})
      HEDGE <- Hedge(num_year)

      STOR_NORM <- HEDGE[[1]]
      STOR_MOD <- HEDGE[[2]]

  	  C1 <- 3179
  	  C2 <- 6358
  	  C3 <- 15895

	  	POR_RELIABILITY <- array(NA,c(length(TEMPP),length(PRCPP)))
	  	for (s in 1:length(TEMPP)) {
	  		for (m in 1:length(PRCPP)) {
  				filename <- paste("INFLOWS_TEMP_",TEMPP[s],"_PRCP_",PRCPP[m],".txt",sep="")
  				filename_BASE <- paste("INFLOWS_TEMP_",0,"_PRCP_",1,".txt",sep="")

  				INFLOW_SIM <- read.table(paste(filename,sep=""),header=TRUE)
  				INFLOW_BASE <- read.table(paste(filename_BASE,sep=""),header=TRUE)

  				INFLOWS <- INFLOW_SIM[,1]*(2.59*10^12)*(input$da)*(2.642*10^-7)*(10^-6) #(mm/month)*(mm^2/miles^2)*(DA of reservoir)*(gallons/mm^3)*(MG/gallons)
  				INFLOWS_BASE <- INFLOW_BASE[,1]*(2.59*10^12)*(input$da)*(2.642*10^-7)*(10^-6) #(mm/month)*(mm^2/miles^2)*(DA of reservoir)*(gallons/mm^3)*(MG/gallons)
  				sim_length <- length(INFLOWS)

  				RESERVOIR_STORAGE <- array(0,c(sim_length))				#Cobble Mountain Storage (mg)
  				RELEASES <- array(0,c(sim_length))					#Cobble Mountain Release (mg); Just for water supply
  				SPILL <- array(0,c(sim_length))				#Cobble Mountain Spill (mg);

  				SHORTFALL <- array(NA, c(sim_length))
  				SHORTFALL_COST <- array(NA, c(sim_length))

  				#initial conditions
  				RESERVOIR_STORAGE[1] <- 0.85*STORAGE_CAPACITY
  				SHORTFALL[1] <- 0
  				SHORTFALL_COST[1] <- 0

  				for (i in 2:sim_length){
  					#Now simulate Cobble Mountain release and, if applicable, shortfall costs
  					RELEASES[i] <- WATER_SUPPLY[i]
  					if (RESERVOIR_STORAGE[i-1]>=STOR_NORM[i-1]) {
  					  RELEASES[i] <- WATER_SUPPLY[i]
  					  SHORTFALL[i] <- 0
  					  SHORTFALL_COST[i] <- 0
  					} else if (RESERVOIR_STORAGE[i-1] < STOR_NORM[i-1] & RESERVOIR_STORAGE[i-1]>=STOR_MOD[i-1]) {
  					  RELEASES[i] <- .95*WATER_SUPPLY[i]
  					  SHORTFALL[i] <- 0
  					  SHORTFALL_COST[i] <- C1*3
  					} else if (RESERVOIR_STORAGE[i-1]<STOR_MOD[i-1] & (RESERVOIR_STORAGE[i-1] + INFLOWS[i]) >= (WATER_SUPPLY[i]-6)) {
  					  RELEASES[i] <- .85*WATER_SUPPLY[i]
  					  SHORTFALL[i] <- 1
  					  SHORTFALL_COST[i] <- C1*3 + C2*3
  					} else {
  					  RELEASES[i] <- RESERVOIR_STORAGE[i-1] + INFLOWS[i]
  					  SHORTFALL[i] <- 1
  					  SHORTFALL_COST[i] <- C1*3 + C2*3 + C3*((WATER_SUPPLY[i]-6)- RELEASES[i])
  					}

  					#define spill
  					if (RESERVOIR_STORAGE[i-1] + INFLOWS[i] - RELEASES[i] > STORAGE_CAPACITY){
  					  SPILL[i] <- (RESERVOIR_STORAGE[i-1] + INFLOWS[i] - RELEASES[i])- STORAGE_CAPACITY
  					} else {
  					  SPILL[i] <- 0
  					}

  					#calculate storage
  					RESERVOIR_STORAGE[i] <- RESERVOIR_STORAGE[i-1] - RELEASES[i] + INFLOWS[i] - SPILL[i]

  				}

  			  POR_RELIABILITY[s,m] <- 1 - sum(SHORTFALL)/sim_length
  			  TOTAL_RELEASES <- RELEASES + SPILL
  	  	}
  	  }
    }

    ########################## PLOT CLIMATE RESPONSE SURFACE ###################
    load("DELTA_TEMP.RData")
    load("DELTA_PRCP.RData")
    num_GCMS <- dim(DELTA_TEMP)[3]
    x <- as.numeric(substring(colnames(DELTA_TEMP),1,5))
    y <- as.numeric(substring(rownames(DELTA_TEMP),1,6))
    lata <- which.min(abs(x-(input$map_click$lat)))
    lona <- which.min(abs(y-(input$map_click$lng)))
    DELTA_TEMP <- DELTA_TEMP[lona,lata,]
    DELTA_PRCP <- DELTA_PRCP[lona,lata,]

   	prcp1 <- unique(rep(seq(0.75,1.25,by=0.05),11))
   	temp1 <- unique(sort(rep(seq(0,5,by=0.5),11)))
   	WEIGHTED_REL <- array(NA,c(num_GCMS))

   	for (i in 1:length(DELTA_TEMP)) {
   		GCM_VAL <- rbind(DELTA_TEMP[i],DELTA_PRCP[i])
   		xa <- which(ceiling(GCM_VAL[1,]*2)/2 == temp1)
   		xb <- which(floor(GCM_VAL[1,]*2)/2 == temp1)
   		ya <- which(ceiling(GCM_VAL[2,]*20)/20 == prcp1)
   		yb <- which(floor(GCM_VAL[2,]*20)/20 == prcp1)

   		PRCP_HIGH_DIST <- abs(GCM_VAL[2,]-prcp1[ya]) #distance from p-1.05
   		PRCP_LOW_DIST <- abs(GCM_VAL[2,]-prcp1[yb]) #distance from p-1
   		TEMP_HIGH_DIST <- abs(GCM_VAL[1,]-temp1[xa]) #distance from t-2
   		TEMP_LOW_DIST <- abs(GCM_VAL[1,]-temp1[xb]) #distance from t-1.5
   		TOTAL_DISTa1 <- sqrt((PRCP_HIGH_DIST^2)+(TEMP_HIGH_DIST^2))
   		TOTAL_DISTb1 <- sqrt((PRCP_HIGH_DIST^2)+(TEMP_LOW_DIST^2))
   		TOTAL_DISTa2 <- sqrt((PRCP_LOW_DIST^2)+(TEMP_HIGH_DIST^2))
   		TOTAL_DISTb2 <- sqrt((PRCP_LOW_DIST^2)+(TEMP_LOW_DIST^2))
   		COMBINED_DISTANCES <- cbind(TOTAL_DISTa1,TOTAL_DISTa2,TOTAL_DISTb1,TOTAL_DISTb2)

   		SAMPLING_WEIGHTS <- (1/COMBINED_DISTANCES^2)/sum(1/COMBINED_DISTANCES^2) #inverse distance squared
   		wa1 <- SAMPLING_WEIGHTS[1]
   		wa2 <- SAMPLING_WEIGHTS[2]
   		wb1 <- SAMPLING_WEIGHTS[3]
   		wb2 <- SAMPLING_WEIGHTS[4]

   		ra1 <- POR_RELIABILITY[xa,ya]
   		ra2 <- POR_RELIABILITY[xa,yb]
   		rb1 <- POR_RELIABILITY[xb,ya]
   		rb2 <- POR_RELIABILITY[xb,yb]
   		WEIGHTED_REL[i] <- ((ra1*wa1)+(ra2*wa2)+(rb1*wb1)+(rb2*wb2))
   	}
   	ACCEPTABLE <- length(which((WEIGHTED_REL>=input$rel)==TRUE))/num_GCMS
   	UNACCEPTABLE <- 1-ACCEPTABLE

    nammee <- paste("Climate Risk\n (Fraction of GCM projections)",sep=" ")
    barplot(c(UNACCEPTABLE,ACCEPTABLE),names.arg =c("Fraction of climate projections \nthat suggest unacceptable performance","Fraction of climate projections \nthat suggest acceptable performance"),cex.main=1.5,col=c("red","blue"),ylim=c(0,1),main=nammee)

    output$downloadFinal <- downloadHandler(
		  filename = function() {
		    paste("Final-", Sys.Date(), input$filetype6, sep=".")
		  },
  		content = function(file) {
  			if (input$filetype6 == "png")
  		    	png(file,height=700,width=700)
  			else
  				pdf(file,height=7,width=8)

  			nammee <- paste("Climate Risk\n (Fraction of GCM projections)",sep=" ")
  			barplot(c(UNACCEPTABLE,ACCEPTABLE),names.arg =c("Fraction of climate projections \nthat suggest unacceptable performance","Fraction of climate projections \nthat suggest acceptable performance"),cex.main=1.5,col=c("red","blue"),ylim=c(0,1),main=nammee)
  			dev.off()
  		}
    )
  })
})
