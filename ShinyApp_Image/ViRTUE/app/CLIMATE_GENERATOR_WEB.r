############################################CLIMATE GENERATOR MODEL####################################################
CLIMATE_GENERATOR <- function(mean_prcp_change_percent,mean_temp_change_celsius,num_year,climateData) {    #THIS SCRIPT PRODUCES MONTHLY CLIMATE DATA
    library(maps)
    library(mapproj)
    library(psych)
    library(MASS)
    #############################################GENERATE AVG ANNUAL CLIMATE OVER SPACE OF INTEREST#########################
      
    CLIMATE_AVG <- BASIN_CLIMATE_AVERAGING(climateData)
    #########################################KNN ON REGIONAL CLIMATE DATA###########################################
    SIMULATED_PRCP_ANNUAL_FINAL <- read.table("Prec_Gen_1.txt",header=TRUE)
    
    CLIMATE_AVG_LOWER <- read.table("CLIMATE_AVG_LOWER.txt",header=TRUE)
    WARM_PRCP_REGIONAL <- SIMULATED_PRCP_ANNUAL_FINAL
    OBS_PRCP_REGIONAL <- CLIMATE_AVG_LOWER[,1:2]
    YEARS <- OBS_PRCP_REGIONAL[,1]
    num_sim <- dim(WARM_PRCP_REGIONAL)[2]
    num_months <- num_year*12
    climate_var <- c("PRCP","TMAX","TMIN","TAVG")
    
    DISTANCE_SAMPLES <- array(NA,c(num_year,num_sim))
    AVG_MONTHLY_CLIMATE_VAR <- array(NA,c(num_months,length(climate_var),num_sim))

    for (samp in 1:num_sim) {
        for (j in 1:num_year) {
            Distance_Climate <- sqrt((WARM_PRCP_REGIONAL[j,samp] - OBS_PRCP_REGIONAL[,2])^2)
            FINAL_DISTANCES <- cbind(YEARS,Distance_Climate)
            FINAL_DISTANCES_SORTED <- FINAL_DISTANCES[which(rank(FINAL_DISTANCES[,2])%in% 1:8),]
            ORDERED_YR_PRCP <- cbind(FINAL_DISTANCES_SORTED[,1],OBS_PRCP_REGIONAL[match(FINAL_DISTANCES_SORTED[,1],YEARS),2])
            SAMPLING_WEIGHTS <- (1/FINAL_DISTANCES_SORTED[,2])^2/sum((1/FINAL_DISTANCES_SORTED[,2])^2) #inverse distance squared
            DISTANCE_SAMPLES[j,samp] <- sample(ORDERED_YR_PRCP[,1],size=1,prob=SAMPLING_WEIGHTS,replace=FALSE)
            AVG_MONTHLY_CLIMATE_VAR[((1+12*(j-1)):(12+12*(j-1))),,samp] <- data.matrix(CLIMATE_AVG[which(DISTANCE_SAMPLES[j,samp]==CLIMATE_AVG[,2]),3:6])
        }    
    }
    #####################################GENERATE CLIMATE CHANGE TRENDS######################################################
   sim_length <- 50

    num_month_sim <- length(AVG_MONTHLY_CLIMATE_VAR[,1,1])
    YEAR_M <- rep(YEARS,each=12)
    MONTH_M <- rep(1:12,length(YEARS))
    
    sim <- sample(1:50,1)     
    CLIMATE_TAVG <- AVG_MONTHLY_CLIMATE_VAR[,4,sim]
    CLIMATE_TMIN <- AVG_MONTHLY_CLIMATE_VAR[,3,sim]
    CLIMATE_TMAX <- AVG_MONTHLY_CLIMATE_VAR[,2,sim]
    CLIMATE_PRCP <- AVG_MONTHLY_CLIMATE_VAR[,1,sim]

    for (t in 1:length(mean_temp_change_celsius)) {
        for (p in 1:length(mean_prcp_change_percent)) {
            dir_ending <- paste("PRCP_MEAN_CHANGES",mean_prcp_change_percent[p],"TEMP_CHANGES",mean_temp_change_celsius[t],sep="_")
	
            TAVG_CLIM_SEQ <- seq(0,mean_temp_change_celsius[t],length.out=num_month_sim) + CLIMATE_TAVG
            TMIN_CLIM_SEQ <- seq(0,mean_temp_change_celsius[t],length.out=num_month_sim) + CLIMATE_TMIN
            TMAX_CLIM_SEQ <- seq(0,mean_temp_change_celsius[t],length.out=num_month_sim) + CLIMATE_TMAX
            PRCP_CLIM_SEQ <- seq(1,mean_prcp_change_percent[p],length.out=num_month_sim)*CLIMATE_PRCP
            
            FINAL_STOCHASTIC_MONTHLY <- cbind(YEAR_M,MONTH_M,PRCP_CLIM_SEQ,TMAX_CLIM_SEQ,TMIN_CLIM_SEQ,TAVG_CLIM_SEQ) 
            colnames(FINAL_STOCHASTIC_MONTHLY) <- c("YEAR","MONTH","PRCP","TMAX","TMIN","TAVG") 
            name <- paste(dir_ending,sep="")
            dirr <- paste(name,sep="/")
            write.table(FINAL_STOCHASTIC_MONTHLY,dirr)              
        }
    }              		
       
    return(AVG_MONTHLY_CLIMATE_VAR)
}









