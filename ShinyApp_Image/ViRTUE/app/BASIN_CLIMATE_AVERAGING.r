BASIN_CLIMATE_AVERAGING <- function(climateData) {
 
	num_site <- 1	
	DATA <- climateData
	YEAR <- DATA[,1]
	MONTH <- DATA[,2]
	DAY <- DATA[,3]

	SITE_PRCP <- DATA[,4]
	SITE_TMAX <- DATA[,5]
	SITE_TMIN <- DATA[,6]
	no_leap <- which(DAY!=29 | MONTH!=2)

	PRCP <- SITE_PRCP[no_leap]
	TMAX <- SITE_TMAX[no_leap]
	TMIN <- SITE_TMIN[no_leap]
	TEMP <- (TMAX + TMIN)/2

	#Create Time Set
	YEAR_D <- DATA[no_leap,1]
	MONTH_D <-  DATA[no_leap,2]
	DAY_D <- DATA[no_leap,3]

	allData <- cbind(YEAR_D,MONTH_D,DAY_D,PRCP,TMAX,TMIN,TEMP)

	#MONTHLY VARIABLES
	MONTHLY_DATES <- aggregate(allData[,4],FUN=sum,by=list(allData[,2],allData[,1]))[,1:2]
	MONTHLY_PRCP <- aggregate(allData[,4],FUN=sum,by=list(allData[,2],allData[,1]))[,3]
	MONTHLY_TMAX <- aggregate(allData[,5],FUN=mean,by=list(allData[,2],allData[,1]))[,3]
	MONTHLY_TMIN <- aggregate(allData[,6],FUN=mean,by=list(allData[,2],allData[,1]))[,3]
	MONTHLY_TEMP <- aggregate(allData[,7],FUN=mean,by=list(allData[,2],allData[,1]))[,3]

	PRCP <- MONTHLY_PRCP[1:(length(MONTHLY_PRCP))]
	TMAX <- MONTHLY_TMAX[1:(length(MONTHLY_PRCP))]
	TMIN <- MONTHLY_TMIN[1:(length(MONTHLY_PRCP))]
	TEMP <- MONTHLY_TEMP[1:(length(MONTHLY_PRCP))]

	return(cbind(MONTHLY_DATES,PRCP,TMAX,TMIN,TEMP))

}