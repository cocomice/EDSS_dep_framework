
#####################################################################################################################################
#ABCDE model2: Model 2 is the model to be run in order to spin up the models initial states
ABCDE_QEST <- function(parameters,P,PE,T,calib_num) {

		
	#parameters = a vector with a,b,c,d
	#P	= a vector with precip time series for current station
	#PE	= a vector with potential evapotranspiration time series for current station
	#P	= a vector with tavg time series for current statio
	#Qobs	= a vector with observed streamflow time series for current station			#Sint = initial soil moisture
	#Gint = initial groundwater storage
	#Aint = initial snow accumulation
		
	parm <- parameters
	Peff <- array(0,calib_num)   	#snow storage
	ETeff <- array(0,calib_num)   	#snow storage
	A <- array(0,calib_num)   	#snow storage
	W <- array(0,calib_num)   	#available water
	Y <- array(0,calib_num)   	#evapotranspiration opportunity
	S <- array(0,calib_num)  	#soil moisture
	E <- array(0,calib_num)  	#actual evaporation
	G <- array(0,calib_num)  	#groundwater storage 
	Qest <- array(0,calib_num)   #estimated runoff
	S[1] <- parm[7]
	G[1] <- parm[8]
	A[1] <- parm[9]
		
	for (i in 2:calib_num) {
		#Begin snow component
		if (T[i] > parm[6]) {
			mt <- min(parm[5]*(T[i]-parm[6])*A[i-1],A[i-1])
			Peff[i] <- P[i]+mt
			ETeff[i] <- PE[i]
			A[i] <- A[i-1] - mt
		} else {
			mt <- 0
			Peff[i] <- 0
			ETeff[i] <- 0
			A[i] <- A[i-1]+P[i]	
		}
		
		W[i] <- Peff[i] + S[i-1]
		#w1 and w2 are intermediate values used to calculate Y
		w1 <- (W[i]+parm[2])/(2*parm[1])
		w2 <- W[i]*parm[2]/parm[1]
		Y[i] <- w1 - sqrt((w1^2)-w2)
		S[i] <- Y[i]*exp(-1*ETeff[i]/parm[2])
		E[i] <- Y[i]*(1-exp(-1*(ETeff[i]/parm[2])))
		G[i] <- (G[i-1] + parm[3]*round((W[i]-Y[i]),2))/(1+parm[4])
		Qest[i] <- (1-parm[3])*round((W[i]-Y[i]),2)+parm[4]*G[i]
	}
	return(Qest)	
}
#################################################################################################################
