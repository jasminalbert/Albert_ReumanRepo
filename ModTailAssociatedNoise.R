#function for making left-tail and right tail associated noise, B and B.sharp
library(copula)
#combine into one functin
ModTail <- function(mu, sd, totT, tail){
	
	ccop <- claytonCopula(param = 3.76, dim = 2)
	logB_pre <- rCopula(n = totT, ccop)
	
	if (tail == 'left') {
		logBpre <- logB_pre
	}
	if (tail == 'right') {
		logBpre <- (-logB_pre + 1)
	}
	if (!any(tail == 'right' || tail == 'left')) {
		stop("Error: tail must defined as left or right")
	}
	
	logB <- logBpre
	logB[,1] <- qnorm(logBpre[,1], mean = mu[1], sd = sd[1])
	logB[,2] <- qnorm(logBpre[,2], mean = mu[2], sd = sd[2])
	
	
	return(logB)
	
}
#function for generating four sets of mod noise
Modnoise <- function(mu, sd, totT){
	ccop <- claytonCopula(param = 3.76, dim = 2)
	logB_pre <- rCopula(n = totT, ccop)
	
	#moderately left tailed noise
	logB_L <- logB_pre
	logB_L[,1] <- qnorm(logB_pre[,1], mean = mu[1], sd = sd[1])
	logB_L[,2] <- qnorm(logB_pre[,2], mean = mu[2], sd = sd[2])	
	
	#moderatley right tailed noise
	logB_preR <- (-logB_pre + 1)
	logB_R <- logB_preR
	logB_R[,1] <- qnorm(logB_preR[,1], mean = mu[1], sd = sd[1])
	logB_R[,2] <- qnorm(logB_preR[,2], mean = mu[2], sd = sd[2])	
	
	#sharp noise
	logB_preSHARP <- rCopula(n = totT, claytonCopula(param = 3.76, dim = 2))
	
	#moderately left tailed noise -sharp
	logB_LSHARP <- logB_preSHARP
	logB_LSHARP[,1] <- qnorm(logB_preSHARP[,1], mean = mu[1], sd = sd[1])
	logB_LSHARP[,2] <- qnorm(logB_preSHARP[,2], mean = mu[2], sd = sd[2])	
	
	#moderatley right tailed noise -sharp
	logB_preRSHARP <- (-logB_preSHARP + 1)
	logB_RSHARP <- logB_preRSHARP
	logB_RSHARP[,1] <- qnorm(logB_preRSHARP[,1], mean = mu[1], sd = sd[1])
	logB_RSHARP[,2] <- qnorm(logB_preRSHARP[,2], mean = mu[2], sd = sd[2])	
	
	return(list(B_MLT = logB_L, B_MLT_sharp = logB_LSHARP, B_MRT = logB_R, B_MRT_sharp = logB_RSHARP ) )
}




#B_MLT <- ModTail(mu=mn, sd=sdev, totT=n, tail='left')
#B_MLT_sharp <- ModTail(mu=mn, sd=sdev, totT=n, tail='left')
#cor(B_MLT)[1,2]
#B_MRT <- ModTail(mu=mn, sd=sdev, totT=n, tail='right')
#B_MRT_sharp <- ModTail(mu=mn, sd=sdev, totT=n, tail='right')
#cor(B_MRT)[1,2]
#plot(B_MRT[1:1000,1], B_MRT[1:1000,2])
#plot(B_MLT[1:1000,1], B_MLT[1:1000,2])


#source("lottery_normcop.R")
#source("lottery_normcop_test.R")

#LeftTailMod <- function(mu.B, sigma.B, totT){
	
	#ccop <- claytonCopula(param = 1.05, dim = 2)
	#logB_pre <- rCopula(totT, ccop)
	
	#logB <- logB_pre
	#logB[,1] <- qnorm(logB_pre[,1], mean = mu.B[1], sd = 		sigma.B[1])
	#logB[,2] <- qnorm(logB_pre[,2], mean = mu.B[2], sd = 		sigma.B[2])
	
	#return(exp(logB))
	
#}

#RightTailMod <- function(mu.B, sigma.B, totT){
	#ccop <- claytonCopula(param = 3.77, dim = 2)
	#logB_pre <- rCopula(totT, ccop)
	
	#logB_pre <- (-logB_pre+1)
	
	#logB <- logB_pre
	#logB[,1] <- qnorm(logB_pre[,1], mean = mu.B[1], sd = 		sigma.B[1])
	#logB[,2] <- qnorm(logB_pre[,2], mean = mu.B[2], sd = 		#sigma.B[2])
	
	#return(logB)
#}

#B_MR <- RightTailMod(mu.B = mn, sigma.B = sdev, totT = n)

#plot(B_MR[1:1000,1], B_MR[1:1000,2])

#testing the fucntions 

#set.seed(121212)
#Bleft <- LeftTailMod(mu.B = mu.B, sigma.B = sigma.B, totT = totT)
#Bleft.sharp <- LeftTailMod(mu.B = mu.B, sigma.B = sigma.B, totT = totT)

#plot(Bleft[1:1000,1], Bleft[1:1000,2], type = 'p')
#plot(Bleft.sharp[,1], Bleft.sharp[,2], type = 'p')

#lottery_normcop(B = Bleft, B.sharp = Bleft.sharp, delta = 0.25, q12 = 1)

#Bright <- RightTailMod(mu.B = mu.B, sigma.B = sigma.B, totT = totT)
#Bright.sharp <- RightTailMod(mu.B = mu.B, sigma.B = sigma.B, totT = totT)

#plot(Bright[1:1000,1], Bright[1:1000,2], type = 'p')






