#function for making left-tail and right tail associated noise, B and B.sharp
library(copula)

source("lottery_normcop.R")

LeftTailMod <- function(mu.B, sigma.B, rho, totT){
	
	ccop <- claytonCopula(param = 1.05, dim = 2)
	logB_pre <- rCopula(totT, ccop)
	
	logB <- logB_pre
	logB[,1] <- qnorm(logB_pre[,1], mean = mu.B[1], sd = 		sigma.B[1])
	logB[,2] <- qnorm(logB_pre[,2], mean = mu.B[2], sd = 		sigma.B[2])
	
	return(exp(logB))
	
}

Belft <- LeftTailMod(mu.B = mu.B, sigma.B = sigma.B, totT = totT)



