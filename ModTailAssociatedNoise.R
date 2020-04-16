#function for making left-tail and right tail associated noise, B and B.sharp
library(copula)

source("lottery_normcop.R")
totT <- 1000
LeftTailMod <- function(mu.B, sigma.B, rho, totT){
	
	ccop <- claytonCopula(param = 1.05, dim = 2)
	logB_pre <- rCopula(totT, ccop)
	
	logB <- logB_pre
	logB[,1] <- qnorm(logB_pre[,1], mean = mu.B[1], sd = 		sigma.B[1])
	logB[,2] <- qnorm(logB_pre[,2], mean = mu.B[2], sd = 		sigma.B[2])
	
	return(exp(logB))
	
}

RightTailMod <- function(mu.B, sigma.B, rho, totT){
	ccop <- claytonCopula(param = 1.05, dim = 2)
	logB_pre <- rCopula(totT, ccop)
	
	logB_pre <- (-logB_pre+1)
	
	logB <- logB_pre
	logB[,1] <- qnorm(logB_pre[,1], mean = mu.B[1], sd = 		sigma.B[1])
	logB[,2] <- qnorm(logB_pre[,2], mean = mu.B[2], sd = 		sigma.B[2])
	
	return(exp(logB))
}

set.seed(121212)
Bleft <- LeftTailMod(mu.B = mu.B, sigma.B = sigma.B, totT = totT)
Bleft.sharp <- LeftTailMod(mu.B = mu.B, sigma.B = sigma.B, totT = totT)

plot(Bleft[1:1000,1], Bleft[1:1000,2], type = 'p')
plot(Bleft.sharp[,1], Bleft.sharp[,2], type = 'p')

lottery_normcop(B = Bleft, B.sharp = Bleft.sharp, delta = 0.25, q12 = 1)

Bright <- RightTailMod(mu.B = mu.B, sigma.B = sigma.B, totT = totT)
Bright.sharp <- RightTailMod(mu.B = mu.B, sigma.B = sigma.B, totT = totT)

plot(Bright[1:1000,1], Bright[1:1000,2], type = 'p')





