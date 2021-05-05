source("./getnoise.R")
source("./popsim2.R")
source("./co.periods.R")
source("./measure.co3.R")
source("./SE_lottery.R")

###########################################################################

# we do not quwation any facts reported by PFCS, but we argue that they empahasized part of a larger picture, and therefore overstated implications for past and future research. 

# the two-species lottery model illustrates why higher IGRs do not necessarily yeild more robust persistence.
# IGR_1 (the IGR of the weaker comepetitor) increases with environmental variability, sigma (the temporal standard deviation of log per capita fecundity).
		#IGR_2 = 0 in lottery model
# The weaker competitor quickly goes extinct when IGR_1<0 (sigma=0.2) but not when IGR_1>0.

n <- 10^6

corval <- 0.88
corRT <- getcorpar(0,.5,corval,numpts=100000,allowneg=FALSE,ploton=FALSE); corRT

mu1 <- 0.45
mu2 <- 0.5
sigma <- 1.1

#generate noise
noise_b <- getnoise2(mu = c(mu1,mu2), sigma = c(sigma, sigma), n=n, corval=corval, corRT=corRT)
noise_b_sharp <- getnoise2(mu = c(mu1,mu2), sigma = c(sigma, sigma), n=n, corval=corval, corRT=corRT)
	
#exponentiate noise
noise_B <- exp(noise_b)
noise_B_sharp <- exp(noise_b_sharp)
		
SE <- SE_lottery(noise_B, noise_B_sharp, 0.5); SE

popLT <- popsim2(B=cbind(noise_B$LT1, noise_B$LT2), N=50, N1=25, delta = 0.5, n=n)
popsym <- popsim2(B=cbind(noise_B$sym1, noise_B$sym2), N=50, N1=25, delta = 0.5, n=n)
popRT <-popsim2(B=cbind(noise_B$RT1, noise_B$RT2), N=50, N1=25, delta = 0.5, n=n)

plot(popLT$N1[1:2000], type = 'l', main="Asymetric (left)"); abline(h=1, col="red")
plot(popsym$N1[1:2000], type = 'l', main="Symetric"); abline(h=1, col="red")
plot(popRT$N1[1:2000], type = 'l', main="Asymetric (right)"); abline(h=1, col="red")

m <- (1/0.55) - 1
50^m

round(popLT$N1[1:500],3)
round(popsym$N1[1:500],3)
round(popRT$N1[1:500],3)



