source("./new_functions/getnoise.r")
source("./new_functions/SE_lottery.R")
#install.packages("cubature")
library(cubature)
#dan suggests
# 1. Finish math
# 2. Write functions that take model parameters to integrate
# 3. Write functions from those functions to compute semi-analytic quantities that we did using the simulation method

# Storage effect equation:
# DeltaI = rbar1 - rbar1sharp + rbar2sharp
# rbar1 = ln(1-delta+delta*exp(b1-b2))
# rbar1sharp = ln(1-delta+delta*exp(b1sharp-b2))
# rbar2sharp = ln(1-delta+delta*exp(b2sharp-b2))

# the joint distributtion of b1 and b2 makes a bivariate normal distribution with means (mu1, mu2) and var matrix (sigma, 0, 0, sigma)

# to compute analytically we take the expected value of the growth rates
# to take expected value, we take the integral of the product of the growth rate and the pdf of the random variables

# rbar1sharp:
# write b1sharp-b2 = u so that:
# r1sharp = ln(1-delta+delta*exp(u))
# then the pdf becomes univariate and is:
# phi(u) = (1/(sigma*sqrt(2*pi))*exp((-1/2)*((u-mu)/sigma)^2))
# a normal distribution with mu = mu1-mu2 and sigma = sqrt(2)sigma

# rbar2sharp:
# write b2sharp-b2 = u so that:
# r2sharp = ln(1-delta+delta*exp(u))
# then the pdf becomes univariate and is:
# phi(u) = (1/(sigma*sqrt(2*pi))*exp((-1/2)*((u-mu)/sigma)^2))
# a normal distribution with mu = mu2-mu2 = 0 and sigma = sqrt(2)sigma
# then, phi(u) = (1/(sigma*sqrt(2*pi))*exp((-1/2)*(u/sigma)^2))

# rbar1 is split into two terms: 
# T1 - perfectly correlated half, T2 - uncorrelated half

# rbar1T1:
# because it is a line, b1-b2 = mu1-mu2 so that:
# r1t1 = ln(1-delta+delta*exp(mu1-mu2))
# pdf is normal with mean 0 and variance sigma^2
# mean is zero because we are trying to integrate to half of the distribution by integrating to/from 0
# for 0 to be the middle everytime, we let the mean be zero
# ie the value would be the same if we integrate to 0.5 and the mean was 0.5

# rbar1T2:
# why cant we do the same u substitution as the rbars? 



# rbarsharp function - can use for both rbar1sharp and rbar2sharp
integrand_rsharp <- function(u, mu1, mu2, sigma, delta){
	mu <- mu1-mu2
	sigma <- sqrt(2)*sigma
	pdf1 <- (1/(sigma*sqrt(2*pi))*exp((-1/2)*((u-mu)/sigma)^2))
	gr <- log(1-delta+delta*exp(u))
	return(pdf1*gr)
}

rbarsharp <- function(lower, upper, mu1, mu2, sigma, delta){
	int <- integrate(integrand_rsharp, lower, upper, mu1, mu2, sigma, delta)
	return(int$value)
}

rbarsharp(-500, 500, 0.7, 0.6, 1.6, 0.5)
rbarsharp(-200, 200, 0.1, 0.9, 1.4, 0.5)

integrate( function(u){log(0.5+0.5*exp(u))*(exp((-1/2)*((u-(0.1))/2.262742)^2)/(2*pi*2.262742))}, -500, 500)

#names(res)
#res$abs.error
#res$subdivisions
#res$message
#res$call

#see curve
x <- seq(-500,500,1)
y <- integrand_rsharp(x, mu1=0.5, mu2=0.5, sigma=1.6, delta=0.5)
plot(x,y,type='l')


# findings:
# for rbarsharp is variance small and mu1=mu2, =0
	# increasing mu1 and increasing variance increases rbarsharp
	# increasing mu2 decreases rbarsharp
	# changing mu1-mu2 changing min and max of "signal"
	
# r1T1 function
integrand_r1T1 <- function(u, mu1, mu2, sigma, delta){
	pdf1 <- (1/(sigma*sqrt(2*pi))*exp((-1/2)*(u/sigma)^2))
	gr <- log(1-delta+delta*exp(mu1-mu2))
	return(pdf1*gr)
}

rbar1T1 <- function(lower, upper, mu1, mu2, sigma, delta){
	int <- integrate(integrand_r1T1, lower, upper, mu1, mu2, sigma, delta)
	return(int$value)
}

rbar1T1(-Inf, 0, 0.2, 0.2, sigma[1], 0.5)
#equals zero if means are equal bc of gr function
#less than zero if mu1<mu2
#more than zero if mu1>mu2
#only depends on difference not on actual values
#changing lower bonf from -500 to -Inf does not change value

gr_r1T1 <- function(mu1, mu2, delta){
	gr <- log(1-delta+delta*exp(mu1-mu2))
	return(gr)
}

pdf1 <- function(u, mu, sigma){
	pdf1 <- (1/(sigma*sqrt(2*pi))*exp((-1/2)*((u-mu)/sigma)^2))
	return(pdf1)
}
integrate(pdf1, -Inf, 1, mu=1, sigma=1)

#see curve
#x <- seq(-500,0,1)
#y <- integrand_r1T1(x, mu1=0.2, mu2=0.2, sigma=4, delta=0.5)
#plot(x,y,type='l')


#g <- gr_r1T1(mu, mu2=0.5, 0.5)
#plot(mu, g,type='l')

#x_ <- seq(-250,250,1)
#h<- pdf1(x_, 0, 2)
#plot(x_, h, type='l')

# i think r1t1 is correct, the problem must be with r1t2

# r1T2
integrand_r1T2 <- function(vars, mu1, mu2, sigma, delta){
	u1 <- vars[1]
	u2 <- vars[2]
	#u1 = b1-mu1; u2 = b2-mu2; so pdf has mean 0
	pdf2 <- exp((-1/2)*(((u1)/sigma)^2+((u2)/sigma)^2))/(2*pi*sigma*sigma)
	grt2 <- log(1-delta+delta*exp(u1-u2+mu1-mu2))
	return(pdf2*grt2)
}

integrand_r1T22 <- function(vars, mu1, mu2, sigma, delta){
	b1 <- vars[1]
	b2 <- vars[2]
	#u1 = b1-mu1; u2 = b2-mu2; so pdf has mean 0
	pdf2 <- exp((-1/2)*(((b1-mu1)/sigma)^2+((b2-mu2)/sigma)^2))/(2*pi*sigma*sigma)
	grt2 <- log(1-delta+delta*exp(b1-b2))
	return(pdf2*grt2)
}

rbar1T2 <- function(lower, upper, mu1, mu2, sigma, delta){
	int <- cuhre(integrand_r1T2, lowerLimit=c(lower, lower), upperLimit=c(upper, upper), mu1=mu1, mu2=mu2, sigma=sigma, delta=delta)
	return(int$integral)
}

rbar1T2(0, 350, 0.5, 0.4, 1.6, 0.5)

# see 3d plot
# function to plot
integrand_r1T2_2 <- function(u1,u2, mu1, mu2, sigma, delta){
	#u1 = b1-mu1; u2 = b2-mu2; so pdf has mean 0
	pdf2 <- exp((-1/2)*(((u1)/sigma)^2+((u2)/sigma)^2))/(2*pi*sigma*sigma)
	grt2 <- log(1-delta+delta*exp(u1-u2+mu1-mu2))
	return(pdf2*grt2)
}

x <- seq(-3,3,0.2)
y <- seq(-3,3,0.2)
z <- outer(x,y,integrand_r1T2_2, sigma=2, delta=0.5, mu2=0.2, mu1=0.2)

persp(x,y,z, theta=-60, phi=25, ltheta=25, ticktype='detailed')
vars <- cbind(x,y,z)
colnames(z) <- x
rownames(z) <- x
round(z*10,2)

# bivariate pdf to plot
pdf2_2 <- function(u1, u2, sigma){
	pdf2 <- exp((-1/2)*(((u1)/sigma)^2+((u2)/sigma)^2))/(2*pi*sigma*sigma)
	return(pdf2)
}

# bivariate pdf to integrate
pdf2_U <- function(U, sigma){
	u1 <- U[1]
	u2 <- U[2]
	pdf2 <- exp((-1/2)*(((u1)/sigma)^2+((u2)/sigma)^2))/(2*pi*sigma*sigma)
	return(pdf2)
}

x<- seq(mu1-3, mu1+3,0.1)
y<- seq(mu2-3, mu2+3,0.1)
p <- outer(x,y,pdf2_2, sigma=2)
persp(x,y,p, theta=45, phi=25, ticktype='detailed')

cuhre(pdf2_U, lowerLimit=c(0,0), upperLimit=c(Inf,Inf), sigma=2)




# ive written functions for each separate component of DeltaI
# now combine

# DeltaI
DeltaI <- function(mu1, mu2, sigma, delta, upper, lower){ #need to figure out bounds before can finish this function
# right now suited for ELT only; can be editted to suit both ELT and ERT
	
	rbar1T1 <- rbar1T1(lower, 0, mu1, mu2, sigma, delta)
	rbar1T2 <- rbar1T2(0, upper, mu1, mu2, sigma, delta)
	rbar1sharp <- rbarsharp(lower, upper, mu1, mu2, sigma, delta)
	rbar2sharp <- rbarsharp(lower, upper, mu2, mu2, sigma, delta)
	rbar1 <- rbar1T1+rbar1T2
	DeltaI <- rbar1-rbar1sharp+rbar2sharp
	return(c(rbar1, rbar1sharp, rbar2sharp, DeltaI))
}

#DeltaI(0.5, 0.4, 1.6, 0.5, 500, -500)

#CHECK
#corval <- 0.818
#corRT <- getcorpar(0,.5,corval,numpts=100000,allowneg=FALSE,ploton=FALSE)
#corRT

#mu <- seq(0.1,1,0.1)
#mu1 <- sample(mu,1)
#mu2 <- sample(mu,1)
#sdev <- seq(0.4,6.4,.4)
#sigma <- rep(sample(sdev,1),2)

#bnoise <- getnoise2(mu=c(mu1,mu2), sigma=sigma, n=10^6, corval=corval, corRT=corRT)
#bnoisesharp <- getnoise2(mu=c(mu1,mu2), sigma=sigma, n=10^6, corval=corval, corRT=corRT)
#Bnoise <- exp(bnoise)
#Bnoisesharp <- exp(bnoisesharp)

#SEres <- SE_lottery(Bnoise, Bnoisesharp, delta=0.5)
#semiana_res <- DeltaI(mu1, mu2, sigma[1], 0.5, 500, -500)

#cat("mu1=",mu1, " mu2=",mu2," sigma=", sigma[1])
#print(semiana_res)
#print(SEres[1,])







