source("lottery_normcop.R")


#testing function using parameters from Ellner et al. 2016

mu.B <- c(0.5,0.6)
sigma.B <- c(0.8, 0.8)
rho <- 0.5
totT <- 10^6

sigma <- cbind(c(sigma.B[1]^2,rho*sigma.B[1]*sigma.B[2]),c(rho*sigma.B[1]*sigma.B[2],sigma.B[2]^2))

set.seed(121212)

B <- exp(mvrnorm(n=totT,mu=mu.B,Sigma=sigma))

B.sharp <- exp(mvrnorm(n=totT,mu=mu.B,Sigma=sigma))

lottery_normcop(B = B, B.sharp = B.sharp, delta = 0.25, q12 = 1)