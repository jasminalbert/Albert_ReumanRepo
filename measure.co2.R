source("./pop.sim_function.R")

#function computing coexistence measure FROM ONE SET OF NOISE (sym) not 3
#ARGS
# time = length of time series
# d = death rate
# sigma = standard deviation of birthrate
# mu1 = mean of birthrate for invader
# mu2 = mean of birthrate for resident
# N = total spaces
# N1 = invader initial abundance
# dom = dominance threshold 


measure.co2 <- function(time, d, sigma, mu1, mu2, N, N1, dom, hist=FALSE){
	
	#make noise
	rho <- 0.8
	n <- 10^6
	sig <- matrix(c(sigma^2,rep(sigma*sigma*rho,2),sigma^2),2,2)
	B_sym<-mvrnorm(n=n,mu=c(mu1, mu2),Sigma=sig)
	
	#population simulation
	pop <- pop.sim(b= B_sym, N=N, N1=N1, d=d, time = time)
	
	#coexistence period
	coexist <- pop[,1] < dom*N & pop[,1] > (1-dom)*N
	RLE <- rle(coexist)
	co_periods <- RLE$lengths[RLE$values == TRUE]
	
	
	#fraction of time where coexistence was possible
	return(sum(co_periods)/time)
}

#test

sigma <- 6.4
mu1 <- 0.5
mu2 <- 0.5
N <- 50
N1 <- 25
time <- 10000
dom <- 0.99

#make sequence of delta into a list so we can use lapply instead of a loop
delta <- seq (0,1,0.1) 
deltaList <- as.list(delta) 
names(deltaList) <- delta

lapply(deltaList, function(X){measure.co2(time=time, d=X, sigma=sigma, mu1=mu1, mu2=mu2, N=N, N1=N1, dom=dom)})












