source("./generate_noise.R")
source("./lottery_normcop.R")
source("./pop.sim_function.R")

#function computing coexistence measure
#ARGS
# time = length of time series
# d = death rate
# sigma = standard deviation of birthrate
# mu1 = mean of birthrate for invader
# mu2 = mean of birthrate for resident
# N = total spaces
# N1 = invader initial abundance
# dom = dominance threshold 

measure.co <- function(time, d, sigma, mu1, mu2, N, N1, dom, hist=FALSE){
	
	#make noise
	noise <-get_noise(mn=c(mu1,mu2),sdev=c(sigma,sigma),n=time,check = F)
	
	#population simulation
	noise <- noise[-c(2,4,6)]#remove sharps first
	pop<-lapply(noise, function(X){pop.sim(b=X, N=N, N1=N1, d=d, time=time)})
	
	#coexistence period
	coexist <- lapply(pop, function(X){X[,1] < dom*N & X[,1] > (1-dom)*N})
	RLE <- lapply(coexist, rle)
	co_periods <- lapply(RLE, function(X){X$lengths[X$values == TRUE]})
	
	#histogram
	if (hist)
	{
		maxlist <- lapply(co_periods,max)
		maxval <- max(unlist(maxlist))
		lapply(co_periods, function(X){hist(X, main="histogram of coexistence period length", breaks=10, xlim=c(0,maxval))})
	}
	
	#fraction of time where coexistence was possible
	return(lapply(co_periods, function(X){sum(X)/time}))
}

#test
#set.seed(1223)
#par(mfrow=c(1,1))
#measure.co(time=100000, d=1, sigma=6.4, mu1=0.5, mu2=0.1, N=50, N1=25, dom=.9)