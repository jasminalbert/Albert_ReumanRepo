source("./measure.co.R")

#function to visualize coexistence measures of multiple generations of the same noise generating processes
#coexistence measure tells us what % of time two species are in coexistence according to a dominance threshold

#ARGS (same as measure.co except for n)
# time = length of time series
# d = death rate
# sigma = standard deviation of birthrate
# mu1 = mean of birthrate for invader
# mu2 = mean of birthrate for resident
# N = total spaces
# N1 = invader initial abundance
# dom = dominance threshold 
# n = number of noise generations

co.hist <- function(time, d, sigma, mu1, mu2, N, N1, dom, n){
	
	#initialize list to store measurements
	co.list <- vector(mode='list', length=n)
	
	for(i in 1:n){
		co.list[[i]] <- measure.co(time=time, d=d, sigma=sigma, mu1=mu1, mu2=mu2, N=N, N1=N1, dom=dom)
	}
	
	co_ELT <- unlist(lapply(cox.list, function(X)			   {return(X$B_ELT)}))
	co_sym <- unlist(lapply(cox.list, function(X){return(X$B_sym)}))
	co_ERT <- unlist(lapply(cox.list, function(X)			     {return(X$B_ERT)}))

	maxx <- max(c(co_ELT, co_sym, co_ERT))

	hist(co_ELT, xlim = c(0, maxx), breaks = 10)
	hist(co_sym, xlim = c(0, maxx), breaks = 10)
	hist(co_ERT, xlim = c(0, maxx), breaks = 10)
}
