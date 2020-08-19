#function for population simulation using lottery model
#ARGS: 
# time = length of time series
# b = a 2xtime matrix of birth rate, ln(B)
# N = total spaces
# N1= invader initial abundance
# d = death rate

pop.sim <- function(b, N, N1, d, time) {
	N  <- N
	N1 <- N1
	N2 <- N-N1
	
	B1 <- exp(b[,1])
	B2 <- exp(b[,2])
	
	for (t in 1:time) {
		N1[t+1] <- (1-d)*N1[t] + d*N*((B1[t]*N1[t])/((B1[t]*N1[t])+																(B2[t]*N2[t])))
		N2[t+1] <- (1-d)*N2[t] + d*N*((B2[t]*N2[t])/((B1[t]*N1[t])+																(B2[t]*N2[t])))
	}
	return(cbind(N1,N2))
}

# test

#popELT <- pop.sim(b=noise$B_ELT, N=50, N1=25, d=1, time=10000)
#plot(popELT[,1], type='l')

















