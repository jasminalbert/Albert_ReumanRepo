# function: measure.co
#computes coexistence metrics from co.periods
#ARGS
# co.periods - list of vector of lengths of co periods
# n - total simulation time

#OUTPUT - 3x3 matrix of the 3 metrics for each noise regime

measure.co <- function(co.periods, n){
	
	metrics <- data.frame(co.frac = 1:3, co.num = 1:3, co.mean = 1:3, row.names = c('ELT', 'sym', 'ERT'))
	
	for (i in 1:3){
		metrics$co.frac[i] <- sum(co.periods[[i]])/n
		metrics$co.num[i] <- length(co.periods[[i]])
		metrics$co.mean[i] <- mean(co.periods[[i]])
	}
	return(metrics)
}

#test
source("./popsim.R")
source("./co.periods.R")

testnoise <- matrix(rnorm(1200), ncol=12)

pop <- popsim(exp(testnoise), 50, 25, 0.5)

dom <- 0.90; N <- 50

co.p <- co.periods(pop, dom, N)

metrics <- measure.co(co.p, T=100)



