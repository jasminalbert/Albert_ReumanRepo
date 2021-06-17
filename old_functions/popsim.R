#function: popsim
#ARGS
# B - exponentiated noise T x 12
# N - total spaces available in model
# N1 - spaces occupied by sp1
# delta - death rate

#OUTPUTS - T x 6 data.frame of populations of sp1 and sp2 for each noise regime

popsim <- function(B, N, N1, delta){
	
	N <- N
	N1 <- N1
	N2 <- N-N1
	
	n <- nrow(B)
	
	temp <- rep(0, times = n)
	
	pop <- data.frame(N1_ELT = temp, N2_ELT = temp, N1_sym = temp, N2_sym = temp, N1_ERT = temp, N2_ERT = temp)
	
	pop[1,] <- c(N1, N2)
	
	for (i in 0:2){
		
		B1 <- B[,1+i*2] #***
		B2 <- B[,2+i*2]
		
		
		for (t in 1:n){
		
		tot_new_juvs <- ((B1[t]*pop[t, 1+i*2]) + (B2[t]*pop[t, 2+i*2]))
		
		pop[t+1, 1+i*2] <- (1-delta)*pop[t, 1+i*2] + delta*N*((B1[t]*pop[t, 1+i*2])/tot_new_juvs)	
		
		pop[t+1, 2+i*2] <- (1-delta)*pop[t, 2+i*2] + delta*N*((B2[t]*pop[t, 2+i*2])/tot_new_juvs)	

			
		}
	}
	return(pop)
}

#test 

#testnoise <- matrix(rnorm(1200), ncol=12)

#popsim(exp(testnoise), 50, 25, 0.5)







