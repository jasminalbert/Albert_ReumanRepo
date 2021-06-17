#function: popsim2
#ARGS
# B - exponentiated noise T x 2
# N - total spaces available in model
# N1 - spaces occupied by sp1
# delta - death rate

#OUTPUTS - T x 6 data.frame of populations of sp1 and sp2 for each noise regime

popsim2 <- function(B, N, N1, delta, n){
	
	N <- N
	N1 <- N1
	N2 <- N-N1
	
	B1 <- B[,1]
	B2 <- B[,2]
		
	for (t in 1:n){
		
		tot_new_juvs <- (B1[t]*N1[t]) + (B2[t]*N2[t])
		
		N1[t+1] <- (1-delta)*N1[t] + delta*N*((B1[t]*N1[t])/tot_new_juvs)	
		
		N2[t+1] <- (1-delta)*N2[t] + delta*N*((B2[t]*N2[t])/tot_new_juvs)
		
			
	}
	return(data.frame(N1=N1, N2=N2))
}


#competition
competition <- function(B, N, N1, delta, n){
	
	N <- N
	N1 <- N1
	N2 <- N-N1
	
	B1 <- B[,1]
	B2 <- B[,2]
	
	C <- 0
		
	for (t in 1:n){
		
		tot_new_juvs <- (B1[t]*N1[t]) + (B2[t]*N2[t])
		
		N1[t+1] <- (1-delta)*N1[t] + delta*N*((B1[t]*N1[t])/tot_new_juvs)	
		
		N2[t+1] <- (1-delta)*N2[t] + delta*N*((B2[t]*N2[t])/tot_new_juvs)

		C[t] <- tot_new_juvs/(delta*N)
			
	}
	return(C)
}






