#function for computing storage effects in lottery model
#ARGS
#B 		a dataframe/matrix of exp noise for different noise regimes - very specific format***
#delta	death rate, between 0:1
#q12		scaling factor, default 1

SE_lottery <- function(B, B_sharp,delta, q12=1){
	
	temp <- rep(0, times = 3)
	
	results <- data.frame(noise =c("ELT","sym","ERT"),rbar1 = temp, rbar1_sharp = temp, rbar2_sharp = temp, Delta_B1 = temp,  row.names = c("ELT","sym","ERT"))
	
	for (i in 0:2){
		
		B1 <- B[,1+i*2] #***
		B2 <- B[,2+i*2]
		B1_sharp <- B_sharp[,1+i*2]
		B2_sharp <- B_sharp[,2+i*2]
		
		C <- B2/delta #C = C1 = C2
		
		r1 <- log(1 - delta + B1/C)
		results$rbar1[i+1] <- mean(r1)
		
		r1_sharp <- log(1 - delta + B1_sharp/C)
		results$rbar1_sharp[i+1] <- mean(r1_sharp)
		
		r2_sharp <- log(1 - delta + B2_sharp/C)
		results$rbar2_sharp[i+1] <- mean(r2_sharp)
		
		results$Delta_B1[i+1] <- results$rbar1[i+1] - results$rbar1_sharp[i+1] + q12*results$rbar2_sharp[i+1]
	}
	
	return(results)
}

#test

testnoise <- matrix(rnorm(1200), ncol=12)

SE_lottery(exp(testnoise), delta=0.5)









