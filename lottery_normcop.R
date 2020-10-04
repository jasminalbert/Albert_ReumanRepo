 # function to produce storage effect value contributing to coesxistence.
# B and B.sharp are independent but identical matrices representing environmental noise
# delta is death rate
# q12 is scaling factor 

lottery_normcop <- function(B, B.sharp, delta, q12){
	
	B1 <- B[,1]
	B2 <- B[,2]
	
	B1.sharp <- B.sharp[,1]
	B2.sharp <- B.sharp[,2]
	
	C1 <- C2 <- B2/delta
	r1.t <- log(1-delta + B1/C1)
	r2.t <- log(1-delta + B2/C2)
	
	rsharp1.t <- log(1-delta + B1.sharp/C1)
	rsharp2.t <- log(1-delta + B2.sharp/C2)
	
	rbar.1 <- mean(r1.t)
	rsharp.1 <- mean(rsharp1.t)
	rbar.2 <- mean(r2.t)
	rsharp.2 <- mean(rsharp2.t)
	
	Delta.Ib1 <- rbar.1 - rsharp.1 + q12*rsharp.2

  return(c(rbar.1=rbar.1,rsharp.1=rsharp.1,rsharp.2=rsharp.2,Delta.Ib1=Delta.Ib1))
}








