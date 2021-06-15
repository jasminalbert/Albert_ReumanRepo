source("./ExtremeTailDep.R")
source("./lottery_normcop.R")
library(MASS)
#combine get noise and lottery function
#outputs storage effects values for 3 cases of bivariate relationships: ERT, sym, ELT

#args
#mn1		mean birth rate of sp 1
#mn2		mean birth rate of sp 2
#sdev	one standard deviation for both columns
#delta	death rate
#n		length of noise 
noise_SE <- function(mn1, mn2, sdev, n, delta, q12=1){
	
	#making noise
	
	#ELT
	B_ELT<-retd(n=n, d=2, rl=-1)
	B_ELT[,1]<-B_ELT[,1]*sdev+mn1
	B_ELT[,2]<-B_ELT[,2]*sdev+mn2

	B_ELT_sharp<-retd(n=n, d=2, rl=-1)
	B_ELT_sharp[,1]<-B_ELT_sharp[,1]*sdev+mn1
	B_ELT_sharp[,2]<-B_ELT_sharp[,2]*sdev+mn2    
    
    #ERT
    B_ERT<-retd(n = n, d = 2, rl = 1)
  	B_ERT[,1]<-B_ERT[,1]*sdev+mn1
  	B_ERT[,2]<-B_ERT[,2]*sdev+mn2
    
    B_ERT_sharp<-retd(n = n, d = 2, rl = 1)
  	B_ERT_sharp[,1]<-B_ERT_sharp[,1]*sdev+mn1
  	B_ERT_sharp[,2]<-B_ERT_sharp[,2]*sdev+mn2
    
    allcors<-c(cor(B_ELT)[1,2],cor(B_ELT_sharp)[1,2],cor(B_ERT)[1,2],cor(B_ERT_sharp)[1,2])

	#sym
	rho<-mean(allcors)
  	sig<-matrix(c(sdev^2,rep(sdev*sdev*rho,2),sdev^2),2,2)
  	B_sym<-mvrnorm(n=n,mu=c(mn1,mn2),Sigma=sig)
  	B_sym_sharp<-mvrnorm(n=n,mu=c(mn1,mn2),Sigma=sig)

	#Noise results
   noiselist<-list(B_ELT=B_ELT, B_ELT_sharp=B_ELT_sharp, B_sym=B_sym, B_sym_sharp=B_sym_sharp, B_ERT=B_ERT, B_ERT_sharp=B_ERT_sharp) 
   
   #exponentiate noise
   xnoise <- lapply(noiselist, exp)
   
   #compute SE using lottery model
   
   #initialize df to store results
   SE <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)
   
	#loop computation for each pair of noise
	for (i in seq(1,5,2)) {
		SE[(i+1)/2,] <- lottery_normcop(xnoise[[i]], xnoise[[i+1]], delta=delta, q12=q12)
	}
	row.names(SE) <- c("ELT", "sym", "ERT")
	return(SE)
}













