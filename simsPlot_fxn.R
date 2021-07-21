#function to plot simulations side by side to compare effect of ATA contributions
source("./pop_sim.R")
source("./transform2.R")
source("./co.periods.R")

co.pPlot <- function(co.p, pop, start, end){
  plot(0, xlab="", ylab="", ylim=c(0,50), xlim=c(start,end), col="white")
  
  
  ybottom <- 0
  ytop <- 50
    
  if (co.p$values[1] == FALSE){
    xleft <- sum(co.p$lengths[1]+1)
    c <- 1
  }
    
  c <- 0
  odds <- seq(1,201,2)
  evens <- seq(2,202,2)
  xleft <- 0
    
  for (p in 1:length(co.p$lengths)){
      
    xright <- sum(co.p$lengths[1:(odds[p]+c)])
      
    rect(xleft, ybottom, xright, ytop, col="grey", border=NA)
      
    xleft <- sum(co.p$lengths[1:(evens[p]+c)]) + 1
     
  }
  lines(pop$N1[start:end])

}

simsPlot <- function(mudif, delta, sigma, start=1, end=500){
  load("./noise_etc.RData")
  
  #noise
  n <- transform(b_tilde, u_tilde, rho, sigma=sigma, mudif=mudif, b_s=T)
  
  #asymmetric pop
  popA <- popsim(n$b_l1,n$b_l2,N=50,N1=25,delta=delta,M)
  copA <- co.periods(popA[start:end,], dom=0.95, N=50)
  #symmetric pop
  popS <- popsim(n$b_s1,n$b_s2,N=50,N1=25,delta=delta,M)
  copS <- co.periods(popS[start:end,], dom=0.95, N=50)
  
  co.pPlot(copA, popA, start, end)
  co.pPlot(copS, popS, start, end)
  
}

#simsPlot(0,0.5,6)

