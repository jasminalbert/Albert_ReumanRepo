#function to plot simulations side by side to compare effect of ATA contributions

source("./transform2.R")

simsPlot <- function(mudif, delta, sigma, start=1, end=500){
  load("./noise_etc.RData")
  
  #noise
  n <- transform(b_tilde, u_tilde, rho, sigma=sigma, mudif=mudif, b_s=T)
  
  #asymmetric pop
  popA <- popsim(n$b_l1,n$b_l2,N=50,N1=25,delta=delta,M)
  #symmetric pop
  popS <- popsim(n$b_s1,n$b_s2,N=50,N1=25,delta=delta,M)
  plot(popS$N1[start:end], type='l')
  plot(popA$N1[start:end], type='l')
  
}