
source("./decomposition_fxn.R")

#choose deltas 1, 0.8, 0.5
#choose sigmas 2, 4, 6

sigma=5
delta=0.8

dePlot2 <- function(sigma, delta, legend=FALSE,...){
  load("./noise_etc.RData")
  mudif <- seq(0,-0.8,-0.01)
  store <- vector(mode='list', length=length(mudif))
  
  for (i in 1:length(mudif)){
    store[[i]] <- decompose(mudif[i],sigma,delta,b_tilde,u=u_tilde)
  }
  
  range <- range(unlist(lapply(store, function(X){(X$D[6:7])})))
  
  plot(0, xlab="", ylab="", ylim=range*1.2, xlim=c(0,min(mudif)), col="white")
  abline(h=0, col="gray",lwd=0.7)
  
  for (i in 1:length(mudif)){
    if(i>=2){
      dat <- cbind(store[[i-1]]$D, store[[i]]$D)
      rwoATA <- dat[7,1:2] - dat[6,1:2]
      lines(c(mudif[i-1], mudif[i]), dat[6,1:2], col="red")#[E||C]
      lines(c(mudif[i-1], mudif[i]), dat[7,1:2], col="orange",lwd=2)#r
      lines(c(mudif[i-1], mudif[i]), rwoATA, col="forestgreen", lty=4)
      
      if(rwoATA[1] >0 & dat[7,1]<0){
        lines(c(mudif[i-1], mudif[i]), rep(0,2), col="deeppink")
      }
      if(rwoATA[1] <0 & dat[7,1]>0){
        lines(c(mudif[i-1], mudif[i]), rep(0,2), col="yellow")
      }
    }
  }
  
}

#dePlot2(5, 0.8)
#dePlot2(7,1)



#dePlot1(-0.8, 0.8, legend=FALSE)[[6]]$D

