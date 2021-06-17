
plotdis <- function(b1,b2,...){
  plot(b2, b1, ylab='b1', xlab="b2", pch=16, col="grey",...)
  rangeb1 <- round(range(b1))
  means <- round(c(mean(b1), mean(b2)),1)
  u <- seq(rangeb1[1],rangeb1[2],0.1)
  v <- u + means[2]-means[1]
  lines(v,u, lty=3)
  points(b2[b1>b2],b1[b1>b2], pch=21, col="red", bg="grey")
}

plot_r1 <- function(sigma, mu, delta, plottype){
  load("./noise_etc.RData")
  
  n <- transform(b_tilde, u_tilde, rho, sigma, mu, b_s=T)
  
  l <- r_mets(n$b_l1-n$b_l2, delta, M)
  r <- r_mets(n$b_r1-n$b_r2, delta, M)
  s <- r_mets(n$u_s, delta, M)
  sharp1 <- r_mets(n$u_1, delta, M)
  sharp2 <- r_mets(n$u_2, delta, M)

  DeltaI <- c(l$rbar_hat - sharp1$rbar_hat + sharp2$rbar_hat, 
              r$rbar_hat - sharp1$rbar_hat + sharp2$rbar_hat,
              s$rbar_hat - sharp1$rbar_hat + sharp2$rbar_hat)
  
  invPrb <- c(length(n$b_l1[n$b_l1>n$b_l2])/M, 
              length(n$b_r1[n$b_r1>n$b_r2])/M, 
              length(n$u_s[n$u_s>0])/M)
  
  invE <- c(mean(l$r[l$r>0]),
            mean(r$r[r$r>0]),
            mean(s$r[s$r>0]))
  
  #pdf("r1.pdf")
  par(oma=c(2,1,1,1), mfrow=c(3,1), mar=c(2,4,2,1))
  
  if (plottype==1){
    
    plot(l$r[1:100], type='l', ylab='r1(L)',xlab=NULL, col='blue')
    abline(h=mean(l$r), lty=3, col='red')
    points(n$b_l1[1:100]-n$b_l2[1:100], cex=0.75)
    mtext(paste("sd=", round(sd(l$r),4), "mean=", round(mean(l$r),4)), 3, cex=0.5, line=-1)
    
    plot(r$r[1:100], type='l', ylab='r1(R)',xlab=NULL, col='blue')
    abline(h=mean(r$r), lty=3, col='red')
    points(n$b_r1[1:100]-n$b_r2[1:100], cex=0.75)
    mtext(paste("sd=", round(sd(r$r),4), "mean=", round(mean(r$r),4)), 3, cex=0.5, line=-1)
    
    plot(s$r[1:100], type='l', ylab='r1(S)',xlab=NULL, col='blue')
    abline(h=mean(s$r), lty=3, col='red')
    points(b_s1[1:100]-b_s2[1:100], cex=0.75)
    mtext(paste("sd=", round(sd(s$r),4), "mean=", round(mean(s$r),4)), 3, cex=0.5,line=-1)
    
    mtext("timestep",1,outer=T, cex=.75)
  }
  

  if (plottype==2){
    
    plotdis(n$b_l1[1:750],n$b_l2[1:750])
    mtext(paste("(L)  P[r1>0]=", round(invPrb[1],3), "E(r1|r1>0)=", round(invE[1],3)), 1, cex=0.5, line=-1)
    
    plotdis(n$b_r1[1:750],n$b_r2[1:750])
    mtext(paste("(R)  P[r1>0]=", round(invPrb[2],3), "E(r1|r1>0)=", round(invE[2],3)), 1, cex=0.5, line=-1)
    
    plotdis(b_s1[1:750],b_s2[1:750])
    mtext(paste("(S)  P[r1>0]=", round(invPrb[3],3), "E(r1|r1>0)=", round(invE[3],3)), 1, cex=0.5, line=-1)
    
  }
  
  if (plottype==3){
    source("./pop_sim.R")
    popl<- popsim(b_l1, b_l2, 1000, 1, delta, M)
    popr<- popsim(b_r1, b_r2, 1000, 1, delta, M)
    pops<- popsim(b_s1, b_s2, 1000, 1, delta, M)
    
    plot(popl$N1[1:100], type='l', ylab="N_1 (L)", xlab="")
    plot(popr$N1[1:100], type='l', ylab="N_1 (R)", xlab='')
    plot(pops$N1[1:100], type='l', ylab="N_1 (S)", xlab='')
    
    mtext("timestep",1,outer=T, cex=.75)
  }
  
  
  mtext(paste("sigma=", sigma, "mu=", mu[1],mu[2], "delta=", delta), 3,outer=T, cex=.75, line=-1)
  
  #dev.off()
}

#plot_r1(3.2, c(0.1,0.9), 0.5, plottype=2)
#plot_r1(3.2, c(0.1,0.9), 0.5, plottype=1)
