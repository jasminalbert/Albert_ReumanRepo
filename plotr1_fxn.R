plot_r1 <- function(sigma, mu, delta){
  load("./noise_etc.RData")
  
  b_l1 <- sigma*b_tilde$l[,1] + mu[1]
  b_l2 <- sigma*b_tilde$l[,2] + mu[2]
  r1_l <- r(b_l1-b_l2, delta)
  
  b_r1 <- sigma*b_tilde$r[,1] + mu[1]
  b_r2 <- sigma*b_tilde$r[,2] + mu[2]
  r1_r <- r(b_r1-b_r2, delta)
  
  b_s1 <- sigma*b_tilde$s[,1] + mu[1]
  b_s2 <- sigma*b_tilde$s[,2] + mu[2]
  r1_s <- r(b_s1-b_s2, delta)
  
  #pdf("r1.pdf")
  par(oma=c(2,1,1,1), mfrow=c(3,1), mar=c(2,3,2,1))
  
  
  plot(r1_l[1:100], type='l', ylab='r1(L)',xlab=NULL, col='blue')
  abline(h=mean(r1_l), lty=3, col='red')
  points(b_l1[1:100]-b_l2[1:100], cex=0.75)
  mtext(paste("sd=", round(sd(r1_l),4), "mean=", round(mean(r1_l),4)), 1, cex=0.5, line=-1)
  
  plot(r1_r[1:100], type='l', ylab='r1(R)',xlab=NULL, col='blue')
  abline(h=mean(r1_l), lty=3, col='red')
  points(b_r1[1:100]-b_r2[1:100], cex=0.75)
  mtext(paste("sd=", round(sd(r1_r),4), "mean=", round(mean(r1_r),4)), 1, cex=0.5, line=-1)
  
  plot(r1_s[1:100], type='l', ylab='r1(S)',xlab=NULL, col='blue')
  abline(h=mean(r1_l), lty=3, col='red')
  points(b_s1[1:100]-b_s2[1:100], cex=0.75)
  mtext(paste("sd=", round(sd(r1_s),4), "mean=", round(mean(r1_s),4)), 1, cex=0.5,line=-1)
  
  mtext(paste("sigma=", sigma, "mu=", mu[1],mu[2], "delta=", delta), 3,outer=T, cex=.75, line=-1)
  mtext("timestep",1,outer=T, cex=.75)
  #dev.off()
}