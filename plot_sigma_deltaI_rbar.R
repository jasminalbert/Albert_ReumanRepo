load("./params.RData")
res <- readRDS("./results_numeric/rbar_DeltaI.RDS")

#only consider delta = 0.5
flag <- res$params$X4 == 0.5
Pflag <- res$params[flag,]
DeltaIflag <- res$DeltaI[flag,]
rbarflag <- res$rbar[flag,]


pdf("sigma_DeltaI_d0.5.pdf")
for (i in 1:nrow(mu)){
  plot(sigma, DeltaIflag$X3[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2]],  type = 'l', xlab='', ylab='',
       col="blue")
  lines(sigma, DeltaIflag$X2[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2]], col="red")
  lines(sigma, DeltaIflag$X1[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2]], col="green")   
  abline(h=0.0, lty=3, lwd=.75)
  title(xlab = expression(paste(sigma)), ylab=expression(paste(Delta, "I")), line=2)
  title(main= paste("mu= [", mu[i,1],mu[i,2],"]"), cex.main=0.75, font.main=1, line=1)
}
dev.off() #all pretty similar


pdf("sigma_rbar_d0.5.pdf")
for (i in 1:nrow(mu)){
  plot(sigma, rbarflag$X3[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2]],  type = 'l', xlab='', ylab='',
       col="blue", lty=2)
  lines(sigma, rbarflag$X2[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2]], col="red", lty=2)
  lines(sigma, rbarflag$X1[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2]], col="green", lty=2)   
  abline(h=0.0, lty=3, lwd=.75)
  title(xlab = expression(paste(sigma)), ylab="IGR", line=2)
  title(main= paste("mu= [", mu[i,1],mu[i,2],"]"), cex.main=0.75, font.main=1, line=1)
}
dev.off()

cbind(res$rbar[,1:3], res$DeltaI, res$params)[1:100,]

res$params[round(res$rbar[,1:3],3)==round(res$DeltaI,3),] #rbar is equal to deltaI when means are only
#proves that storage effect is the only contributing mechanism when means are equal 



pdf("sigma_rbar_DeltaI_d0.5.pdf")
for (i in 1:nrow(mu)){
  mx <- DeltaIflag$X3[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2] & Pflag$X1==6.4]
  mx <- mx*1.1
  mn <- rbarflag$X2[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2] & Pflag$X1==sigma[1]]
  mn <- mn*0.85
  plot(sigma, DeltaIflag$X3[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2]],  type = 'l', xlab='', ylab='',
       col="blue", ylim=c(mn,mx))
  lines(sigma, DeltaIflag$X2[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2]], col="red")
  lines(sigma, DeltaIflag$X1[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2]], col="green")   
  
  lines(sigma, rbarflag$X3[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2]], col="blue", lty=2)
  lines(sigma, rbarflag$X2[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2]], col="red", lty=2)
  lines(sigma, rbarflag$X1[Pflag$X2==mu[i,1] & Pflag$X3==mu[i,2]], col="green", lty=2)  
  abline(h=0.0, lty=3, lwd=.75)
  title(xlab = expression(paste(sigma)), ylab="IGR/DeltaI", line=2)
  title(main= paste("mu= [", mu[i,1],mu[i,2],"]"), cex.main=0.75, font.main=1, line=1)
}
dev.off()
