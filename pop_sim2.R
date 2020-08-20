time <- 1000

d <- 1
sigma <- 1.6
mu1 <- 0.1
mu2 <- 0.1

noise <-get_noise(mn=c(mu1,mu2),sdev=c(sigma,sigma),n=time,check = F)

resL<- lottery_normcop(exp(noise$B_ELT), exp(noise$B_ELT_sharp), d, 1)
resS<- lottery_normcop(exp(noise$B_sym), exp(noise$B_sym_sharp), d, 1)
resR<- lottery_normcop(exp(noise$B_ERT), exp(noise$B_ERT_sharp), d, 1)

B1_ELT <- exp(noise$B_ELT[,1])
B2_ELT <- exp(noise$B_ELT[,2])

B1_sym <- exp(noise$B_sym[,1])
B2_sym <- exp(noise$B_sym[,2])

B1_ERT <- exp(noise$B_ERT[,1])
B2_ERT <- exp(noise$B_ERT[,2])

N <- 50
N1 <- 25
N2 <- N-N1[1]
r1 <- NA
r2 <- NA

pdf('pop_sim2.pdf')
#Extreme left tail
for (t in 1:time) {
	#N2[t] <- N-N1[t]
	N1[t+1] <- (1-d)*N1[t] + d*N*((B1_ELT[t]*N1[t])/								((B1_ELT[t]*N1[t])+(B2_ELT[t]*N2[t])))
	N2[t+1] <- (1-d)*N2[t] + d*N*((B2_ELT[t]*N2[t])/								((B1_ELT[t]*N1[t])+(B2_ELT[t]*N2[t])))
	
}

C1 <- C2 <- B2_ELT/d
r1 <- log(1-d + B1_ELT/C1)
r2 <- log(1-d + B2_ELT/C2)

N1_ELT <- N1
N2_ELT <- N2

plot(N1_ELT, type = 'l', ylim=c(0,50), xlab = "t", ylab = "N1_ELT")
title(main = paste("t=",time," delta=",d," mu1=",mu1," mu2=",mu2," sigma=", sigma), sub=paste("rbar1=",round(resL[1],5), " SE=",round(resL[4],5)))

lines(N2_ELT, type = 'l', col = 'red')

plot(r1, type = 'l')
plot(r2, type = 'l')
r1_ELT <- mean(r1)
r2_ELT <- mean(r2)

lines(B1_ELT[1:200], type = 'l', lty =2)
lines(B2_ELT[1:200], type = 'l', lty =2, col = 'red')

#symmetric
for (t in 1:time) {
	N1[t+1] <- (1-d)*N1[t] + d*N*((B1_sym[t]*N1[t])/								((B1_sym[t]*N1[t])+(B2_sym[t]*N2[t])))
	N2[t+1] <- (1-d)*N2[t] + d*N*((B2_sym[t]*N2[t])/								((B1_sym[t]*N1[t])+(B2_sym[t]*N2[t])))
	
	
}
C1 <- C2 <- B2_sym/d
r1 <- log(1-d + B1_sym/C1)
r2 <- log(1-d + B2_sym/C2)

N1_sym <- N1
N2_sym <- N2

plot(N1_sym, type = 'l', ylim=c(0,50), xlab="t", ylab="N1_sym")
title(main = paste("t=",time," delta=",d," mu1=",mu1," mu2=",mu2," sigma=", sigma), sub=paste("rbar1=",round(resS[1],5), " SE=",round(resS[4],5)))

lines(N2_sym, col = 'red')
plot(r1, type = 'l')
plot(r2, type = 'l')

r1_sym <- mean(r1)
r2_sym <- mean(r2)

lines(B1_sym, type = 'l', lty =2)
lines(B2_sym, type = 'l', lty =2, col = 'red')

#Extreme right tail
for (t in 1:time) {
	N1[t+1] <- (1-d)*N1[t] + d*N*((B1_ERT[t]*N1[t])/								((B1_ERT[t]*N1[t])+(B2_ERT[t]*N2[t])))
	N2[t+1] <- (1-d)*N2[t] + d*N*((B2_ERT[t]*N2[t])/								((B1_ERT[t]*N1[t])+(B2_ERT[t]*N2[t])))
	
}
C1 <- C2 <- B2_ERT/d
r1 <- log(1-d + B1_ERT/C1)
r2 <- log(1-d + B2_ERT/C2)

N1_ERT <- N1
N2_ERT <- N2

plot(N1_ERT, type = 'l', ylim =c(0,50), xlab = "t", ylab ="N1_ERT")
title(main = paste("t=",time," delta=",d," mu1=",mu1," mu2=",mu2," sigma=", sigma), sub=paste("rbar1=",round(resR[1],5), " SE=",round(resR[4],5)))
lines(N2_ERT, type = 'l', col = 'red')

plot(r1, type = 'l')
plot(r2, type = 'l')
r1_ERT <- mean(r1)
r2_ERT <- mean(r2)

lines(B1_ERT, type = 'l', lty =2)
lines(B2_ERT, type = 'l', lty =2, col = 'red')

##################################################

LTGR <- data.frame(r1 = 1:3, r2 = 1:3)

LTGR$r1 <- c(r1_ELT,r1_sym,r1_ERT)
LTGR$r2 <- c(r2_ELT,r2_sym,r2_ERT)


lottery_normcop(exp(noise$B_ELT), exp(noise$B_ELT_sharp), d, 1)
lottery_normcop(exp(noise$B_sym), exp(noise$B_sym_sharp), d, 1)
lottery_normcop(exp(noise$B_ERT), exp(noise$B_ERT_sharp), d, 1)

plot(noise$B_ELT[,1], col = rgb(0,0,0,0.25), pch = 20, cex=3)
points(noise$B_ELT[,2], col = rgb(1,0,0,0.25), pch = 20, cex=3)


plot(B1_ELT, col = rgb(0,0,0,0.25), pch = 20, cex=3)
points(B2_ELT, col = rgb(1,0,0,0.25), pch = 20, cex=3)


plot(noise$B_sym[,1], col = rgb(0,0,0,0.25), pch = 20, cex=3)
points(noise$B_sym[,2], col = rgb(1,0,0,0.25), pch = 20, cex=3)

plot(B1_sym, col = rgb(0,0,0,0.25), pch = 20, cex=3)
points(B2_sym, col = rgb(1,0,0,0.25), pch = 20, cex=3)

plot(noise$B_ERT[,1], col = rgb(0,0,0,0.25), pch = 20, cex=3)
points(noise$B_ERT[,2], col = rgb(1,0,0,0.25), pch = 20, cex=3)

plot(B1_ERT, col = rgb(0,0,0,0.25), pch = 20, cex=3)
points(B2_ERT, col = rgb(1,0,0,0.25), pch = 20, cex=3)

#####################

#plots
pdf('pop_sim4.1.pdf')

plot(N1_ELT[1:300], type = 'l', ylim=c(0,50), xlab = "t", ylab = "N1_ELT")
title(main = paste("t=",time," delta=",d," mu1=",mu1," mu2=",mu2," sigma=", sigma), sub=paste("rbar1=",round(resL[1],5), " SE=",round(resL[4],5)))

plot(N1_sym[1:300], type = 'l', ylim=c(0,50), xlab="t", ylab="N1_sym")
title(main = paste("t=",time," delta=",d," mu1=",mu1," mu2=",mu2," sigma=", sigma), sub=paste("rbar1=",round(resS[1],5), " SE=",round(resS[4],5)))

plot(N1_ERT[1:300], type = 'l', ylim =c(0,50), xlab = "t", ylab ="N1_ERT")
title(main = paste("t=",time," delta=",d," mu1=",mu1," mu2=",mu2," sigma=", sigma), sub=paste("rbar1=",round(resR[1],5), " SE=",round(resR[4],5)))

dev.off()




