#source("./math_functions.r")

delta <- 0.5
mu <- seq(0.1,1,0.1)
#mu1 <- sample(mu,1)
#mu2 <- sample(mu,1)
sdev <- seq(0.4,6.4,.4)
#sigma <- rep(sample(sdev,1),2)

bnoise <- getnoise2(mu=c(mu1,mu2), sigma=sigma, n=10^6, corval=corval, corRT=corRT)

L1 <- data.frame(b1=bnoise$LT1[bnoise$LT1<mu1], b2=bnoise$LT2[bnoise$LT2<mu2])
L2 <- data.frame(b1=bnoise$LT1[bnoise$LT1>mu1], b2=bnoise$LT2[bnoise$LT2>mu2])

bnoisesharp <- getnoise2(mu=c(mu1,mu2), sigma=sigma, n=10^6, corval=corval, corRT=corRT)
Bnoise <- exp(bnoise)
Bnoisesharp <- exp(bnoisesharp)

layout(matrix(c(1,1,1,2,2,
				1,1,1,2,2,
				1,1,1,3,3,
				1,1,1,3,3), nrow=4, byrow=TRUE))
plot(bnoise$LT1[1:1000], bnoise$LT2[1:1000])
title(paste("mu1=",mu1," mu2=",mu2," sigma=", sigma[1]))
plot(L2$b1[1:1000], L2$b2[1:1000], cex=0.4)
plot(L1$b1[1:1000], L1$b2[1:1000],cex=0.4)

SEres <- SE_lottery(Bnoise, Bnoisesharp, delta)
semiana_res <- DeltaI(mu1, mu2, sigma[1], delta, 500, -500)
names(semiana_res) <- colnames(SEres)
r1t1 <- log(1-delta+delta*exp(L1$b1-L1$b2)); mean(r1t1)
r1t2 <- log(1-delta+delta*exp(L2$b1-L2$b2)); mean(r1t2)
r1 <- log(1-delta+delta*exp(bnoise$LT1-bnoise$LT2)); mean(r1)

plot(r1[1:1000], col='grey40')
abline(h=mean(r1), col='red', lty=2, lwd=2)
abline(h=mean(r1t1)+mean(r1t2), col='green', lty=2, lwd=2)
plot(r1t2[1:1000], col='grey40',cex=0.4)
abline(h=mean(r1t2), col='red', lty=2, lwd=2)
plot(r1t1[1:1000], col='grey40',cex=0.4)
abline(h=mean(r1t1), col='red', lty=2, lwd=2)

# the sum of means does not equal mean of sums
# the mean of means equals the mean of sums

t1<-rbar1T1(-Inf, 0, mu1, mu2, sigma[1], 0.5)
t2<-rbar1T2(0, 400, mu1, mu2, sigma[1], 0.5)


cat("parameters: mu1=",mu1, " mu2=",mu2," sigma=", sigma[1], "\n\n")

cat("sim results:", "\n") 
print(SEres[1,])
cat("r1t1=",round(mean(r1t1),4)," r1t2=",round(mean(r1t2),4), " r1t1+r1t2=", round(mean(r1t1)+mean(r1t2),4), " mean(r1t1,r1t2)=", round(mean(c(mean(r1t1),mean(r1t2))),4), "\n\n")

cat("semi-analytic results:", "\n")
print(semiana_res)
cat("r1t1=",round(t1,4)," r1t2=",round(t2,4), " r1t1+r1t2=", round(t1+t2,4), " mean(r1t1,r1t2)=", round(mean(c(t1,t2)),4), "\n\n")

cat("Does semi-ana r1t1 times 2 equal sim r1t1? \nDoes semi-ana r1t2 times 3 equal sim r1t2? \n\n")
cat("s-a r1t1 x2 =", round(t1*2,4), "sim r1t1=",round(mean(r1t1),4),"\n")
cat("s-a r1t2 x3 =", round(t2*3,4), "sim r1t2=",round(mean(r1t2),4),"\n")

print(t1/mean(r1t1))
print(t2/mean(r1t2))


#source("./check_mathfunctions.R")




#plot(b1, b2)
#plot(b1-b2)








