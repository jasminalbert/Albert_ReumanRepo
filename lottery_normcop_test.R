library(MASS)
library(copula)

source("lottery_normcop.R")

#testing function using parameters from Ellner et al. 2016
mu.B <- c(0.5,0.6)
sigma.B <- c(0.8, 0.8)
rho <- 0.5
totT <- 10^6
sigma <- cbind(c(sigma.B[1]^2,rho*sigma.B[1]*sigma.B[2]),c(rho*sigma.B[1]*sigma.B[2],sigma.B[2]^2))

set.seed(121212)
logB<-mvrnorm(n=totT,mu=mu.B,Sigma=sigma)
B <- exp(logB)
logB.sharp<-mvrnorm(n=totT,mu=mu.B,Sigma=sigma)
B.sharp <- exp(logB.sharp)

lottery_normcop(B = B, B.sharp = B.sharp, delta = 0.25, q12 = 1)

#now try it with moderately left-tail associated noise
ccop<-claytonCopula(param=1.05,dim=2) #make a copula with left-tail assoc
logB_pre<-rCopula(totT,ccop) #get totT data from it
dim(logB_pre)
plot(logB_pre[1:1000,1],logB_pre[1:1000,2],type="p") #plot to see what it looks like, and also look at marginals
hist(logB_pre[,1],50)
hist(logB_pre[,2],50) #not normal
logB<-logB_pre #make logB by magically tranforming logB_pre
logB[,1]<-qnorm(logB_pre[,1],mean=mu.B[1],sd=sqrt(sigma[1,1]))
logB[,2]<-qnorm(logB_pre[,2],mean=mu.B[2],sd=sqrt(sigma[1,1]))
hist(logB[,1],50)
hist(logB[,2],50) #now we have normal marginals, which is what we wanted
mean(logB[,1])
mu.B[1] #so the means are about the same as Ellner's logB
mean(logB[,2])
mu.B[2] #so the means are about the same as Ellner's logB
var(logB[,1])
sigma[1,1]
var(logB[,2])
sigma[2,2] #variance also the same as Ellner's logB
cov(logB[,1],logB[,2]) 
sigma[1,2] #by guess-and-check we got the covariance to be about the same as Ellner's logB
#now we have noise logB with these properties
#1) logB[,1] is normal with mean mu.B[1] and variance sigma[1,1]
#2) logB[,2] is normal with mean mu.B[2] and variance sigma[2,2]
#3) covariance between the columns is also about the same as Ellner's
#4) But we have left-tail association
plot(logB[1:1000,1],logB[1:1000,2],type="p")
B_left <- exp(logB)
#making B.sharp
ccop.sharp<-claytonCopula(param=1.05,dim=2)
logB_pre.sharp<-rCopula(totT,ccop)
logB.sharp<-logB_pre.sharp 
logB.sharp[,1]<-qnorm(logB_pre.sharp[,1],mean=mu.B[1],sd=sqrt(sigma[1,1]))
logB.sharp[,2]<-qnorm(logB_pre.sharp[,2],mean=mu.B[2],sd=sqrt(sigma[1,1]))
B_left.sharp <- exp(logB.sharp)

lottery_normcop(B = B_left, B.sharp = B_left.sharp, delta = 0.25, q12 = 1)
#Jasmin: write a function that gives you your logB, call it twice

#try it with moderately right-tail associated noise
logB_pre<-rCopula(totT,claytonCopula(param=1.05,dim=2))
logB_pre<-(-logB_pre+1)

plot(logB_pre[1:1000,1],logB_pre[1:1000,2],type="p") #plot to see what it looks like, and also look at marginals
logB <- logB_pre
logB[,1] <- qnorm(logB_pre[,1], mean = mu.B[1], sd = 		sigma.B[1])
logB[,2] <- qnorm(logB_pre[,2], mean = mu.B[2], sd = 		sigma.B[2])
plot(logB[1:1000,1],logB[1:1000,2],type="p")

#Jasmin: proceed in the same way as above



#try it with extremely left-tail associated noise


#try it with extremely right-tail associated noise


#****Notes

#learning about the covariance and covariance matrix for a bivariate normal random variable 
class(logB)
dim(logB)
hist(logB[,1],50)
hist(logB[,2],50)
plot(logB[1:1000,1],logB[1:1000,2],type="p")
mean(logB[,1])
mean(logB[,2])
var(logB[,1])
var(logB[,2])
cor(logB[,1],logB[,2])
cov(logB[,1],logB[,2])/(sqrt(var(logB[,1])*var(logB[,2])))
cov(logB[,1],logB[,2])
cor(logB[,1],logB[,2])*(sqrt(var(logB[,1])*var(logB[,2])))
