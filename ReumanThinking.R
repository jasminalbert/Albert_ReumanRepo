rm(list=ls())

#***Function

source("ExtremeTailDep.R")

# This function will return a named list of 6 matrices with these properties
#1) all n by 2 in dimensions
#2) all columns normally distributed
#3) all first columns have mean mn[1] and second columns have mean mn[2]
#4) all first columns have sd sdev[1] and all second columns also have sd sdev[2]
#5) correlation (Pearson) between columns is the same, up to sampling variation, for all matrices
#6) Two matrices show extreme left-tail association, two are bivariate normal, and two show extreme 
#right-tail association. The names in the list indicate which is which.
#
#Args
#mn     Length-2 vector with means of columns of matrices
#sdev   Length-2 vector with sds of columns of matrices
#n      Dimension 1 of all the matrices
#
#Output - a named list of the matrices
#
get_noise<-function(mn,sdev,n)
{
  #***get the extreme tail association noise 
  
  # for extreme left tail dep.
  B_ELT<-retd(n = n, d = 2, rl = -1)
  B_ELT[,1]<-B_ELT[,1]*sdev[1]+mn[1]
  B_ELT[,2]<-B_ELT[,2]*sdev[2]+mn[2]
  
  B_ELT_sharp<-retd(n = n, d = 2, rl = -1)
  B_ELT_sharp[,1]<-B_ELT_sharp[,1]*sdev[1]+mn[1]
  B_ELT_sharp[,2]<-B_ELT_sharp[,2]*sdev[2]+mn[2]
  
  # for extreme right tail dep.
  B_ERT<-retd(n = n, d = 2, rl = 1)
  B_ERT[,1]<-B_ERT[,1]*sdev[1]+mn[1]
  B_ERT[,2]<-B_ERT[,2]*sdev[2]+mn[2]
  
  B_ERT_sharp<-retd(n = n, d = 2, rl = 1)
  B_ERT_sharp[,1]<-B_ERT_sharp[,1]*sdev[1]+mn[1]
  B_ERT_sharp[,2]<-B_ERT_sharp[,2]*sdev[2]+mn[2]
  
  allcors<-c(cor(B_ELT)[1,2],cor(B_ELT_sharp)[1,2],cor(B_ERT)[1,2],cor(B_ERT_sharp)[1,2])
  
  #***get the symmetric tail association noise
  rho<-mean(allcors)
  sig<-matrix(c(sdev[1]^2,rep(sdev[1]*sdev[2]*rho,2),sdev[2]^2),2,2)
  B_sym<-mvrnorm(n=n,mu=mn,Sigma=sig)
  B_sym_sharp<-mvrnorm(n=n,mu=mn,Sigma=sig)
  
  return(list(B_ELT=B_ELT,B_ELT_sharp=B_ELT_sharp,
              B_sym=B_sym,B_sym_sharp=B_sym_sharp,
              B_ERT=B_ERT,B_ERT_sharp=B_ERT_sharp))  
}

#***Test it

mn<-c(.5,.6)
sdev<-c(.8,.8)
res<-get_noise(mn=mn,sdev=sdev,n=10^6)
B_ELT<-res$B_ELT
B_ELT_sharp<-res$B_ELT_sharp
B_sym<-res$B_sym
B_sym_sharp<-res$B_sym_sharp
B_ERT<-res$B_ERT
B_ERT_sharp<-res$B_ERT_sharp

#check the histograms are normal, with about the right means and sds in all cases
hist(B_ELT[,1],50,main=paste0("B_ELT[,1], mean=",round(mean(B_ELT[,1]),3),"; sd=",round(sd(B_ELT[,1]),3)))
hist(B_ELT[,2],50,main=paste0("B_ELT[,2], mean=",round(mean(B_ELT[,2]),3),"; sd=",round(sd(B_ELT[,2]),3)))
  
hist(B_ELT_sharp[,1],50,main=paste0("B_ELT_sharp[,1], mean=",round(mean(B_ELT_sharp[,1]),3),"; sd=",round(sd(B_ELT_sharp[,1]),3)))
hist(B_ELT_sharp[,2],50,main=paste0("B_ELT_sharp[,2], mean=",round(mean(B_ELT_sharp[,2]),3),"; sd=",round(sd(B_ELT_sharp[,2]),3)))
  
hist(B_sym[,1],50,main=paste0("B_sym[,1], mean=",round(mean(B_sym[,1]),3),"; sd=",round(sd(B_sym[,1]),3)))
hist(B_sym[,2],50,main=paste0("B_sym[,2], mean=",round(mean(B_sym[,2]),3),"; sd=",round(sd(B_sym[,2]),3)))

hist(B_sym_sharp[,1],50,main=paste0("B_sym_sharp[,1], mean=",round(mean(B_sym_sharp[,1]),3),"; sd=",round(sd(B_sym_sharp[,1]),3)))
hist(B_sym_sharp[,2],50,main=paste0("B_sym_sharp[,2], mean=",round(mean(B_sym_sharp[,2]),3),"; sd=",round(sd(B_sym_sharp[,2]),3)))

hist(B_ERT[,1],50,main=paste0("B_ERT[,1], mean=",round(mean(B_ERT[,1]),3),"; sd=",round(sd(B_ERT[,1]),3)))
hist(B_ERT[,2],50,main=paste0("B_ERT[,2], mean=",round(mean(B_ERT[,2]),3),"; sd=",round(sd(B_ERT[,2]),3)))
  
hist(B_ERT_sharp[,1],50,main=paste0("B_ERT_sharp[,1], mean=",round(mean(B_ERT_sharp[,1]),3),"; sd=",round(sd(B_ERT_sharp[,1]),3)))
hist(B_ERT_sharp[,2],50,main=paste0("B_ERT_sharp[,2], mean=",round(mean(B_ERT_sharp[,2]),3),"; sd=",round(sd(B_ERT_sharp[,2]),3)))

#check the correlations are all the same, to sampling variation
c(cor(B_ELT)[1,2],
  cor(B_ELT_sharp)[1,2],
  cor(B_sym)[1,2],
  cor(B_sym_sharp)[1,2],
  cor(B_ERT)[1,2],
  cor(B_ERT_sharp)[1,2])

#make plots to check tail association
plot(B_ELT[1:500,1],B_ELT[1:500,2],type="p")
points(mn[1],mn[2],col="red")

plot(B_ELT_sharp[1:500,1],B_ELT_sharp[1:500,2],type="p")
points(mn[1],mn[2],col="red")

plot(B_sym[1:500,1],B_sym[1:500,2],type="p")
points(mn[1],mn[2],col="red")

plot(B_sym_sharp[1:500,1],B_sym_sharp[1:500,2],type="p")
points(mn[1],mn[2],col="red")

plot(B_ERT[1:500,1],B_ERT[1:500,2],type="p")
points(mn[1],mn[2],col="red")

plot(B_ERT_sharp[1:500,1],B_ERT_sharp[1:500,2],type="p")
points(mn[1],mn[2],col="red")

#***Now look at the special case of identically distributed B1 and B2 mentioned by Ellner

mn<-c(.5,.5)
sdev<-c(1.8,1.8)
res<-get_noise(mn=mn,sdev=sdev,n=10^7)
B_ELT<-exp(res$B_ELT)
B_ELT_sharp<-exp(res$B_ELT_sharp)
B_sym<-exp(res$B_sym)
B_sym_sharp<-exp(res$B_sym_sharp)
B_ERT<-exp(res$B_ERT)
B_ERT_sharp<-exp(res$B_ERT_sharp)
delta<-.25
r1_ELT<-log(1-delta+delta*B_ELT[,1]/B_ELT[,2])
r1_sym<-log(1-delta+delta*B_sym[,1]/B_sym[,2])
r1_ERT<-log(1-delta+delta*B_ERT[,1]/B_ERT[,2])
mean(r1_ELT)
mean(r1_sym)
mean(r1_ERT)
var(r1_ELT)
var(r1_sym)
var(r1_ERT)
hist(r1_ELT)
hist(r1_sym)
hist(r1_ERT)
#So one thing I notice is the distributions are wildly different, even if their means are
#very similar

mean(delta*(exp(res$B_ELT[,1]-res$B_ELT[,2])-1))
mean(delta*(exp(res$B_sym[,1]-res$B_sym[,2])-1))
mean(delta*(exp(res$B_ERT[,1]-res$B_ERT[,2])-1))

x_ELT<-delta*(exp(res$B_ELT[,1]-res$B_ELT[,2])-1)
x_sym<-delta*(exp(res$B_sym[,1]-res$B_sym[,2])-1)
x_ERT<-delta*(exp(res$B_ERT[,1]-res$B_ERT[,2])-1)
plot(x_ELT[1:500],log(1+x_ELT[1:500]),type="p")
plot(x_sym[1:500],log(1+x_sym[1:500]),type="p")
plot(x_ERT[1:500],log(1+x_ERT[1:500]),type="p")
mean(x_ELT)
mean(x_sym)
mean(x_ERT)
mean(log(1+x_ELT))
mean(log(1+x_sym))
mean(log(1+x_ERT))

hist(log10(B_ELT[,1]/B_ELT[,2]),50)
hist(log10(B_sym[,1]/B_sym[,2]),50)
hist(log10(B_ERT[,1]/B_ERT[,2]),50)

#***Now do some other experimentation
