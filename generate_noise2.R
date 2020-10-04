source("./ExtremeTailDep.R")
require(MASS)

####  lapply friendly version of get_noise()   ####

# This function will return a named list of 6 matrices with these properties
#1) all n by 2 in dimensions
#2) all columns normally distributed
#3) all first columns have mean mn[1] and second columns have mean mn[2]
#4) all first columns have sd sdev[1] and all second columns also have sd sdev[2]
#5) correlation (Pearson) between columns is the same, up to sampling variation, for all matrices
#6) Two matrices show extreme left-tail association, two show moderate left-tail association,
#   two are bivariate normal, two show moderate right-tail association, two show extreme right-
#   tail association. The names in the list indicate which is which.
#
#Args are now one vector A
#A <- (mu1, mu2, sdev, n)

#no check 

#Output - a named list of the matrices
#
get_noise2<-function(A)
{
	mn<-A[1:2]
	sdev<-A[3]
	n<-A[4]
  #***get the extreme tail association noise 

  # for extreme left tail dep.
  B_ELT<-retd(n = n, d = 2, rl = -1)
  B_ELT[,1]<-B_ELT[,1]*sdev+mn[1]
  B_ELT[,2]<-B_ELT[,2]*sdev+mn[2]
  
  B_ELT_sharp<-retd(n = n, d = 2, rl = -1)
  B_ELT_sharp[,1]<-B_ELT_sharp[,1]*sdev+mn[1]
  B_ELT_sharp[,2]<-B_ELT_sharp[,2]*sdev+mn[2]
  
  # for extreme right tail dep.
  B_ERT<-retd(n = n, d = 2, rl = 1)
  B_ERT[,1]<-B_ERT[,1]*sdev+mn[1]
  B_ERT[,2]<-B_ERT[,2]*sdev+mn[2]

  B_ERT_sharp<-retd(n = n, d = 2, rl = 1)
  B_ERT_sharp[,1]<-B_ERT_sharp[,1]*sdev+mn[1]
  B_ERT_sharp[,2]<-B_ERT_sharp[,2]*sdev+mn[2]

  allcors<-c(cor(B_ELT)[1,2],cor(B_ELT_sharp)[1,2],cor(B_ERT)[1,2],cor(B_ERT_sharp)[1,2])
  
  
    #check these are all about the same
    print(allcors) 
    
  
  #***get the symmetric tail association noise
  rho<-mean(allcors)
  sig<-matrix(c(sdev^2,rep(sdev*sdev*rho,2),sdev^2),2,2)
  B_sym<-mvrnorm(n=n,mu=mn,Sigma=sig)
  B_sym_sharp<-mvrnorm(n=n,mu=mn,Sigma=sig)
  

  
  return(list(B_ELT=B_ELT,B_ELT_sharp=B_ELT_sharp,
              B_sym=B_sym,B_sym_sharp=B_sym_sharp,
              B_ERT=B_ERT,B_ERT_sharp=B_ERT_sharp, cors = allcors))  
}

A <- c(0.1,0.9,1.6,10000)
res<-get_noise2(A)

#res<-get_noise(mn=c(.1,.9),sdev=c(1.6,1.6),n=10^6,check = T)

#lapply(res, head)

#checking plots
#plot(res$B_ELT[1:1000,1], res$B_ELT[1:1000,2], main = "A) left-tail association", xlab="u", ylab ="v")
#plot(res$B_ELT_sharp[1:1000,1], res$B_ELT_sharp[1:1000,2])
#plot(res$B_MLT[1:1000,1], res$B_MLT[1:1000,2])
#plot(res$B_MLT_sharp[1:1000,1], res$B_MLT_sharp[1:1000,2])
#plot(res$B_sym[1:1000,1], res$B_sym[1:1000,2], main = "B) symmetric association", xlab="u", ylab ="v")
#plot(res$B_sym_sharp[1:1000,1], res$B_sym_sharp[1:1000,2])
#plot(res$B_MRT[1:1000,1], res$B_MRT[1:1000,2])
#plot(res$B_MRT_sharp[1:1000,1], res$B_MRT_sharp[1:1000,2])
#plot(res$B_ERT[1:1000,1], res$B_ERT[1:1000,2], main = "C) right-tail association", xlab="u", ylab ="v")
#plot(res$B_ERT_sharp[1:1000,1], res$B_ERT_sharp[1:1000,2])






