rm(list=ls())

source("ExtremeTailDep.R")
library(MASS)

# Function to generate different tail dep. noise E(t)

get_noise<-function(mn,sdev,n,rho_given=F){
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
  if(rho_given==F){
    rho<-mean(allcors)
  }else{
    rho<-0.5
  }
  
  #rho=0.5
  sig<-matrix(c(sdev[1]^2,rep(sdev[1]*sdev[2]*rho,2),sdev[2]^2),2,2)
  B_sym<-mvrnorm(n=n,mu=mn,Sigma=sig)
  B_sym_sharp<-mvrnorm(n=n,mu=mn,Sigma=sig)
  
  return(list(rho=rho,B_ELT=B_ELT,B_ELT_sharp=B_ELT_sharp,
              B_sym=B_sym,B_sym_sharp=B_sym_sharp,
              B_ERT=B_ERT,B_ERT_sharp=B_ERT_sharp))  
}

#------------------------------

# write a function to calculate delta_Ib (storage effect)

# Input:
# Bmat = a 2 column matrix for two sp. lottery model, 
#           commonly represent a life-history parameter varies over time as environmental condition changes
# Bmat_sharp = another independent draw similar to Bmat
# delta = 0.25 (default)

# Output:

get_deltaIb<-function(Bmat,Bmat_sharp,delta=0.25){
  
  B1<-Bmat[,1]
  B2<-Bmat[,2]
  
  B1_sharp<-Bmat_sharp[,1]
  B2_sharp<-Bmat_sharp[,2]
  
  C1<-C2<-B2/delta
  
  r1_t<-log(1-delta+(B1/C1))
  r2_t<-log(1-delta+(B2/C2))
  
  r1_sharp_t<-log(1-delta+(B1_sharp/C1))
  r2_sharp_t<-log(1-delta+(B2_sharp/C2))
  
  r1_bar = mean(r1_t)
  r1_sharp = mean(r1_sharp_t)
  
  r2_bar = mean(r2_t)
  r2_sharp = mean(r2_sharp_t)
  
  q12<-1
  
  deltaIb_1 = r1_bar - r1_sharp + q12*r2_sharp
  deltaIb_2 = r2_bar - r2_sharp + q12*r1_sharp
  
  return(list(r1_bar=r1_bar,
              r1_sharp=r1_sharp,
              r2_sharp=r2_sharp,
              deltaIb_1=deltaIb_1,
              deltaIb_2=deltaIb_2))
}

set.seed(1234)
mn<-c(.5,.6)
sdev<-c(0.8,0.8)
#sdev<-c(1.8,1.8)
#res<-get_noise(mn=mn,sdev=sdev,n=10^6,rho_given = F)
res<-get_noise(mn=mn,sdev=sdev,n=10^6,rho_given = T)
res$rho
range(res$B_ELT)
#----------------------------
library(copula)
ccop <- claytonCopula(param = 2, dim = 2)
mypar<-iRho(ccop,rho=res$rho)
ccop<-claytonCopula(param = mypar, dim = 2)
B_MLT_raw<-rCopula(n=10^6,copula = ccop)
B_MLT<-qnorm(B_MLT_raw)
#hist(B_MLT[,2])
B_MLT[,1]<-B_MLT[,1]*sdev[1]+mn[1]
B_MLT[,2]<-B_MLT[,2]*sdev[2]+mn[2]

B_MLT_sharp_raw<-rCopula(n=10^6,copula = ccop)
B_MLT_sharp<-qnorm(B_MLT_sharp_raw)
B_MLT_sharp[,1]<-B_MLT_sharp[,1]*sdev[1]+mn[1]
B_MLT_sharp[,2]<-B_MLT_sharp[,2]*sdev[2]+mn[2]

get_deltaIb(Bmat = exp(B_MLT), Bmat_sharp = exp(B_MLT_sharp), delta = 0.25)
#------------------------------
B_MRT<-1-B_MLT_raw
B_MRT<-qnorm(B_MRT)
B_MRT[,1]<-B_MRT[,1]*sdev[1]+mn[1]
B_MRT[,2]<-B_MRT[,2]*sdev[2]+mn[2]

B_MRT_sharp<-1-B_MLT_sharp_raw
B_MRT_sharp<-qnorm(B_MRT_sharp)
B_MRT_sharp[,1]<-B_MRT_sharp[,1]*sdev[1]+mn[1]
B_MRT_sharp[,2]<-B_MRT_sharp[,2]*sdev[2]+mn[2]

get_deltaIb(Bmat = exp(B_MRT), Bmat_sharp = exp(B_MRT_sharp), delta = 0.25)

#-------------------------------
B_ELT<-exp(res$B_ELT) # caution: check rho
B_ELT_sharp<-exp(res$B_ELT_sharp)
get_deltaIb(Bmat = B_ELT, Bmat_sharp = B_ELT_sharp, delta = 0.25)

#---------------------------------
B_sym<-exp(res$B_sym)
B_sym_sharp<-exp(res$B_sym_sharp)
get_deltaIb(Bmat = B_sym, Bmat_sharp = B_sym_sharp, delta = 0.25)

#-------------------------------------
B_ERT<-exp(res$B_ERT)
B_ERT_sharp<-exp(res$B_ERT_sharp)
get_deltaIb(Bmat = B_ERT, Bmat_sharp = B_ERT_sharp, delta = 0.25)













