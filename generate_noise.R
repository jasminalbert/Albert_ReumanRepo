#------------------------------------------------------------------------------------------------------------
# This script will save a list of 6 matrices (two for each: normal bivariate, extreme LT, extreme RT)
# These matrices are meant to be used as input environmental noise in storage effect delta_Ib calculation later.
# For ref: see https://onlinelibrary.wiley.com/doi/abs/10.1111/ele.12672
#------------------------------------------------------------------------------------------------------------
set.seed(seed=121212)
source("./ExtremeTailDep.R")
#-----------------------
# mean
mn<-c(0.5,0.6)

#standard deviation
sdev<- c(0.8,0.8)

# bivariate noise
d<-2

# number of points to generate
n<-10^6


# call the function

# for extreme right tail dep.
B_RT<-retd(n = n, d = d, rl = 1, mn = mn, sdev = sdev)
B_RT_sharp<-retd(n = n, d = d, rl = 1, mn = mn, sdev = sdev)

# for extreme left tail dep.
B_LT<-retd(n = n, d = d, rl = -1, mn = mn, sdev = sdev)
B_LT_sharp<-retd(n = n, d = d, rl = -1, mn = mn, sdev = sdev)

# correlation between two matrices: B_RT, B_RT_sharp
require("Hmisc")
sr<-rcorr(x=B_RT,y=B_RT_sharp,type="pearson") 
cor_RT<-sr$r
sr$P

# correlation between two matrices: B_LT, B_LT_sharp
sl<-rcorr(x=B_LT,y=B_LT_sharp,type="pearson") 
cor_LT<-sl$r
sl$P

# ok, so, I checked : pearson correlation between two columns for each tail-dep. matrix 
#                        remains same (upto sampling variation) ~ 0.897
c(cor_RT[1,2],cor_RT[3,4],cor_LT[1,2],cor_LT[3,4]) 


# now get bivariate normal, as Ellner used
require(MASS)
sigma.B <- sdev
rho<-0.897
sigma <-cbind(c(sigma.B[1]^2,rho*sigma.B[1]*sigma.B[2]),
              c(rho*sigma.B[1]*sigma.B[2],sigma.B[2]^2))

B <- mvrnorm(n=n,mu=mn,Sigma=sigma)

B_sharp <- mvrnorm(n=n,mu=mn,Sigma=sigma)

s<-rcorr(x=B,y=B_sharp,type="pearson")
s$r
s$P


all_noise_list<-list(B=B,
                     B_sharp=B_sharp,
                     B_LT=B_LT,
                     B_LT_sharp=B_LT_sharp,
                     B_RT=B_RT,
                     B_RT_sharp=B_RT_sharp)


saveRDS(all_noise_list,"all_noise_list.RDS")


