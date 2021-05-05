rm(list=ls())

library(testthat)

source("getnoise.R")

#***visual checks of taildepcopdat

set.seed(101)
res<-taildepcopdat(.5,.25,.75,1000)  
plot(res[,1],res[,2],type="p")
res<-taildepcopdat(.9,.25,.75,1000)  
plot(res[,1],res[,2],type="p")
res<-taildepcopdat(0,.25,.75,1000)  
plot(res[,1],res[,2],type="p")
res<-taildepcopdat(.9,.1,.75,1000)  
plot(res[,1],res[,2],type="p")
res<-taildepcopdat(.9,.5,.9,1000)  
plot(res[,1],res[,2],type="p")
res<-taildepcopdat(-.9,.5,.9,1000)  
plot(res[,1],res[,2],type="p") #this last one is weird but not wrong - we would not normally use it

#***other tests of tauldepcopdat

set.seed(101)
beta1<-0.1
beta2<-0.75

#test the error catching
expect_error(taildepcopdat(1.1,beta1,beta2,100),"Error in taildepcopdat: bad value of corpar")
expect_error(taildepcopdat(.9,-.1,.5,100),"Error in taildepcopdat: bad values of beta1 and beta2")
expect_error(taildepcopdat(.9,.1,1.1,100),"Error in taildepcopdat: bad values of beta1 and beta2")
expect_error(taildepcopdat(.9,.2,.1,100),"Error in taildepcopdat: bad values of beta1 and beta2")

#test a boundary case with corpar=1
res<-taildepcopdat(1,beta1,beta2,100)
expect_equal(res[,1],res[,2])
  
#test a boundary case with beta1 and beta2 equal
res<-taildepcopdat(.5,.5,.5,100)
expect_equal(res[,1],res[,2])
    
#test a non-boundary example
res<-taildepcopdat(.5,beta1,beta2,100)  
  
#make sure it's the right format
expect_equal(class(res),"matrix")
expect_equal(dim(res),c(100,2))
expect_true(all(res>=0))
expect_true(all(res<=1))
  
#make sure the components are the same when below beta1 or above beta2
inds<-which(res[,1]<=beta1 | res[,1]>=beta2)
expect_equal(res[inds,1],res[inds,2])

#***visual checks of taildepnormdat, not formal unit testing
#formal unit testing not considered necessary for this function 

set.seed(101)  
res<-taildepnormdat(.5,.25,.75,1000)  
plot(res[,1],res[,2],type="p")
hist(res[,1],50)
hist(res[,2],50)
mean(res[,1])
mean(res[,2])

#***informal testing of getcorpar - formal testing would be tricky

corpar<-getcorpar(beta1=.1,beta2=.9,rho=.8,numpts=10000,allowneg=FALSE) 
corpar #so this means if you want taildepnormdat to give noise with correlation .8 and using beta1=.1 and beta2=.9, you need to use corpar equal to this value
#so let's test that
testres<-taildepnormdat(corpar=corpar,beta1=.1,beta2=.9,numpts=100000)
dim(testres)
cor(testres[,1],testres[,2]) #works pretty well

#more informal testing, but now using beta1=0
corpar<-getcorpar(beta1=0,beta2=.5,rho=.7,numpts=10000) 
corpar #so there is no corpar that gives this rho for these values of the betas

#***testing of getnoise

res<-getnoise(mu=c(1,2),sigma=c(3,4),n=10000,corval=.5)
res<-getnoise(mu=c(1,2),sigma=c(3,4),n=10000,corval=.9)

#test for the right format of results
expect_equal(class(res),"list")
expect_equal(names(res),c("LT","sym","RT"))
expect_equal(dim(res$LT),c(10000,2))
expect_equal(dim(res$sym),c(10000,2))
expect_equal(dim(res$RT),c(10000,2))

#now look at the results
hist(res$LT[,1],50)
hist(res$LT[,2],50)
mean(res$LT[,1])
mean(res$LT[,2])
sd(res$LT[,1])
sd(res$LT[,2])
cor(res$LT[,1],res$LT[,2])
plot(res$LT[1:1000,1],res$LT[1:1000,2],type="p")
lines(rep(1,2),c(-100,100))
lines(c(-100,100),rep(2,2))

hist(res$sym[,1],50)
hist(res$sym[,2],50)
mean(res$sym[,1])
mean(res$sym[,2])
sd(res$sym[,1])
sd(res$sym[,2])
cor(res$sym[,1],res$sym[,2])
plot(res$sym[1:1000,1],res$sym[1:1000,2],type="p")
lines(rep(1,2),c(-100,100))
lines(c(-100,100),rep(2,2))

hist(res$RT[,1],50)
hist(res$RT[,2],50)
mean(res$RT[,1])
mean(res$RT[,2])
sd(res$RT[,1])
sd(res$RT[,2])
cor(res$RT[,1],res$RT[,2])
plot(res$RT[1:1000,1],res$RT[1:1000,2],type="p")
lines(rep(1,2),c(-100,100))
lines(c(-100,100),rep(2,2))

#***testing of getnoise again, this time for a very high correlation

res<-getnoise(mu=c(1,2),sigma=c(3,4),n=10000,corval=.99)

#test for the right format of results
expect_equal(class(res),"list")
expect_equal(names(res),c("LT","sym","RT"))
expect_equal(dim(res$LT),c(10000,2))
expect_equal(dim(res$sym),c(10000,2))
expect_equal(dim(res$RT),c(10000,2))

#now look at the results
hist(res$LT[,1],50)
hist(res$LT[,2],50)
mean(res$LT[,1])
mean(res$LT[,2])
sd(res$LT[,1])
sd(res$LT[,2])
cor(res$LT[,1],res$LT[,2])
plot(res$LT[1:1000,1],res$LT[1:1000,2],type="p")

hist(res$sym[,1],50)
hist(res$sym[,2],50)
mean(res$sym[,1])
mean(res$sym[,2])
sd(res$sym[,1])
sd(res$sym[,2])
cor(res$sym[,1],res$sym[,2])
plot(res$sym[1:1000,1],res$sym[1:1000,2],type="p")

hist(res$RT[,1],50)
hist(res$RT[,2],50)
mean(res$RT[,1])
mean(res$RT[,2])
sd(res$RT[,1])
sd(res$RT[,2])
cor(res$RT[,1],res$RT[,2])
plot(res$RT[1:1000,1],res$RT[1:1000,2],type="p")

#***testing of getnoise again, this time for a very low correlation

res<-getnoise(mu=c(1,2),sigma=c(3,4),n=10000,corval=.82)

#test for the right format of results
expect_equal(class(res),"list")
expect_equal(names(res),c("LT","sym","RT"))
expect_equal(dim(res$LT),c(10000,2))
expect_equal(dim(res$sym),c(10000,2))
expect_equal(dim(res$RT),c(10000,2))

#now look at the results
hist(res$LT[,1],50)
hist(res$LT[,2],50)
mean(res$LT[,1])
mean(res$LT[,2])
sd(res$LT[,1])
sd(res$LT[,2])
cor(res$LT[,1],res$LT[,2])
plot(res$LT[1:1000,1],res$LT[1:1000,2],type="p")

hist(res$sym[,1],50)
hist(res$sym[,2],50)
mean(res$sym[,1])
mean(res$sym[,2])
sd(res$sym[,1])
sd(res$sym[,2])
cor(res$sym[,1],res$sym[,2])
plot(res$sym[1:1000,1],res$sym[1:1000,2],type="p")

hist(res$RT[,1],50)
hist(res$RT[,2],50)
mean(res$RT[,1])
mean(res$RT[,2])
sd(res$RT[,1])
sd(res$RT[,2])
cor(res$RT[,1],res$RT[,2])
plot(res$RT[1:1000,1],res$RT[1:1000,2],type="p")
