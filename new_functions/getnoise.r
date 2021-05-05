library(copula)

#Generates points from certain bivariate copulas with tail dependence
#
#Args
#corpar           For the part of the distribution where the variables are
#                   not perfectly related they are generated with a bivariate 
#                   normal copula with this parameter. Values between -1 and 1.
#beta1, beta2     These specify where the tail dependence starts in each tail.
#                   Numbers between 0 and 1 with beta1<beta2.
#numpts           Number of draws from the copula desired
#
#Output
#A matrix of dimensions numpts by 2. The two variables are each uniformly
#distributed, and are perfectly correlated to the left of beta1 and to the right
#of beta 2.
#
taildepcopdat<-function(corpar,beta1,beta2,numpts)
{
  #some error catching
  if (corpar>1 || corpar<(-1))
  {
    stop("Error in taildepcopdat: bad value of corpar")
  }
  if (beta1<0 || beta2>1 || beta1>beta2)
  {
    stop("Error in taildepcopdat: bad values of beta1 and beta2")
  }
  
  #get the necessary copula object
  corpar<-matrix(corpar,2,2)
  diag(corpar)<-1
  corpar<-P2p(corpar)
  ncop<-normalCopula(corpar,2,"un")
  
  #generate the initial noise
  res<-rCopula(numpts,ncop)
  
  #impose the tail association
  tdornot<-runif(numpts)
  inds<-(tdornot<beta1 | tdornot>beta2)
  res[inds,]<-tdornot[inds] #observations are same at the rows
  res[!inds,]<-(beta2-beta1)*res[!inds,]+beta1 #reassigning values between betas to be shift mean by beta1, scale variance by beta2-beta1 

  return(res)
}

#Just like the above, but a transformation is applied to make the marginals be normal
#with mean vector mu and standard deviation vector std
#
taildepnormdat<-function(corpar,beta1,beta2,numpts,mu=c(0,0),std=c(1,1))
{
  if (any(std<=0))
  {
    stop("Error in taildepnormdat: bad value for std")
  }
  
  res<-taildepcopdat(corpar,beta1,beta2,numpts)
  res<-qnorm(res)
  res[,1]<-res[,1]*std[1]+mu[1]
  res[,2]<-res[,2]*std[2]+mu[2]
  return(res)
}

#A function which finds the value of corpar to use so that the output of taildepnormdat
#has a particular Pearson correlation
#
#Args
#beta1, beta2         The inputs to taildepnormdat that are intended
#rho                  The desired Pearson correlation
#numpts               The number of points to generate to get the answer. Larger values 
#                       are slower but should be more accurate
#allowneg             Allow negative answers?
#ploton               Display the associated plot?
#
#Output
#A single number which is the value of corpar to use
#A plot is also generated showing Pearson correlation as a function of corpar
#
getcorpar<-function(beta1,beta2,rho,numpts=10000,allowneg=FALSE,ploton=TRUE)
{
  #try a whole range of values of corpar and see what rho you get for each
  if (allowneg==TRUE)
  {
    corparvals<-seq(from=-0.99,by=0.01,to=0.99)
  }else
  {
    corparvals<-seq(from=0,by=0.01,to=0.99)
  }
  rhovals<-NA*numeric(length(corparvals))
  for (counter in 1:length(corparvals))
  {
    dat<-taildepnormdat(corparvals[counter],beta1,beta2,numpts)
    rhovals[counter]<-cor(dat[,1],dat[,2],method="pearson") #pcor of vars at that corpar
  }
  
  #use a regression to get the best values of corpar to use
  rg<-range(corparvals)
  linmod<-lm(rhovals~corparvals)
  cf<-coef(linmod)
  corpar<-(rho-cf[1])/cf[2] #y = mx+b (y is rho)
  
  #make a plot, if desired
  if (ploton==TRUE)
  {
    plot(corparvals,rhovals,type="l",xlab="corpar",ylab="rho")
    lines(rg,rep(rho,2))
    lines(rg,cf[1]+cf[2]*rg,type="l")
    lines(rep(corpar,2),c(-1,1))
  }
  
  #return the value, if one exists
  if (rho<min(rhovals) || rho>max(rhovals))
  {
    return(NA)    
  }
  return(unname(corpar))
}

#Given a desired Pearson correlation, this function uses taildepnormdat in an attempt
#get left-tail, right-tail and symmetric noise with that correlation
#
#Args
#mu             Means of each of the two columns (this is a length-2 vector)
#sigma          Standard deviations of the two columns (length-2 vector)
#n              Number of desired points
#corval         The desired Pearson correlation between the columns
#
#Output - a list with these elements
#LT             An n by 2 matrix with left-tail associated noise (perfect correltion below the means)
#sym            An n by 2 matrix with symmetrically associated noise 
#RT             An n by 2 matrix with right-tail associated noise (perfect correlation above the means)
#
getnoise<-function(mu,sigma,n,corval)
{
  corRT<-getcorpar(0,.5,corval,numpts=100000,allowneg=FALSE,ploton=FALSE)
  #corLT<-getcorpar(.5,1,corval,numpts=100000,allowneg=FALSE,ploton=FALSE) #should be very similar
  if (is.na(corRT))
  {
    stop("Error in getnoise: the desired corval is not possible")
  }
  
  LT<-taildepnormdat(corRT,0,.5,n,mu=mu,std=sigma)
  RT<-taildepnormdat(corRT,.5,1,n,mu=mu,std=sigma)
  sym<-taildepnormdat(corval,0,1,n,mu=mu,std=sigma)
  
  return(list(LT=LT,sym=sym,RT=RT))
}

getnoise2 <- function(mu, sigma, n, corval, corRT){
	
	RT<-taildepnormdat(corRT,0,.5,n,mu=mu,std=sigma)
  	LT<-taildepnormdat(corRT,.5,1,n,mu=mu,std=sigma)
  	sym<-taildepnormdat(corval,0,1,n,mu=mu,std=sigma)

	return(data.frame(LT1=LT[,1], LT2=LT[,2], sym1=sym[,1], sym2=sym[,2], RT1=RT[,1], RT2=RT[,2]))
}






