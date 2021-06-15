library(copula)

#A function sample from a copula with certain properties
#
#Args
#n            The number of points to draw from the copula
#dim          The dimension of the copula you want
#rho          Controls the strength of association in the "middle part". Higher numbers means stronger 
#               association in that part, 0 means no association in that part
#thresh_lo    A lower threshhold between 0 and 1 and less than thresh_hi. Below this, the components
#               of the copula are perfectly associated. Controls the values over which lower-tail 
#               association is perfect. Set to 0 if you want no special lower-tail association. 
#thresh_hi    An upper threshhold between 0 and 1 and greater than thresh_lo. Above this, the components
#               of the copula are perfectly associated. Controls the values over which upper-tail 
#               association is perfect. Set to 0 if you want no special upper-tail association.   
#
#Output - a n by dim matrix of values between 0 and 1.
#
newcop<-function(n,dim,rho,thresh_lo,thresh_hi)
{
  U<-runif(n)
  ncop<-normalCopula(rho,dim=dim) #this is the copula of a multivariate normal with covariance matrix having 1s on
  #the diagonal and all rhos in all off-diagonal positions
  res<-rCopula(n,ncop)
  res<-(thresh_hi-thresh_lo)*res+thresh_lo
  res[U>thresh_hi,]<-rep(U[U>thresh_hi],times=dim) #this line takes the rows of res for which the corresponding
  #entry of U is bigger than thresh_hi and replaces both entries of those rows by the corresponding value of U
  res[U<thresh_lo,]<-rep(U[U<thresh_lo],times=dim) #this line takes the rows of res for which the corresponding
  #entry of U is smaller than thresh_lo and replaces both entries of those rows by the corresponding value of U
  return(res)
}


#Tests

n<-10000
dim<-3
rho<-0
thresh_hi<-.9
thresh_lo<-.1
res<-newcop(n,dim,rho,thresh_lo,thresh_hi)
dim(res)
plot(res[,1],res[,2],type="p")
plot(res[,1],res[,3],type="p")


