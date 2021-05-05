source("./new_functions/getnoise.r")
source("./new_functions/SE_lottery.R")
source("./new_functions/measure.co3.R")
source("./new_functions/popsim2.R")
source("./new_functions/co.periods.R")

#solving IGR expected value

# adding and subtracting normals
x1 <- rnorm(10^5, 0.5, 3.2)
x2 <- rnorm(10^5, 0.3, 3.2)
mean(x1);sd(x1)
mean(x2);sd(x2)
hist(x1, 50)
hist(x2,50)
X <- x1-x2
hist(X,50)
mean(X); sd(X)
xp1 <- X+x2 # X = x1-x2, x1 =? X+x2?
hist(xp1)
mean(xp1); sd(xp1)
var(x2)
var(X)
var(xp1)
cov(x2, X)

xp2 <- x1-X
mean(xp2);sd(xp2)

# expected value of log normal 
# if X is a normal random var and Y = exp(X) then Y is lognormal
# in our equation we have B1/B2 where Bi = exp(bi) and bi is normal bivariate with means mu1 and mu2 and sd sigma 
# thus B1/B2 = exp(b1)/exp(b2) = exp(b1-b2)
# subtracting normal distributions gives us a normal distribution
# let b1-b2 = a where a~N(mu1-mu2, 2sigma^2)
# let Q = exp(a) so Q = exp(b1-b2)=B1/B2

mu1 <- 0.5
mu2 <- 0.3
sigma <- 0.8
b1 <- rnorm(10^5, mu1, sigma); var(b1)
b2 <- rnorm(10^5, mu2, sigma); var(b2)
a <- b1-b2
mean(a);var(a);sd(a)
Q <- exp(a)

# if X is a normal random var with mean mu and sd sigma and Y = exp(X) then Y is lognormal with first moment (expected value) e^(mu + (sigma^2)/2)
# since a=b1-b2 its mean is mu1-mu2 and sd is sqrt(2)sigma
# so expcted value of Q=exp(a) is e^((mu1-mu2) + sigma^2

Z <- rnorm(10^7,0,1)
Q <- exp((mu1-mu2) + sqrt(2)*sigma*Z) #same as exp(a)
sigmaa <- sqrt(2)*sigma

mean((mu1-mu2) + sqrt(2)*sigma*Z); sd((mu1-mu2) + sqrt(2)*sigma*Z) # mean and sd of a 
#somehow setting Q as exp(a) vs Q as exp of the linear transformation of Z to a is different 
# second way better? 
#expected value of Q
exp(0.2 + (sigmaa^2)/2 ) #EXPECTED VALUE OF LOGNORMAL 
mean(Q)
EQp <- 0
for (i in 1:100){
	Z <- rnorm(10^7,0,1)
	Q <- exp((mu1-mu2) + sqrt(2)*sigma*Z)
	EQ[i] <- mean(Q)
}

mean(EQp)
#computed mean becomes more accurate as simulations increase
#computed variance
# variance (second moment) of lognormal, like the expected value (first moment) is computed by using the paramters of the normal 
# variance of lognormal is (exp(sigma^2)-1)exp(2*mu+sigma^2)
(exp(sigmaa^2)-1)*exp(2*0.2+sigmaa^2) # VARIANCE OF LOGNORMAL 
var(Q) #yes 

# expected value transformation of log 
# E log(a + N) ~= log(a + EN) + (VarN/2(a+EN)^2)
# given that r1 = ln(1-delta + delta(exp(b1-b2))) find Er1
# we already said that b1-b2=a and Q=exp(a)
# we now know how to find E and Var of Q given paramters from a 
# Er1 = E[ln(1-delta + delta*Q)]
#	  = ln(1-delta + delta*EQ) + (delta*VarQ)/2(1-delta + delta*EQ)^2

delta <- 0.5
EQ <- exp(0.2 + (sigmaa^2)/2 ) #EXPECTED VALUE OF LOGNORMAL
VarQ <- (exp(sigmaa^2)-1)*exp(2*0.2+sigmaa^2) # VARIANCE OF LOGNORMAL 


Er1 <- log(1-delta + delta*EQ) + ((delta*VarQ)/(2*(1-delta + delta*EQ)^2))

#check
params <- c(0.5, 0.3, 0.8)

corval <- 0.88
corRT <- getcorpar(0,.5,corval,numpts=100000,allowneg=FALSE,ploton=FALSE)

bnoise <- getnoise2(mu=params[1:2], sigma=rep(params[3],2), n=10^7, corval=corval, corRT=corRT)
Bnoise <- exp(bnoise)
bnoise_ <- getnoise2(mu=params[1:2], sigma=rep(params[3],2), n=10^7, corval=corval, corRT=corRT)
Bnoise_ <- exp(bnoise_)

SE_lottery(Bnoise, Bnoise_, delta)

sb1 <- bnoise$sym1
sb2 <- bnoise$sym2
sB1 <- Bnoise$sym1
sB2 <- Bnoise$sym2
mean(sb1);mean(sb2)
sd(sb1);sd(sb2);var(sb1);var(sb2)

# checking combinations of normal vars
sa <- sb1-sb2
mean(sa);sd(sa);var(sa) #mean checks out but not var bc
cov(sb1, sb2) #b1 and b2 are not uncorrelated!
sQ <- exp(sa)
mean(sQ);sd(sQ);var(sQ)

sC <- sB1/sB2 #competition is in this form 
mean(sC);sd(sC);var(sC) #checking same as sQ (properties of log)

lb1 <- bnoise$LT1
lb2 <- bnoise$LT2
plot(lb1[1:1000], lb2[1:1000])

plot(lb1[lb1<=0.5][1:1000], lb2[lb2<=0.3][1:1000])
cov(lb1[lb1<0.5],lb2[lb2<0.3]) #0.0777

#can we get uncorrelation
corval <- 0.818
corRT <- getcorpar(0,.5,corval,numpts=100000,allowneg=FALSE,ploton=FALSE)
corRT
bnoise <- getnoise2(mu=c(0.8,0.9), sigma=rep(1.6,2), n=10^6, corval=corval, corRT=corRT)
bnoiseSH <- getnoise2(mu=c(0.8,0.9), sigma=rep(1.6,2), n=10^6, corval=corval, corRT=corRT)
lb1 <- bnoise$RT1
lb2 <- bnoise$RT2
lb1s <- bnoiseSH$RT1
lb2s <- bnoiseSH$RT2
sb1 <- bnoise$sym1
sb2 <- bnoise$sym2

cor(lb1[lb1<0.8],lb2[lb2<0.9])
cov<-cov(lb1[lb1>0.8],lb2[lb2>0.9])

pdf("02032021_noisecorplots.pdf")
plot(lb1[1:3000], lb2[1:3000], main="extreme left tail noise; b1, b2", sub="mu1=0.8, mu2=0.9, sigma=1.6", xlab="b1", ylab="b2")
plot(lb1s[1:3000], lb2[1:3000], main="extreme left tail noise; b1#, b2", sub="mu1=0.8, mu2=0.9, sigma=1.6", xlab="b1#", ylab="b2")
pdf("b2sharpb2.pdf")
plot(lb2s[1:3000], lb2[1:3000], main="extreme left tail noise; b2#, b2", sub="mu1=0.8, mu2=0.9, sigma=1.6", xlab="b2#", ylab="b2")
dev.off()
plot(lb1[lb1>0.8][1:1000], lb2[lb2>0.9][1:1000])

#3D plot
install.packages("GA")
library(GA)

#input
mu1<-0.8 #mean of X_1
mu2<-0.9 #mean of X_2
sigma11<-0.8^2 #variance of X_1
sigma22<-sigma11 #variance of X_2
sigma12<-0.635 #covariance of X_1 and X_2 

#plot
x1 <- seq(mu1-3, mu1+3, length= 500)
x2 <- seq(mu2-3, mu2+3, length= 500)
z <- function(x1,x2){ z <- exp(-(sigma22*(x1-mu1)^2+sigma11*(x2-mu2)^2-2*sigma12*(x1-mu1)*(x2-mu2))/(2*(sigma11*sigma22-sigma12^2)))/(2*pi*sqrt(sigma11*sigma22-sigma12^2)) }
f <- outer(x1,x2,z)
persp3D(x1, x2, f, theta = 30, phi = 30, expand = 0.5)


library(plotly)
f <- t(outer(x1,x2,z))

plot_ly(x=x1,y=x2,z=f,type = "contour")%>%layout(xaxis=list(title="x1"),yaxis=list(title="x2"))



#input
mu1<-0.2 #mean of X_1
mu2<-0.2 #mean of X_2
sigma11<-2^2 #variance of X_1
sigma22<-sigma11 #variance of X_2
sigma12<-0 #covariance of X_1 and X_2

#plot
x1 <- seq(mu1-3, mu1+3,0.1)
x2 <- seq(mu2-3, mu2+3,0.1)

z <- function(x1,x2){ 
	z <- exp((-((x1-mu1)^2)/(2*sigma11))-(((x2-mu2)^2)/(2*sigma22)))/(2*pi*sqrt(sigma11*sigma22))}

z <- function(X){ 
	x1 <- X[1]
	x2 <- X[2]
	z <- exp((-((x1-mu1)^2)/(2*sigma11))-(((x2-mu2)^2)/(2*sigma22)))/(2*pi*sqrt(sigma11*sigma22))}

cuhre(z, lowerLimit=c(0.2,0.2), upperLimit=c(Inf,Inf))

f <- outer(x1,x2,z)
persp(x1,x2,f, theta=45, phi=25, ticktype='detailed')
persp3D(x1, x2, f, theta = 30, phi = 30, expand = 0.5)


g <- function(x1,x2){g <- z*(log(1-delta+delta*exp(x1-x2)))}
delta <- 0.5
f <- outer(x1,x2,g)
persp3D(x1, x2, f, theta = 30, phi = 30, expand = 0.5)




# to plot a 3D function:

# define domain 

b1 <- seq(-4,4, length=600)
b2 <- seq(-4,4, length=600)

# define function

r <- function(b1,b2,delta=0.5) {
	r <- log(1-delta + delta*(exp(b1-b2)))
}
#use outer product function?
f <- outer(b1,b2,r)
persp3D(b1,b2,f)

#product function
Er <- function(b1,b2,delta=0.5,mu1=0.6,mu2=0.5,sigma=1.6){
	r <- log(1-delta + delta*(exp(b1-b2)))
	pdf <- (1/(2*pi*(sigma^2)))*exp((-1/(2*(sigma^2)))*((b1-mu1)^2)+(b2-mu2)^2)
	Er <- r*pdf
}

int <- outer(b1,b2,Er)
persp3D(b1,b2,int)




int <- outer(b1,b2,Er)
persp3D(b1,b2,int)


b1line <- function(b2, mu1=0.6, mu2=0.5){
	b1line <- mu1 + (b2-mu2)
}
b2line <- function(b1, mu1=0.6, mu2=0.5){
	b1line <- mu2 + (b1-mu1)
}
plot(b1line(b2), type='l')








