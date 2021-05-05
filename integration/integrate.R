? integrate

install.packages("cubature")
library(cubature)

pdf1 <- function(x, mu=0, sigma=1){
	pdf1 <- 1/(sigma*sqrt(2*pi))*exp((-1/2)*((x-mu)/sigma)^2)
	return(pdf1)
}

gr <- function(u, delta=0.5){
	gr <- log(1-delta+delta*exp(u))
	return(gr)
}

integrand_rsharp1 <- function(u, mu=0, sigma=1, delta=0.5){
	pdf1 <- (1/(sigma*sqrt(2*pi))*exp((-1/2)*((u-mu)/sigma)^2))
	gr <- log(1-delta+delta*exp(u))
	return(pdf1*gr)
}

curve(integrand_rsharp, from=0, to=250)

x <- seq(-250,250,1)
y <- integrand_rsharp(x, sigma = 1)
plot(x,y, type="l")

integrate(integrand_rsharp1, -500,500, mu=0, sigma=1.9)
integrate(pdf1, -Inf, Inf)
integrate(gr,-500,500)
gr(1)

mu1 <- 0.5; mu2 <- 0.4
b1 <- #normal noise 1 #mean mu1 var sigma^2
b2 <- #normal noise 2 #mean mu2 var sigma^2
b1sharp <- #normal noise 1 sharp #mean mu1 var sigma^2
b2sharp <- #normal noise 2 sharp #mean mu2 var sigma^2

u_r1sharp <- b1sharp - b2 #normal with mean mu1-mu2 and var 2sigma^2
u_r2sharp <- b2sharp - b2 #normal with mean 0 and var 2sigma^2 
# use to plug into rsharp

#term1 of rbar1
integrand_r1t1 <- function(u, mu=0, sigma=1, delta=0.5, mu1=0.5, mu2=0.4){
	pdf1 <- (1/(sigma*sqrt(2*pi))*exp((-1/2)*((u-mu)/sigma)^2))
	grt1 <- log(1-delta + delta*exp(mu1-mu2))
	return(pdf1*grt1)
}

integrate(integrand_r1t1, -Inf, 0)


pdf2 <- function(vars, mu1, mu2, sigma=1){
	u1 <- vars[1]
	u2 <- vars[2]
	pdf2 <- (1/(2*pi*(sigma)^2))*exp((-1/2)*(((u1-mu1)/sigma)^2+((u2-mu2)/sigma)^2))
	return(pdf2)
}



cuhre(pdf2, lowerLimit=c(-Inf, -Inf), upperLimit=c(Inf, Inf), mu1=0.5, mu2=0.5)

integrand_r1t2 <- function(vars, mu1, mu2, sigma, delta){
	u1 <- vars[1]
	u2 <- vars[2]
	pdf2 <- (1/(2*pi*(sigma)^2))*exp((-1/2)*(((u1)/sigma)^2+((u2)/sigma)^2))
	#pdf with mean 0
	grt2 <- log(1-delta+delta*exp(u1-u2+mu1-mu2))
	return(pdf2*grt2)
}

cuhre(integrand_r1t2, lowerLimit=c(0, 0), upperLimit=c(700, 700), mu1=0.9, mu2=0.9, sigma=2, delta=0.5) #wtf
int$integral














