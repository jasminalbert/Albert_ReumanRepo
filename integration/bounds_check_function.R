# write integral functions to test bounds
# ten values


# rbarsharp function - can use for both rbar1sharp and rbar2sharp
integrand_rsharp <- function(u, mu1, mu2, sigma, delta){
	mu <- mu1-mu2
	sigma <- sqrt(2)*sigma
	pdf1 <- (1/(sigma*sqrt(2*pi))*exp((-1/2)*((u-mu)/sigma)^2))
	gr <- log(1-delta+delta*exp(u))
	return(pdf1*gr)
}

# rbar1-term1
integrand_r1T1 <- function(u, mu1, mu2, sigma, delta){
	pdf1 <- (1/(sigma*sqrt(2*pi))*exp((-1/2)*(u/sigma)^2))
	gr <- log(1-delta+delta*exp(mu1-mu2))
	return(pdf1*gr)
}

# r1T2 - bivariate
integrand_r1T2 <- function(vars, mu1, mu2, sigma, delta){
	u1 <- vars[1]
	u2 <- vars[2]
	#u1 = b1-mu1; u2 = b2-mu2; so pdf has mean 0
	pdf2 <- exp((-1/2)*(((u1)/sigma)^2+((u2)/sigma)^2))/(2*pi*sigma*sigma)
	grt2 <- log(1-delta+delta*exp(u1-u2+mu1-mu2))
	return(pdf2*grt2)
}

# runs multiple bounds checks for errors and returns value computed with the widest bounds if the value matches the value of the secound widest bounds
bounds_uni <- function(integrand,mu1,mu2,sigma,delta, bounds=seq(350,800,50)){
	
	res <- matrix(c(bounds,rep(NA,10)), ncol=2)
	
	for (b in 1:length(bounds)){
		lower <- -(bounds[b])
		upper <- bounds[b]
		int <- try(integrate(integrand, lower, upper, mu1, mu2, sigma, delta, stop.on.error=FALSE), silent=T)
		
		if (class(int)=="try-error"){
			warning("error at ", bounds[b],"\n")
		} else if (int$abs.error == 0){
			warning("absolute error < 0 at ",bounds[b],"\n")
		} else if (int$message != "OK"){
			warning("message not OK at ",bounds[b],"\n")
		} else {res[b,2] <- try(int$value, silent=T)
			}
		
	}
	
	res1<-res[!is.na(res[,2]),]
	i <- nrow(res1)
	if (round(res1[i,2],4)==round(res1[i-1,2],4)){
		maxB <- res1[i,1]
		val <- res1[i,2]
	}
	
	answer <- c(val, maxB)
	names(answer) <- c("value","bound")
	return(answer)
}

bounds_uni(integrand_r1T1, mu1=0.5,mu2=0.4,sigma=1.6,delta=0.5)


bounds_biv <- function(integrand,mu1,mu2,sigma,delta, bounds=seq(350,800,50)){
	
	res <- matrix(c(bounds,rep(NA,10)), ncol=2)
	
	for (b in 1:length(bounds)){
		#lower <- rep(-(bounds[b]),2)
		lower <- 0
		upper <- bounds[b]
		int <- try(cuhre(integrand, lowerLimit=c(lower, lower), upperLimit=c(upper, upper), mu1=mu1, mu2=mu2, sigma=sigma, delta=delta), silent=T)
	
		if (class(int)=="try-error"){
			warning("error at ", bounds[b],"\n")
		} else if (int$error == 0){
			warning("absolute error < 0 at ",bounds[b],"\n")
		} else if (int$prob != 0){
			warning("prob not 0 at ",bounds[b],"\n")
		} else if (int$returnCode != 0){
			warning("returnCode not 0 at",bounds[b],"\n")
		} else {res[b,2] <- try(int$integral, silent=T)
			}		
	}
	
	res1<-res[!is.na(res[,2]),]
	i <- nrow(res1)
	if (round(res1[i,2],4)==round(res1[i-1,2],4)){
		maxB <- res1[i,1]
		val <- res1[i,2]
	}
	
	answer <- c(val, maxB)
	names(answer) <- c("value","bound")
	return(answer)
}

bounds_biv(integrand_r1T2, mu1, mu2, sigma, delta)





