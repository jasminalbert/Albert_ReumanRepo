source("./measure.co.R")

#function to visualize coexistence measures of multiple generations of the same noise generating processes
#coexistence measure tells us what % of time two species are in coexistence according to a dominance threshold

#ARGS (same as measure.co except for n)
# time = length of time series
# d = death rate
# sigma = standard deviation of birthrate
# mu1 = mean of birthrate for invader
# mu2 = mean of birthrate for resident
# N = total spaces
# N1 = invader initial abundance
# dom = dominance threshold 
# n = number of noise generations

co.hist <- function(time, d, sigma, mu1, mu2, N, N1, dom, n){
	
	#initialize list to store measurements
	co.list <- vector(mode='list', length=n)
	
	for(i in 1:n){
		co.list[[i]] <- measure.co(time=time, d=d, sigma=sigma, mu1=mu1, mu2=mu2, N=N, N1=N1, dom=dom)
	}
	
	co_ELT <- unlist(lapply(co.list, function(X)			   {return(X$B_ELT)}))
	co_sym <- unlist(lapply(co.list, function(X){return(X$B_sym)}))
	co_ERT <- unlist(lapply(co.list, function(X)			     {return(X$B_ERT)}))

	maxx <- max(c(co_ELT, co_sym, co_ERT))

	hist(co_ELT, xlim = c(0, maxx), breaks = 10, sub = paste("sigma=", sigma, " mu1=", mu1, " mu2=",mu2))
	hist(co_sym, xlim = c(0, maxx), breaks = 10, sub = paste("sigma=", sigma, " mu1=", mu1, " mu2=",mu2))
	hist(co_ERT, xlim = c(0, maxx), breaks = 10, sub = paste("sigma=", sigma, " mu1=", mu1, " mu2=",mu2))
	
	return(list("ELT"=mean(co_ELT), "sym"=mean(co_sym), "ERT"=mean(co_ERT)))
}


#test
#for where SE is postive in sym and negative in asym
pdf("co.hist_sympos.95.pdf")

sp1 <- co.hist(time=10000, d=1, sigma=1.6, mu1=0.1, mu2=0.1, N=50, N1=25, dom=0.95, n=1000)
sp2 <- co.hist(10000, 1, 0.8, 0.2, 0.2, 50, 25, 0.95, 1000)
sp3 <- co.hist(10000, 1, 0.8, 0.9, 0.9, 50, 25, 0.95, 1000)
sp4 <- co.hist(10000, 1, 1.6, 0.5, 0.5, 50, 25, 0.95, 1000)
sp5 <- co.hist(10000, 1, 6.4, 0.6, 0.6, 50, 25, 0.95, 1000)

dev.off()

#DATAFRAME of coexistence fractions when SEsym>0 and SEasym<0
spMat <- matrix(c(unlist(sp1), unlist(sp2), unlist(sp3), unlist(sp4), unlist(sp5)), nrow = 5, byrow=T)

colnames(spMat) <- c("ELT", "sym", "ERT")

spDF <- data.frame(spMat)
spDF[,4] <- c(1.6,0.8,0.8,1.6,6.4)
spDF[,5] <- c(0.1,0.2,0.9,0.5,0.6)
names(spDF)[4] <- paste("sdev")
names(spDF)[5] <- paste("mn")

#for where SE is negative in sym and positive in asym
pdf("co.hist_symneg.95.pdf")

sn1 <- co.hist(10000, 1, 0.8, 0.7, 0.7, 50, 25, 0.95, 1000)
sn2 <- co.hist(10000, 1, 6.4, 0.8, 0.8, 50, 25, 0.95, 1000)
dev.off()

snMat <- matrix(c(unlist(sn1), unlist(sn2)), nrow = 2, byrow=T)

colnames(snMat) <- c("ELT", "sym", "ERT")

snDF <- data.frame(snMat)
snDF[,4] <- c(0.8,6.4)
snDF[,5] <- c(0.7,0.8)
names(spDF)[4] <- paste("sdev")
names(spDF)[5] <- paste("mn")




