#trying to measure dominance periods lengths 
# 1. generate noise
# 2. popsim
# 3. measure lengths
# 4. other metrics
source("./new_functions/getnoise.R")
source("./new_functions/popsim2.R")
source("./new_functions/co.periods.R")
source("./new_functions/measure.co3.R")

#set correlation
corval <- 0.88
corRT <- getcorpar(0,.5,corval,numpts=100000,allowneg=FALSE,ploton=FALSE)

#set params
n <- 10^6
N <- 50
N1 <- 25
delta <- seq(0,1,0.1)
mu1 <- c(0.1,0.5,0.9); mu2 <- mu1
sigma <- c(0.4, 0.8, 1.6, 3.2, 6.4)
tot_sims <- length(sigma)*length(mu1)*length(mu2)*length(delta)
delta_sims <- rep(delta, tot_sims/length(delta))
mu2_sims <- sort(rep(mu2, length(delta)))
mu1_sims <- sort(rep(mu1, length(mu2_sims)))
sigma_sims <- sort(rep(sigma, length(mu1_sims)))
paramdf <- data.frame(sigma=sigma_sims, mu1=mu1_sims, mu2=mu2_sims, delta=delta_sims)

index1 <- c(1:11, 12, 14, 17, 20, 22, 23 ,25 ,28 ,31 ,33 ,45 ,47 ,50 ,53 ,55 ,56 ,58 ,61 ,64 ,66 ,89 ,91 ,94 ,97 ,99)
indexpsued <- c(1, 3, 6, 9, 11, 12, 14, 17, 20, 22, 23 ,25 ,28 ,31 ,33 ,45 ,47 ,50 ,53 ,55 ,56 ,58 ,61 ,64 ,66 ,89 ,91 ,94 ,97 ,99)
index2 <- c(index1, indexpsued+99)
index3 <- c(index2, indexpsued+(99*2))
index4 <- c(index3, indexpsued+(99*3))
index5 <- c(index4, indexpsued+(99*4))

for (i in 2:156){
	params <- paramdf[index5[i],]; params

	#generate noise
	noise_b <- getnoise2(mu = c(params$mu1, params$mu2), sigma = c(params$sigma,params$sigma), n=n, corval=corval, corRT=corRT)
		
	#exponentiate noise
	noise_B <- exp(noise_b)
			
	#population simulation
	popLT <- popsim2(B=cbind(noise_B$LT1, noise_B$LT2), N=N, N1=N1, delta = params$delta, n=n)
	popsym <- popsim2(B=cbind(noise_B$sym1, noise_B$sym2), N=N, N1=N1, delta = params$delta, n=n)
	popRT <-popsim2(B=cbind(noise_B$RT1, noise_B$RT2), N=N, N1=N1, delta = params$delta, n=n)
		
	pop <- cbind(popLT, popsym, popRT)
	
	#coexistence-dominance periods
	co.p <- co.periods(pop, dom=0.95, N=50); co.p
	dom.p <- dom.periods(pop, dom=0.95, N=50); dom.p

	#coexistence metrics
	metrics <- measure.co2(co.p, dom.p, n); metrics

	#res <- metrics
	res <- rbind(res, metrics); res
	print(i)
	print(res)
}

params <- paramdf[index5[156],]; params

#generate noise
noise_b <- getnoise2(mu = c(params$mu1, params$mu2), sigma = c(params$sigma,params$sigma), n=n, corval=corval, corRT=corRT)
		
#exponentiate noise
noise_B <- exp(noise_b)
			
#population simulation
popLT <- popsim2(B=cbind(noise_B$LT1, noise_B$LT2), N=N, N1=N1, delta = params$delta, n=n)
popsym <- popsim2(B=cbind(noise_B$sym1, noise_B$sym2), N=N, N1=N1, delta = params$delta, n=n)
popRT <-popsim2(B=cbind(noise_B$RT1, noise_B$RT2), N=N, N1=N1, delta = params$delta, n=n)
		
pop <- cbind(popLT, popsym, popRT)
	
#coexistence-dominance periods
co.p <- co.periods(pop, dom=0.95, N=50); co.p
dom.p <- dom.periods(pop, dom=0.95, N=50); dom.p

#coexistence metrics
metrics <- measure.co2(co.p, dom.p, n); metrics

#res <- metrics
res <- rbind(res, metrics); res
pindex <- sort(rep(index5,3))
ress <- cbind(pindex, res)

head(ress)

res.mean <- ress[, c(1, 4, 7)]
head(res.mean, 50)
noisename <- rep(c("ELT","sym","ERT"),156)
res.mean <- cbind(res.mean, noisename)

pdf("dom_co_delta.pdf")

# 0.4 0.1 0.1
d3 <- sort(rep(delta,3))
plot(d3,res.mean[1:33,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[index5[1],1], " mu1=",paramdf[index5[1],2], " mu2=",paramdf[index5[1],3]))
points(d3, res.mean[1:33, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 0.4 0.1 0.5
d5 <- c(0, 0.2, 0.5, 0.8, 1.0)
d5 <- sort(rep(d5,3))
plot(d5,res.mean[34:48,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[index5[12],1], " mu1=",paramdf[index5[12],2], " mu2=",paramdf[index5[12],3]))
points(d5, res.mean[34:48, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 0.4 0.1 0.9
plot(d5,res.mean[49:63,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[23,1], " mu1=",paramdf[23,2], " mu2=",paramdf[23,3]))
points(d5, res.mean[49:63, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 0.4 0.5 0.5
plot(d5,res.mean[64:78,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[45,1], " mu1=",paramdf[45,2], " mu2=",paramdf[45,3]))
points(d5, res.mean[64:78, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 0.4 0.5 0.9
plot(d5,res.mean[79:93,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[56,1], " mu1=",paramdf[56,2], " mu2=",paramdf[56,3]))
points(d5, res.mean[79:93, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 0.4 0.9 0.9
res.mean[94:108,]
plot(d5,res.mean[94:108,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[89,1], " mu1=",paramdf[89,2], " mu2=",paramdf[56,3]))
points(d5, res.mean[94:108, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 0.8 0.1 0.1
rm <- res.mean[109:123,]
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 0.8 0.1 0.5
rm <- res.mean[124:138,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 0.8 0.1 0.9
rm <- res.mean[139:153,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 0.8 0.5 0.5
rm <- res.mean[154:168,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 0.8 0.5 0.9
rm <- res.mean[169:183,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 0.8 0.9 0.9
rm <- res.mean[184:198,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 1.6 0.1 0.1
rm <- res.mean[199:213,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 1.6 0.1 0.5
rm <- res.mean[214:228,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 1.6 0.1 0.9
rm <- res.mean[229:243,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 1.6 0.5 0.5
rm <- res.mean[244:258,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 1.6 0.5 0.9
rm <- res.mean[259:273,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 1.6 0.9 0.9
rm <- res.mean[274:288,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 3.2 0.1 0.1
rm <- res.mean[289:303,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 3.2 0.1 0.5
rm <- res.mean[304:318,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 3.2 0.1 0.9
rm <- res.mean[319:333,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 3.2 0.5 0.5
rm <- res.mean[334:348,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 3.2 0.5 0.9
rm <- res.mean[349:363,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 3.2 0.9 0.9
rm <- res.mean[364:378,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 6.4 0.1 0.1
rm <- res.mean[379:393,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 6.4 0.1 0.5
rm <- res.mean[394:408,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 6.4 0.1 0.9
rm <- res.mean[409:423,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 6.4 0.5 0.5
rm <- res.mean[424:438,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 6.4 0.5 0.9
rm <- res.mean[439:453,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

# 6.4 0.9 0.9
rm <- res.mean[454:468,]; rm
plot(d5,rm[,"co.mean"], col=c("red","green","blue"), log='y', cex=1.5, ylab="period length (mean), logscale", xlab="delta", main="Coexistence and Dominance Period Length", sub=paste("sigma=",paramdf[rm[1,1],1], " mu1=",paramdf[rm[1,1],2], " mu2=",paramdf[rm[1,1],3]))
points(d5, rm[, "dom.mean"], col=c("red","green","blue"), pch=2, cex=1.5)

dev.off()


res3 <- ress[,order(colnames(ress))]
res3 <- res3[,-7]


mets <- sort(colnames(res3))[-7];mets
metst3 <- sort(rep(mets, 3)); metst3
noiset6 <- rep(c("ELT","sym","ERT"),6); noiset6

cnames <- paste(metst3, noiset6, sep=""); cnames


cofrac <- matrix(res3$co.frac, ncol = 3, byrow=TRUE)
comean <- matrix(res3$co.mean, ncol = 3, byrow=TRUE)
conum <- matrix(res3$co.num, ncol = 3, byrow=TRUE)
domfrac <- matrix(res3$dom.frac, ncol = 3, byrow=TRUE)
dommean <- matrix(res3$dom.mean, ncol = 3, byrow=TRUE)
domnum <- matrix(res3$dom.num, ncol = 3, byrow=TRUE)

mat <- cbind(cofrac, comean, conum, domfrac, dommean, domnum)
colnames(mat) <- cnames




res4 <- cbind(paramdf[index5,], mat)
head(res4)






















