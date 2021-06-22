source("./measure.co.R")
res1<-read.csv("generated_data/run3/SEandCoMresults202101.csv")
cores1 <- res1[,c(2:5, 18:26)]
cores1all <- read.csv("generated_data/run3/CoMetrics_results.csv")[,-1]
params <- c(6.4,0.9,0.9)
sigmares<-cores1[cores1$sigma==params[1],]

cores1$mudif <- abs(cores1$mu1-cores1$mu2)

plot(sigmares$co.meanELT[1:50], type='l', col='red')
lines(sigmares$co.meanERT[1:50], col='blue')
lines(sigmares$co.meansym[1:50], col='green')
lines((sigmares$delta*1000)[1:50])

pdf("co_mean_noise_diff.pdf")

sigmares<-cores1[cores1$sigma==6.4,]

plot(sigmares$co.meansym-sigmares$co.meanELT, type='l', col='red',  main="Symmetric coexistence length mean - left-tailed coexistence length mean",sub="sigma=6.4",ylim=c(-400,700))
lines((sigmares$delta*100)-400, col='grey')
lines((sigmares$mu1*100)+500)
lines((sigmares$mu2*100)+500, col='grey')
plot(sigmares$co.meansym-sigmares$co.meanERT, type='l', col='blue',  main="Symmetric coexistence length mean - right-tailed coexistence length mean",sub="sigma=6.4",ylim=c(-400,700))
lines((sigmares$delta*100)-400, col='grey')
lines((sigmares$mu1*100)+500)
lines((sigmares$mu2*100)+500, col='grey')


sigmares<-cores1[cores1$sigma==3.2,]

plot(sigmares$co.meansym-sigmares$co.meanELT, type='l', col='red',  main="Symmetric coexistence length mean - left-tailed coexistence length mean",sub="sigma=3.2",ylim=c(-400,700))
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+600)
lines((sigmares$mu2*100)+600, col='grey')
plot(sigmares$co.meansym-sigmares$co.meanERT, type='l', col='blue',  main="Symmetric coexistence length mean - right-tailed coexistence length mean; sigma=3.2",ylim=c(-400,700))
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+600)
lines((sigmares$mu2*100)+600, col='grey')


sigmares<-cores1[cores1$sigma==1.6,]

plot(sigmares$co.meansym-sigmares$co.meanELT, type='l', col='red',  main="Symmetric coexistence length mean - left-tailed coexistence length mean",sub="sigma=1.6", ylim=c(-350,550))
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+450)
lines((sigmares$mu2*100)+450, col='grey')
plot(sigmares$co.meansym-sigmares$co.meanERT, type='l', col='blue',  main="Symmetric coexistence length mean - right-tailed coexistence length mean",sub="sigma=1.6", ylim=c(-350,550))
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+450)
lines((sigmares$mu2*100)+450, col='grey')


sigmares<-cores1[cores1$sigma==0.8,]

plot(sigmares$co.meansym-sigmares$co.meanELT, type='l', col='red',  main="Symmetric coexistence length mean - left-tailed coexistence length mean",sub="sigma=0.8", ylim=c(-350,300))
lines((sigmares$delta*100)-250, col='grey')
lines((sigmares$mu1*100)+100)
lines((sigmares$mu2*100)+100, col='grey')
plot(sigmares$co.meansym-sigmares$co.meanERT, type='l', col='blue',  main="Symmetric coexistence length mean - right-tailed coexistence length mean",sub="sigma=0.8", ylim=c(-350,300))
lines((sigmares$delta*100)-250, col='grey')
lines((sigmares$mu1*100)+100)
lines((sigmares$mu2*100)+100, col='grey')


sigmares<-cores1[cores1$sigma==0.4,]

plot(sigmares$co.meansym-sigmares$co.meanELT, type='l', col='red',  main="Symmetric coexistence length mean - left-tailed coexistence length mean",sub="sigma=0.4", ylim=c(-400,350))
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+200)
lines((sigmares$mu2*100)+200, col='grey')
plot(sigmares$co.meansym-sigmares$co.meanERT, type='l', col='blue',  main="Symmetric coexistence length mean - right-tailed coexistence length mean",sub="sigma=0.4", ylim=c(-400,350))
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+200)
lines((sigmares$mu2*100)+200, col='grey')

dev.off()


pdf("co_mean_noise_mean_diff.pdf")

sigmares<-cores1[cores1$sigma==6.4,]
plot(abs(sigmares$mu1-sigmares$mu2), sigmares$co.meansym-sigmares$co.meanELT, col='red',main="sym - left-tailed mean co length vs abs mean difference", sub="sigma=6.4")
plot(abs(sigmares$mu1-sigmares$mu2), sigmares$co.meansym-sigmares$co.meanERT, col='blue',main="sym - right-tailed mean co length vs abs mean difference", sub="sigma=6.4")
sigmares<-cores1[cores1$sigma==3.2,]
plot(abs(sigmares$mu1-sigmares$mu2), sigmares$co.meansym-sigmares$co.meanELT, col='red',main="sym - left-tailed mean co length vs abs mean difference", sub="sigma=3.2")
plot(abs(sigmares$mu1-sigmares$mu2), sigmares$co.meansym-sigmares$co.meanERT, col='blue',main="sym - right-tailed mean co length vs abs mean difference", sub="sigma=3.2")
sigmares<-cores1[cores1$sigma==1.6,]
plot(abs(sigmares$mu1-sigmares$mu2), sigmares$co.meansym-sigmares$co.meanELT, col='red',main="sym - left-tailed mean co length vs abs mean difference", sub="sigma=1.6")
plot(abs(sigmares$mu1-sigmares$mu2), sigmares$co.meansym-sigmares$co.meanERT, col='blue',main="sym - right-tailed mean co length vs abs mean difference", sub="sigma=1.6")
sigmares<-cores1[cores1$sigma==0.8,]
plot(abs(sigmares$mu1-sigmares$mu2), sigmares$co.meansym-sigmares$co.meanELT, col='red',main="sym - left-tailed mean co length vs abs mean difference", sub="sigma=0.8")
plot(abs(sigmares$mu1-sigmares$mu2), sigmares$co.meansym-sigmares$co.meanERT, col='blue',main="sym - right-tailed mean co length vs abs mean difference", sub="sigma=0.8")
sigmares<-cores1[cores1$sigma==0.4,]
plot(abs(sigmares$mu1-sigmares$mu2), sigmares$co.meansym-sigmares$co.meanELT, col='red',main="sym - left-tailed mean co length vs abs mean difference", sub="sigma=0.4")
plot(abs(sigmares$mu1-sigmares$mu2), sigmares$co.meansym-sigmares$co.meanERT, col='blue',main="sym - right-tailed mean co length vs abs mean difference", sub="sigma=0.4")

dev.off()

boxplot(sigmares$co.meansym-sigmares$co.meanELT~abs(sigmares$mu1-sigmares$mu2))




sigmares<-cores1[cores1$sigma==0.4 & cores1$mudif==0,]
plot(sigmares$co.meansym-sigmares$co.meanELT, type='l')
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+200)

plot(sigmares$delta,sigmares$co.meansym-sigmares$co.meanELT)


sigmares<-cores1[cores1$sigma==6.4,]
plot(sigmares$co.numsym, type='l', col='forestgreen')
lines(sigmares$co.numELT, col='red')
lines(sigmares$co.numERT, col='blue')


rp <- sample(1:4455,1);rp #random parameters
cores1[rp,]
ck<-as.numeric(cores1[rp,])
measure.co(time=100000, d=ck[4], sigma=ck[1], mu1=ck[2], mu2=ck[3], N=50, N1=25, dom=.9)






#co.num


pdf("co_mean_noise_diff.pdf")

sigmares<-cores1[cores1$sigma==6.4,]

plot(sigmares$co.numsym-sigmares$co.numELT, type='l', col='red',  main="# Symmetric coexistence periods - # left-tailed coexistence periods",sub="sigma=6.4")
lines((sigmares$delta*10)-10, col='grey')
lines((sigmares$mu1*10)+50)
lines((sigmares$mu2*10)+50, col='grey')
plot(sigmares$co.meansym-sigmares$co.meanERT, type='l', col='blue',  main="Symmetric coexistence length mean - right-tailed coexistence length mean",sub="sigma=6.4",ylim=c(-400,700))
lines((sigmares$delta*100)-400, col='grey')
lines((sigmares$mu1*100)+500)
lines((sigmares$mu2*100)+500, col='grey')


sigmares<-cores1[cores1$sigma==3.2,]

plot(sigmares$co.meansym-sigmares$co.meanELT, type='l', col='red',  main="Symmetric coexistence length mean - left-tailed coexistence length mean",sub="sigma=3.2",ylim=c(-400,700))
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+600)
lines((sigmares$mu2*100)+600, col='grey')
plot(sigmares$co.meansym-sigmares$co.meanERT, type='l', col='blue',  main="Symmetric coexistence length mean - right-tailed coexistence length mean; sigma=3.2",ylim=c(-400,700))
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+600)
lines((sigmares$mu2*100)+600, col='grey')


sigmares<-cores1[cores1$sigma==1.6,]

plot(sigmares$co.meansym-sigmares$co.meanELT, type='l', col='red',  main="Symmetric coexistence length mean - left-tailed coexistence length mean",sub="sigma=1.6", ylim=c(-350,550))
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+450)
lines((sigmares$mu2*100)+450, col='grey')
plot(sigmares$co.meansym-sigmares$co.meanERT, type='l', col='blue',  main="Symmetric coexistence length mean - right-tailed coexistence length mean",sub="sigma=1.6", ylim=c(-350,550))
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+450)
lines((sigmares$mu2*100)+450, col='grey')


sigmares<-cores1[cores1$sigma==0.8,]

plot(sigmares$co.meansym-sigmares$co.meanELT, type='l', col='red',  main="Symmetric coexistence length mean - left-tailed coexistence length mean",sub="sigma=0.8", ylim=c(-350,300))
lines((sigmares$delta*100)-250, col='grey')
lines((sigmares$mu1*100)+100)
lines((sigmares$mu2*100)+100, col='grey')
plot(sigmares$co.meansym-sigmares$co.meanERT, type='l', col='blue',  main="Symmetric coexistence length mean - right-tailed coexistence length mean",sub="sigma=0.8", ylim=c(-350,300))
lines((sigmares$delta*100)-250, col='grey')
lines((sigmares$mu1*100)+100)
lines((sigmares$mu2*100)+100, col='grey')


sigmares<-cores1[cores1$sigma==0.4,]

plot(sigmares$co.meansym-sigmares$co.meanELT, type='l', col='red',  main="Symmetric coexistence length mean - left-tailed coexistence length mean",sub="sigma=0.4", ylim=c(-400,350))
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+200)
lines((sigmares$mu2*100)+200, col='grey')
plot(sigmares$co.meansym-sigmares$co.meanERT, type='l', col='blue',  main="Symmetric coexistence length mean - right-tailed coexistence length mean",sub="sigma=0.4", ylim=c(-400,350))
lines((sigmares$delta*100)-300, col='grey')
lines((sigmares$mu1*100)+200)
lines((sigmares$mu2*100)+200, col='grey')

dev.off()




# new sim with 10^6 timesteps
# load data
# df with all 10 reps
res2<-read.csv("generated_data/run3/CoMetrics_results2.csv")[,-1]
res2m<-read.csv("generated_data/run3/CoMetrics_mean2.csv")[,-1]

# random value check 
rp <- sample(1:495,1);rp #random parameters
res2m[rp,]
res2m[res2m$sigma==6.4,]

plotCoM <- function(df, params, d = seq(0,1,0.1),...){
	
	#x <- df[1:11,4]
	yLT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],8]
	yRT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],9]
	ysm <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],10]
	
	y <- c(yLT, yRT, ysm)
	miny <- min(y)-abs(0.1*min(y))
	maxy <- max(y)+0.1*max(y)
	range <- c(miny,maxy)
	
	plot(0, ylim=range, xlim=range(d), type='n', xlab=expression(delta), ylab="Co period length (mean)",...)
	lines(d, yLT, col='red', lty = 2, lwd=1.5)
	lines(d, yRT, col='blue', lty = 2, lwd=1.5)
	lines(d, ysm, col='green', lty = 2, lwd=1.5)
	mtext(paste("sigma=", params[1]," mu1=",params[2]," mu2=",params[3]), side=1, line=-1)
	legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
}

linesCoM <- function(df, params, pdf){
	x <- pdf[1:11,4]
	df <- df[pdf[,1]==params[1] & pdf[,2]==params[2] & pdf[,3]==params[3],]

	for (i in 1:10){
		lines(x, df[,30+i], col=rgb(1,0,0, 0.2))
		lines(x, df[,40+i], col=rgb(0,0,1, 0.2))
		lines(x, df[,50+i], col=rgb(0,1,0, 0.2))
	}
}


params <- c(6.4,0.9,0.9)
#parameters for noise
delta <- seq(0,1,0.1)
mu1 <- c(0.1,0.5,0.9)
mu2 <- mu1
sigma <- c(0.4, 0.8, 1.6, 3.2, 6.4)
tot_sims <- length(sigma)*length(mu1)*length(mu2)*length(delta)

delta_sims <- rep(delta, tot_sims/length(delta))
mu2_sims <- sort(rep(mu2, length(delta)))
mu1_sims <- sort(rep(mu1, length(mu2_sims)))
sigma_sims <- sort(rep(sigma, length(mu1_sims)))

paramsdf2 <- data.frame(sigma=sigma_sims, mu1=mu1_sims, mu2=mu2_sims, delta=delta_sims)

plotsims <- length(sigma)*length(mu1)*length(mu1)
mu1.psims <- rep(mu1, times=plotsims/length(mu1))
mu2.psims <- sort(rep(mu1, times=length(mu1)))
sigma.psims <- sort(rep(sigma, times=plotsims/length(sigma)))
plotparammat<- cbind(sigma.psims, mu1.psims,mu2.psims)

pdf("run3_comean_plotcheck.pdf")
for (i in 1:plotsims){
	params <- as.numeric(plotparammat[i,])
	plotCoM(df=res2m, params=params)
	linesCoM(df=res2, params=params, pdf=paramsdf2)
}
dev.off()

pdf("run3_comean_plotcheck2.pdf")
for (i in 1:plotsims){
	params <- as.numeric(plotparammat[i,])
	plotCoM(df=res2m, params=params, d=seq(0.3,1,0.1))
	linesCoM(df=res2, params=params, pdf=paramsdf2)
}
dev.off()


# make df for equal means only (EM)
res2mEM <- res2m[res2m$mu1 == res2m$mu2,]
res2EM <- res2[res2m$mu1 == res2m$mu2,]

plotparam2 <- unique(plotparammat[,1:2])
plotparam2<- cbind(plotparam2,sort(rep(1:5,3)))


pdf("comean_eqmean.pdf")
plot(0, ylim = c(0,10^6), xlim = range(mu1),type = 'n', ylab = "Co period length (mean)", xlab = expression(delta), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparam2)){
	params <- as.numeric(plotparam2[i,])
	lines(delta,res2mEM[res2mEM$sigma == params[1] & res2mEM$mu1 == params[2], 10], col = "green", lty=params[3])
	lines(delta,res2mEM[res2mEM$sigma == params[1] & res2mEM$mu1 == params[2], 8], col = "blue", lty=params[3])
	lines(delta,res2mEM[res2mEM$sigma == params[1] & res2mEM$mu1 == params[2], 9], col = "red", lty=params[3])
}
legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), bty ='n', border='white')
legend("right", title = expression(sigma), legend = c(0.4,0.8,1.6,3.2,6.4), lty = 1:5, bty='n')

plot(0, ylim = c(0,5000), xlim = c(0.2,1),type = 'n', ylab = "Co period length (mean)", xlab = expression(delta), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparam2)){
	params <- as.numeric(plotparam2[i,])
	lines(delta,res2mEM[res2mEM$sigma == params[1] & res2mEM$mu1 == params[2], 10], col = "green", lty=params[3])
	lines(delta,res2mEM[res2mEM$sigma == params[1] & res2mEM$mu1 == params[2], 8], col = "blue", lty=params[3])
	lines(delta,res2mEM[res2mEM$sigma == params[1] & res2mEM$mu1 == params[2], 9], col = "red", lty=params[3])
}
legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), bty ='n', border='white')
legend("right", title = expression(sigma), legend = c(0.4,0.8,1.6,3.2,6.4), lty = 1:5, bty='n')


plot(0, ylim = c(0,1000), xlim = c(0.4,1),type = 'n', ylab = "Co period length (mean)", xlab = expression(delta), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparam2)){
	params <- as.numeric(plotparam2[i,])
	lines(delta,res2mEM[res2mEM$sigma == params[1] & res2mEM$mu1 == params[2], 10], col = "green", lty=params[3])
	lines(delta,res2mEM[res2mEM$sigma == params[1] & res2mEM$mu1 == params[2], 8], col = "blue", lty=params[3])
	lines(delta,res2mEM[res2mEM$sigma == params[1] & res2mEM$mu1 == params[2], 9], col = "red", lty=params[3])
}
legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), bty ='n', border='white')
legend("right", title = expression(sigma), legend = c(0.4,0.8,1.6,3.2,6.4), lty = 1:5, bty='n')

plot(0, ylim = c(0,230), xlim = c(0.6,1),type = 'n', ylab = "Co period length (mean)", xlab = expression(delta), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparam2)){
	params <- as.numeric(plotparam2[i,])
	lines(delta,res2mEM[res2mEM$sigma == params[1] & res2mEM$mu1 == params[2], 10], col = "green", lty=params[3])
	lines(delta,res2mEM[res2mEM$sigma == params[1] & res2mEM$mu1 == params[2], 8], col = "blue", lty=params[3])
	lines(delta,res2mEM[res2mEM$sigma == params[1] & res2mEM$mu1 == params[2], 9], col = "red", lty=params[3])
}
legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), bty ='n', border='white')
legend("right", title = expression(sigma), legend = c(0.4,0.8,1.6,3.2,6.4), lty = 1:5, bty='n')
dev.off()


rep(delta,5)
sort(rep(sigma,11))
plotparams3 <- cbind(sort(rep(sigma,11)),rep(delta,5),sort(rep(1:5,11)))
mu <- seq(0.1,0.9,0.1)


pdf("comean_eqmean2.pdf")

plot(0, ylim = c(0,10^6), xlim = range(mu1),type = 'n', ylab = "Co period length (mean)", xlab = expression(paste(mu,"1 = ",mu,"2")), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparams3)){
	params <- signif(plotparams3[i,],2)
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 10], col = "green", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 8], col = "blue", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 9], col = "red", lty=params[3])
}
legend("left", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), bty ='n', border='white')
legend("right", title = expression(sigma), legend = c(0.4,0.8,1.6,3.2,6.4), lty = 1:5, bty='n')

plot(0, ylim = c(8*10^5,10^6), xlim = range(mu1),type = 'n', ylab = "Co period length (mean)", xlab = expression(paste(mu,"1 = ",mu,"2")), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparams3)){
	params <- signif(plotparams3[i,],2)
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 10], col = "green", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 8], col = "blue", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 9], col = "red", lty=params[3])
}

plot(0, ylim = c(0,2.25*10^5), xlim = range(mu1),type = 'n', ylab = "Co period length (mean)", xlab = expression(paste(mu,"1 = ",mu,"2")), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparams3)){
	params <- signif(plotparams3[i,],2)
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 10], col = "green", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 8], col = "blue", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 9], col = "red", lty=params[3])
}

legend("top", title = expression(sigma), legend = c(0.4,0.8,1.6,3.2,6.4), lty = 1:5, bty='n')

plot(0, ylim = c(0,50000), xlim = range(mu1),type = 'n', ylab = "Co period length (mean)", xlab = expression(paste(mu,"1 = ",mu,"2")), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparams3)){
	params <- signif(plotparams3[i,],2)
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 10], col = "green", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 8], col = "blue", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 9], col = "red", lty=params[3])
}


plot(0, ylim = c(0,10000), xlim = range(mu1),type = 'n', ylab = "Co period length (mean)", xlab = expression(paste(mu,"1 = ",mu,"2")), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparams3)){
	params <- signif(plotparams3[i,],2)
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 10], col = "green", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 8], col = "blue", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 9], col = "red", lty=params[3])
}

legend("top", title = expression(sigma), legend = c(0.4,0.8,1.6,3.2,6.4), lty = 1:5, bty='n', horiz=TRUE)

plot(0, ylim = c(0,2000), xlim = range(mu1),type = 'n', ylab = "Co period length (mean)", xlab = expression(paste(mu,"1 = ",mu,"2")), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparams3)){
	params <- signif(plotparams3[i,],2)
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 10], col = "green", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 8], col = "blue", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 9], col = "red", lty=params[3])
}

legend("top", title = expression(sigma), legend = c(0.4,0.8,1.6,3.2,6.4), lty = 1:5, bty='n', horiz=TRUE)


plot(0, ylim = c(0,700), xlim = range(mu1),type = 'n', ylab = "Co period length (mean)", xlab = expression(paste(mu,"1 = ",mu,"2")), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparams3)){
	params <- signif(plotparams3[i,],2)
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 10], col = "green", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 8], col = "blue", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 9], col = "red", lty=params[3])
}

legend("top", title = expression(sigma), legend = c(0.4,0.8,1.6,3.2,6.4), lty = 1:5, bty='n', horiz=TRUE)

plot(0, ylim = c(0,150), xlim = range(mu1),type = 'n', ylab = "Co period length (mean)", xlab = expression(paste(mu,"1 = ",mu,"2")), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparams3)){
	params <- signif(plotparams3[i,],2)
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 10], col = "green", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 8], col = "blue", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 9], col = "red", lty=params[3])
}

legend("top", title = expression(sigma), legend = c(0.4,0.8,1.6,3.2,6.4), lty = 1:5, bty='n', horiz=TRUE)

plot(0, ylim = c(0,75), xlim = range(mu1),type = 'n', ylab = "Co period length (mean)", xlab = expression(paste(mu,"1 = ",mu,"2")), main = expression(paste("Co.mean according to changes in ",mu,"1 = ",mu,"2 and ", delta)))

for (i in 1:nrow(plotparams3)){
	params <- signif(plotparams3[i,],2)
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 10], col = "green", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 8], col = "blue", lty=params[3])
	lines(mu1,res2mEM[res2mEM$sigma == params[1] & res2mEM$delta == params[2], 9], col = "red", lty=params[3])
}

legend("top", title = expression(sigma), legend = c(0.4,0.8,1.6,3.2,6.4), lty = 1:5, bty='n', horiz=TRUE)

dev.off()




