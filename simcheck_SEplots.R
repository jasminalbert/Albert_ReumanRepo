#checking simulation

res1<-read.csv("generated_data/run3/SEandCoMresults202101.csv")
res1 <- res1[,-1]
res1all <- read.csv("generated_data/run3/SE_results.csv")
res1all <- data.frame(cbind(res1[,1:4],res1all[,2:31]))
res0 <- read.csv("generated_data/run1/sim_mean")
res0 <- res0[,-1]
res0_1 <- read.csv("generated_data/run1/sim_df1")[,6:8]
res0_2 <- read.csv("generated_data/run1/sim_df2")[,6:8]
res0_3 <- read.csv("generated_data/run1/sim_df3")[,6:8]
res0_4 <- read.csv("generated_data/run1/sim_df4")[,6:8]
res0_5 <- read.csv("generated_data/run1/sim_df5")[,6:8]
res0_6 <- read.csv("generated_data/run1/sim_df6")[,6:8]
res0_7 <- read.csv("generated_data/run1/sim_df7")[,6:8]
res0_8 <- read.csv("generated_data/run1/sim_df8")[,6:8]
res0_9 <- read.csv("generated_data/run1/sim_df9")[,6:8]
res0_10 <- read.csv("generated_data/run1/sim_df10")[,6:8]
res0all <- cbind(res0_1,res0_2,res0_3,res0_4,res0_5,res0_6,res0_7,res0_8,res0_9,res0_10)
res0all <- data.frame(res0all)
res0all <- res0all[,order(colnames(res0all))]
res0all <- cbind(res1[,1:4],res0all)



res0[,5:7] <- res0[,5:7][,order(colnames(res0[,5:7]))]
colnames(res0) <- c(colnames(res0[,1:4]),"ELT", "ERT","sym")
head(res0[,5:7])

rp <- sample(1:4455,1);rp #random parameters

rp <- sample(1:4455,1);res0[rp,1:7];res1[rp,1:7]

delta <- res1$delta[1:11]

plot(0, ylim=c(-.1,.5), xlim=c(0,1), type='n', xlab=expression(delta))
lines(delta, res1[res1$sigma==0.4 & res1$mu1==0.1 & res1$mu2==0.1,]$Delta_B1ELT)

plotDeltaI <- function(df, params){
	
	x <- df[1:11,4]
	yLT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3],5]
	yRT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3],6]
	ysm <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3],7]
	
	y <- c(yLT, yRT, ysm)
	miny <- min(y)-abs(0.1*min(y))
	maxy <- max(y)+0.1*max(y)
	range <- c(miny,maxy)
	
	plot(0, ylim=range, xlim=c(0,1), type='n', xlab=expression(delta), ylab="DeltaI")
	lines(x, yLT, col='red', lty = 2, lwd=1.5)
	lines(x, yRT, col='blue', lty = 2, lwd=1.5)
	lines(x, ysm, col='green', lty = 2, lwd=1.5)
	mtext(paste("sigma=", params[1]," mu1=",params[2]," mu2=",params[3]), side=1, line=-1)
	legend("topleft", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
}

lines1DeltaI <- function(df, params){
	x <- df[1:11,4]
	df <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3],]

	for (i in 1:10){
		lines(x, df[,4+i], col=rgb(1,0,0, 0.2))
		lines(x, df[,14+i], col=rgb(0,0,1, 0.2))
		lines(x, df[,24+i], col=rgb(0,1,0, 0.2))
	}
}

muplot <- c(0.1,0.5,0.9)
plotsims <- length(sigma)*length(muplot)*length(muplot)
mu1.psims <- rep(muplot, times=plotsims/length(muplot))
mu2.psims <- sort(rep(muplot, times=length(muplot)))
sigma.psims <- sort(rep(sigma, times=plotsims/length(sigma)))
plotparammat<- cbind(sigma.psims, mu1.psims,mu2.psims)

pdf("run3_plotcheck.pdf")
for(i in 1:plotsims){
	plotDeltaI(res1, params=plotparammat[i,])
	lines1DeltaI(res1all, params=plotparammat[i,])
}
dev.off()

pdf("run1_plotcheck.pdf")
for(i in 1:plotsims){
	plotDeltaI(res0, params=plotparammat[i,])
	lines1DeltaI(res0all, params=plotparammat[i,])
}
dev.off()











