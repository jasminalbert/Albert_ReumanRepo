#script for figures 2-8 in summary document


#FIGURE 2
source("./plot_paramDelta.R")

pdf("figure21.pdf")

par(mar=c(5,3,2,0.8), mgp = c(3,1,0), mfrow = c(1,2), oma = c(6,2,6,1))

#sym = extinct
plot_paramDelta(sdev=6.4, mn1=.8, mn2=0.8, list=dflist, mean_df=mean_df, range = c(-0.02,.04), xlim = c(.99,1), xaxt ="n", yaxt = "n",cex.lab=1.5, cex.main=1.2,main=expression(paste(sigma, "=",6.4,", ", mu,"1=",0.8,", ", mu,"2=", 0.8)))
axis(1, cex.axis=1.2)
axis(2, cex.axis=1.2)
abline(h=0, lty=3)

#TASS = extinct 
plot_paramDelta(sdev=1.6, mn1=.1, mn2=0.1, list=dflist, mean_df=mean_df, range = c(-0.005,.01), xlim = c(.99,1), xaxt ="n", yaxt = "n",cex.lab=1.5, cex.main=1.2,main=expression(paste(sigma, "=",1.6,", ", mu,"1=",0.1,", ", mu,"2=", 0.1)))
axis(1, cex.axis=1.2)
axis(2, cex.axis=1.2)
abline(h=0, lty=3)

title(main = expression(paste("Storage effect according to changes in ",delta)),cex.main=1.2, outer = T, line=0.6)
legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0, cex = 1.1)

mtext("SE", 2, outer = T, line =0.5, cex = 1.25)
dev.off()

#FIGURE 3

pdf("figure3.1.1.pdf")
par(mfrow = c(1,1),mar=c(5,4.5,4,2), oma=c(1,0,0,0))

plot_paramDelta(sdev=0.4, mn1=0.1, mn2=0.9, list=dflist, mean_df=mean_df, range = c(-0.006,.016), main = expression(paste("SE vs. ",delta)), ylab = "SE",cex.lab=1.5)
legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
mtext(expression(paste(sigma, "=",0.4,", ", mu,"1=",0.1,", ", mu,"2=", 0.9)), 1, outer = T, line =-0.55, cex = 1.25)

dev.off()

pdf("figure3.1.2.pdf")
par(mfrow = c(1,1),mar=c(5,4.5,4,2), oma=c(1,0,0,0))

plot_paramDelta2(sdev=0.4, mn1=0.1, mn2=0.9, list=dflist, mean_df=mean_df, range = c(-0.00155,.00155), cex.lab=1.5)
mtext(expression(paste(sigma, "=",0.4,", ", mu,"1=",0.1,", ", mu,"2=", 0.9)), 1, outer = T, line =-0.55, cex = 1.25)

dev.off()
###
pdf("figure3.2.1.pdf")
par(mfrow = c(1,1),mar=c(5,4.5,4,2), oma=c(1,0,0,0))

plot_paramDelta(sdev=1.6, mn1=0.1, mn2=0.9, list=dflist, mean_df=mean_df, range = c(-0.03,.155), main = expression(paste("SE vs. ",delta)), ylab = "SE",cex.lab=1.5)
legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
mtext(expression(paste(sigma, "=",1.6,", ", mu,"1=",0.1,", ", mu,"2=", 0.9)), 1, outer = T, line =-0.55, cex = 1.25)

dev.off()

pdf("figure3.2.2.pdf")
par(mfrow = c(1,1),mar=c(5,4.5,4,2), oma=c(1,0,0,0))

plot_paramDelta2(sdev=1.6, mn1=0.1, mn2=0.9, list=dflist, mean_df=mean_df, range = c(-0.016,.011), cex.lab=1.5)
mtext(expression(paste(sigma, "=",1.6,", ", mu,"1=",0.1,", ", mu,"2=", 0.9)), 1, outer = T, line =-0.55, cex = 1.25)

dev.off()
###
pdf("figure3.3.1.pdf")
par(mfrow = c(1,1),mar=c(5,4.5,4,2), oma=c(1,0,0,0))

plot_paramDelta(sdev=6.4, mn1=0.1, mn2=0.9, list=dflist, mean_df=mean_df, range = c(-0.05,1.13), main = expression(paste("SE vs. ",delta)), ylab = "SE",cex.lab=1.5)
legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
mtext(expression(paste(sigma, "=",6.4,", ", mu,"1=",0.1,", ", mu,"2=", 0.9)), 1, outer = T, line =-0.55, cex = 1.25)

dev.off()

pdf("figure3.3.2.pdf")
par(mfrow = c(1,1),mar=c(5,4.5,4,2), oma=c(1,0,0,0))

plot_paramDelta2(sdev=6.4, mn1=0.1, mn2=0.9, list=dflist, mean_df=mean_df, range = c(-0.23,.051), cex.lab=1.5)
mtext(expression(paste(sigma, "=",0.4,", ", mu,"1=",0.1,", ", mu,"2=", 0.9)), 1, outer = T, line =-0.55, cex = 1.25)


dev.off()

#FIGURE 5
source("/dif_meanMaxSE.R")
#color
bcol <- brewer.pal(n=5, "Paired")
#5.1
pdf("figure5.1.pdf")

plot(0, ylim = c(.2,.8), xlim =range(deltaMdifD$mdif),type = 'n', ylab = expression(paste(delta)), xlab = expression(paste(mu,"1-", mu,"2")), main = "Maximum storage effects", yaxt = "n", xaxt = "n",cex.lab=1.3, cex.main=1.3)
axis(1, at=seq(-.8,.8,.1))
axis(2, at=delta)

for (i in 5:1){
	points(jitter(deltaMdifD[deltaMdifD$sdev==sdev[i], "mdif"]), jitter(deltaMdifD[deltaMdifD$sdev==sdev[i], "delta"]), col = bcol[i], pch=c(21,24,24))
}
dev.off()

pdf("figure5_legend.pdf")
plot.new()
legend(0.45,0.5,legend=sdev,fill=bcol, title=expression(paste(sigma)), cex=.9, horiz=T)
legend(-0.0,0.5,legend=c("asymmetric","symmetric"), pch = c(24,21),title="noise type",cex=.9, horiz=T)
dev.off()

#5.2
pdf("figure5.2.pdf")
plot(0, ylim = c(.2,.8), xlim =range(deltaMdifE$mdif),type = 'n', ylab = expression(paste(delta)), xlab = expression(paste(mu,"1-", mu,"2")), main = "Maximum absolute difference", yaxt = "n", xaxt = "n")

for (i in 5:1){
	axis(1, at=seq(-.8,.8,.1))
	axis(2, at=delta)
	points(jitter(deltaMdifE[deltaMdifE$sdev==sdev[i], "mdif"]), jitter(deltaMdifE[deltaMdifE$sdev==sdev[i], "delta"]), col = bcol[i], pch=c(24))
}
dev.off()

#FIGURE 6

pdf("figure6.1.pdf")
plot(0, ylim = c(0,1.00637), xlim = range(mn1),type = 'n', ylab = "SE", xlab = expression(paste(mu,"1 = ",mu,"2")), main = expression(paste("SE according to changes in ",mu,"1 = ",mu,"2")))

for (i in sdev){
	lines(mn1,eq_meanDF[eq_meanDF$sdev == i, "sym"], type = 'l', col = "green", )
	lines(mn1,eq_meanDF[eq_meanDF$sdev == i, "ELT"], type = 'l', col = "blue")
	lines(mn1,eq_meanDF[eq_meanDF$sdev == i, "ERT"], type = 'l', col = "red")
}
legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
dev.off()
##
pdf("figure6.2.pdf")

plot_paramDelta(sdev=1.6, mn1=0.1, mn2=0.1, list=dflist, mean_df=mean_df, range = c(-0.006,.155), main = expression(paste("SE vs. ",delta)), ylab = "SE",cex.lab=1.5)
legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
mtext(expression(paste(sigma, "=",1.6,", ", mu,"1=",0.1,", ", mu,"2=", 0.1)), 1, outer = T, line =-0.55, cex = 1.25)

dev.off()
##
pdf("figure6.3.pdf")

plot_paramDelta(sdev=1.6, mn1=0.6, mn2=0.6, list=dflist, mean_df=mean_df, range = c(-0.006,.155), main = expression(paste("SE vs. ",delta)), ylab = "SE",cex.lab=1.5)
legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
mtext(expression(paste(sigma, "=",1.6,", ", mu,"1=",0.6,", ", mu,"2=", 0.6)), 1, outer = T, line =-0.55, cex = 1.25)

dev.off()

#FIGURE 7
source("/equalMeans_maxSE.R")
pdf("figure7.1.pdf")

plot(0, xlim = c(0,6.4), ylim =c(0.3,1),type = 'n', xlab = expression(paste(sigma)), ylab = expression(paste("maximal ", delta)), main =expression(paste(delta," at maximum absolute SE difference when ",mu,"1 = ",mu,"2")), yaxt = "n", xaxt = "n", cex.lab = 1.25, cex.main = 1.3)

axis(1, at = sdev, labels =sdev, cex.axis = 1.2)
axis(2, at = delta[4:11], cex.axis = 1.2)

points(y=jitter(deltaABC[,"elt"],1.2), x=deltaABC[,"sdev"])
points(y=jitter(deltaABC[,"ert"],1.2), x=deltaABC[,"sdev"])

dev.off()
##
pdf("figure7.2.1.pdf")
par(mfrow = c(1,1),mar=c(5,4.5,4,2), oma=c(1,0,0,0))

par(mfrow = c(1,1),mar=c(5,4.5,4,2), oma=c(1,0,0,0))

plot_paramDelta2(sdev=0.4, mn1=0.5, mn2=0.5, list=dflist, mean_df=mean_df, range = c(-0.00155,.00155), cex.lab=1.5)
mtext(expression(paste(sigma, "=",0.4,", ", mu,"1=",0.5,", ", mu,"2=", 0.5)), 1, outer = T, line =-0.55, cex = 1.25)

dev.off()

pdf("figure7.2.2.pdf")
par(mfrow = c(1,1),mar=c(5,4.5,4,2), oma=c(1,0,0,0))

plot_paramDelta2(sdev=6.4, mn1=0.5, mn2=0.5, list=dflist, mean_df=mean_df, range = c(-0.26,.052), cex.lab=1.5)
mtext(expression(paste(sigma, "=",6.4,", ", mu,"1=",0.5,", ", mu,"2=", 0.5)), 1, outer = T, line =-0.55, cex = 1.25)

dev.off()

#FIGURE 8 - using most extreme difference between asym & sym SE
source("/pop.sim_function.R")
#(6.4,0.9,0.9,0.5)
mu1<-0.9
mu2<-0.9
sigma<-6.4
d<-0.5
n<-10
N<- 50
N<-25
time<-10000
dom<-0.95

#initialize list to store measurements
co.list <- vector(mode='list', length=n)

for(i in 1:n){
	co.list[[i]] <- get_noise(mn=c(mu1,mu2),sdev=c(sigma,sigma),n=time,check = F)
	}

#remove sharps
co.list <- lapply(co.list, function(X){X[-c(2,4,6)]})

#separate noise types to average
co_ELT <- lapply(co.list, function(X){return(X$B_ELT)})
co_sym <- lapply(co.list, function(X){return(X$B_sym)})
co_ERT <- lapply(co.list, function(X){return(X$B_ERT)})

#population simulation
pop.L <- lapply(co_ELT, function(X){pop.sim(b=X, N=N, N1=N1, d=d, time=time)})
pop.s <- lapply(co_sym, function(X){pop.sim(b=X, N=N, N1=N1, d=d, time=time)})
pop.R <- lapply(co_ERT, function(X){pop.sim(b=X, N=N, N1=N1, d=d, time=time)})

#coexistence period 
coexist.L <- lapply(pop.L, function(X){X[,1] < dom*N & X[,1] > (1-dom)*N})
RLE.L <- lapply(coexist.L, rle)
co_periods.L <- lapply(RLE.L, function(X){X$lengths[X$values == TRUE]})

coexist.s <- lapply(pop.s, function(X){X[,1] < dom*N & X[,1] > (1-dom)*N})
RLE.s <- lapply(coexist.s, rle)
co_periods.s <- lapply(RLE.s, function(X){X$lengths[X$values == TRUE]})

coexist.R <- lapply(pop.R, function(X){X[,1] < dom*N & X[,1] > (1-dom)*N})
RLE.R <- lapply(coexist.R, rle)
co_periods.R <- lapply(RLE.R, function(X){X$lengths[X$values == TRUE]})

#now get metrics

#co.fraction: fraction of time of noticeable coexistence 
fraction.L<-(lapply(co_periods.L, function(X){sum(X)/time}))
fraction.s<-(lapply(co_periods.s, function(X){sum(X)/time}))
fraction.R<-(lapply(co_periods.R, function(X){sum(X)/time}))
co.fraction<-c(mean(unlist(fraction.L)),mean(unlist(fraction.s)),mean(unlist(fraction.R)))

#co.mean: mean length of noticable coexistence periods
mean.L<-(lapply(co_periods.L,mean))
mean.s<-(lapply(co_periods.s,mean))
mean.R<-(lapply(co_periods.R,mean))
co.mean<-c(mean(unlist(mean.L)),mean(unlist(mean.s)),mean(unlist(mean.R)))

#co.num: number of noticeable coexistence periods
num.L<-lapply(co_periods.L,length)
num.s<-lapply(co_periods.s,length)
num.R<-lapply(co_periods.R,length)
co.num<-c(mean(unlist(num.L)),mean(unlist(num.s)),mean(unlist(num.R)))

#combine into one dataframe
co_periods<-data.frame(co.fraction=co.fraction,co.num=co.num,co.mean=co.mean, row.names = c("ELT","sym","ERT"))

names(co_periods)<-c("co.fraction","co.num","co.mean")

#now make plot
pdf('co.sim3.pdf', width=13, height=8)
mat <- matrix(c(
0,1,1,1,1,1,1,1,2,2,2,2,2,
9,3,3,3,3,3,3,3,4,4,4,4,4,
9,3,3,3,3,3,3,3,4,4,4,4,4,
9,3,3,3,3,3,3,3,4,4,4,4,4,
10,5,5,5,5,5,5,5,6,6,6,6,6,
10,5,5,5,5,5,5,5,6,6,6,6,6,
10,5,5,5,5,5,5,5,6,6,6,6,6,
11,7,7,7,7,7,7,7,8,8,8,8,8,
11,7,7,7,7,7,7,7,8,8,8,8,8,
11,7,7,7,7,7,7,7,8,8,8,8,8,
14,12,12,12,12,12,12,12,13,13,13,13,13), nrow=11, byrow=T)
layout(mat=mat)
marp<-par(mar=c(2,4,2,3))

u<-c(300,450,450,300)
v<-c(500,500,1500,1500)

#titles
plot.new()
mtext("Population simulation of sp1",1, cex=1.5)
plot.new()
mtext("Histogram of periods of",1,-.5,cex=1.5)
mtext("noticeable coexistence",1,1.5,cex=1.5)

#ELT
plot(pop.L[[1]][1:10000,1],type='l', xlab=NULL,ylab="sp 1 abundance",xaxt ="n", yaxt = "n", cex.lab=1.2)
axis(1, cex.axis=1.5)
axis(2, cex.axis=1.5)

hist(unlist(co_periods.L),ylim=c(0,2050), breaks=brk, main=NULL,xlab=NULL,xaxt ="n", yaxt = "n", cex.lab=1.2)
axis(1, cex.axis=1.5)
axis(2, cex.axis=1.5)
abline(v=co_periods["ELT","co.mean"],col="red", lty=2, lwd=2)
polygon(x=u,y=v, border="lightgray")
mtext(paste("co.frac= ", signif(co_periods["ELT","co.fraction"],3)), at=305, adj=0, line=-5)
mtext(paste("co.num= ", signif(co_periods["ELT","co.num"],3)), at=305, adj=0, line=-7)
mtext(paste("co.mean= ", signif(co_periods["ELT","co.mean"],3)), at=305, adj=0, line=-9)


#sym
plot(pop.s[[1]][1:10000,1],type='l', xlab=NULL,ylab="sp 1 abundance",xaxt ="n", yaxt = "n", cex.lab=1.2)
axis(1, cex.axis=1.5)
axis(2, cex.axis=1.5)

hist(unlist(co_periods.s),ylim=c(0,2050),breaks=brk,main=NULL,xlab=NULL,xaxt ="n", yaxt = "n", cex.lab=1.2)
axis(1, cex.axis=1.5)
axis(2, cex.axis=1.5)
abline(v=co_periods["sym","co.mean"],col="red", lty=2, lwd=2)
polygon(x=u,y=v, border="lightgray")
mtext(paste("co.frac= ", signif(co_periods["sym","co.fraction"],3)), at=305, adj=0, line=-5)
mtext(paste("co.num= ", signif(co_periods["sym","co.num"],3)), at=305, adj=0, line=-7)
mtext(paste("co.mean= ", signif(co_periods["sym","co.mean"],3)), at=305, adj=0, line=-9)

#ERT
plot(pop.R[[1]][1:10000,1],type='l', xlab=NULL,ylab="sp 1 abundance",xaxt ="n", yaxt = "n", cex.lab=1.2)
axis(1, cex.axis=1.5)
axis(2, cex.axis=1.5)

hist(unlist(co_periods.R),ylim=c(0,2050),breaks=brk,main=NULL,xlab=NULL,xaxt ="n", yaxt = "n", cex.lab=1.2)
axis(1, cex.axis=1.5)
axis(2, cex.axis=1.5)
abline(v=co_periods["ERT","co.mean"],col="red", lty=2, lwd=2)
polygon(x=u,y=v, border="lightgray")
mtext(paste("co.frac= ", signif(co_periods["ERT","co.fraction"],3)), at=305, adj=0, line=-5)
mtext(paste("co.num= ", signif(co_periods["ERT","co.num"],3)), at=305, adj=0, line=-7)
mtext(paste("co.mean= ", signif(co_periods["ERT","co.mean"],3)), at=305, adj=0, line=-9)

#left labels
plot.new()
mtext("Extreme left tail", 4,cex=1.5)
plot.new()
mtext("Symmetric", 4, cex=1.5)
plot.new()
mtext("Extreme right tail", 4, cex=1.5)

#x labels
plot.new()
mtext("timesteps",line=-1)
plot.new()
mtext("length of noticeable coexistence",line=-1)

plot.new()

dev.off()



























