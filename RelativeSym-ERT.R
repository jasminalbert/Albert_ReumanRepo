
#testing relative difference of ERT from sym

#first approach
#working from mean_df

#remove 0's from df
xdf <- mean_df[mean_df$sym != 0,]
dif_fracR <- abs((xdf$sym-xdf$ERT)/xdf$sym)
#order xdf by dif_frac
xdf_fracR <- xdf[order(-dif_fracR),]
#add value to df
xdf_fracR$fracR <- abs((xdf_fracR$sym-xdf_fracR$ERT)/xdf_fracR$sym)
xdf_fracR <- xdf_fracR[,-5] #deleting ELT for ease of viewing

#----------------------------------------------------------

#second approach
#find values for all 10 dfs first
fracR <- matrix(NA, ncol = length(dflist), nrow = nrow(simdfx))

#add value to all dfs then isolate to find mean
for (i in 1:length(dflist)){
	dflist[[i]]$fracR <-(abs(dflist[[i]]$sym - dflist[[i]]$ERT))/dflist[[i]]$sym
	fracR[,i] <- dflist[[i]]$fracR
}

fracR_mean <- apply(fracR, MARGIN =1, mean)	

#then add mean to mean_df
mean_df$fracR <- fracR_mean

#then similar protocal as above
xdf <- mean_df[mean_df$sym != 0,]
xdf_fracR <- xdf[order(-xdf$fracR),]
xdf_fracR <- xdf_fracR[,c("sdev","mn1","mn2","delta","sym","ERT","fracR","mdif")]

#produces different results!!
#but i think second approach is more "sound"?

head(xdf_fracR,20)
tail(xdf_fracR,20)
hist(xdf_fracR$fracR,100, main="Hist of abs difference btwn sym and ERT relative to sym")
hist(xdf_fracR$fracR,200, xlim=c(-25,50), main=NULL)
hist(xdf_fracR$fracR,400, xlim=c(-10,20), main=NULL)
hist(xdf_fracR$fracR,800, xlim=c(-5,10), main=NULL)

#most values lie between 0:0.5
zdf<-xdf_fracR[xdf_fracR$fracR>0 & xdf_fracR$fracR<0.5,]
table(zdf$sdev)
table(zdf$mdif)
#contains more smaller mdif and more uniform sdev dis
table(zdf$mdif,zdf$sdev)
#smaller fracR values but contains bigger SEs


ydf<-xdf_fracR[xdf_fracR$fracR<0 & xdf_fracR$fracR<0.5,]
dim(ydf)
table(ydf$sdev)
table(ydf$mdif)
table(ydf$mdif,ydf$sdev)
#contains more lower sdev and lower small mdif (more uniform mdif dis)
#and smaller SE values
hist(xdf$mdif)
hist(ydf$mdif)
hist(zdf$mdif)
hist(ydf$ERT,12)
hist(zdf$ERT,40)
boxplot(ydf$ERT)
boxplot(zdf$ERT)
hist(zdf$fracR)
#-------------------------------------------------------------------

mean_df$mdif <- abs(mean_df$mn1-mean_df$mn2)

a <- mean_df[mean_df$sdev==1.6 & mean_df$delta==1.0,]

head(a)

plot(a$mdif, a$sym, type = 'l', col ='green')
lines(a$mdif, a$ERT, col = 'red')
lines(a$mdif, a$ELT, col = 'blue')


plot(xdf_fracR$sdev, type = 'l')
plot(xdf_fracR$mn1[1:100], type = 'l')
plot(xdf_fracR$mn2[1:100], type = 'l')
plot(xdf_fracR$mn1[1:100]-xdf_fracR$mn2[1:100], type = 'l')
plot(abs(xdf_fracR$mn1-xdf_fracR$mn2)[1:100], type = 'l')

plot(xdf_fracR$delta[1:100], type = 'l')

