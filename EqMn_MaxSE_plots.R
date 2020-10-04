source("/equalMeans_maxSE.R")
#additional plots related to figure 7 
#box plot and histogram


deltaABC <- data.frame(deltaABC)

#turn into boxplot

boxplot(
deltaABC[deltaABC$sdev==0.4,"elt"],
deltaABC[deltaABC$sdev==0.4,"ert"],
deltaABC[deltaABC$sdev==0.8,"elt"],
deltaABC[deltaABC$sdev==0.8,"ert"],
deltaABC[deltaABC$sdev==1.6,"elt"],
deltaABC[deltaABC$sdev==1.6,"ert"],
deltaABC[deltaABC$sdev==3.2,"elt"],
deltaABC[deltaABC$sdev==3.2,"ert"],
deltaABC[deltaABC$sdev==6.4,"elt"],
deltaABC[deltaABC$sdev==6.4,"ert"], 
border = "snow4",at = c(1,2,4,5,7,8,10,11,13,14),
horizontal = T, col = c("tomato1", "lightblue1"), axes = FALSE, frame.plot=FALSE, main = "DELTA at maximal SE when mn1=mn2")
axis(2, at = c(0,1.5,4.5,7.5,10.5,13.5), labels = c(0,sdev))
axis(1)
legend("topright", legend=c("left", "right"), fill = c("tomato1","lightblue1"))
mtext("sdev",2, line=3)
mtext("delta",1,line=3)



#histogram for max sym (always at 0.5)
hist(unlist(deltaA), xlim = c(-0.5,1), col = "mistyrose", border = "white", xlab = "delta", main = "Historgram of delta values at max SE values when mn1=mn2" )
mtext("Shows that when mean birth rates of both resident and invader  ", 1, -22, cex = .87)
mtext("(When mn1-mn2 = 0, max(SE) occurs when delta = 0.5)", 1, -20, cex = .7)
mtext("are equal, the highest SE value will be when the death rate is 0.5.", 1, -21, cex = .87)
mtext("SE value indicates success of invader coexisting with resident.", 1, -18, cex = .83 )
mtext("Probably could get more variation if delta was tested at higher resolution.",1, -14, cex = .7)
#proves all max SE values when mn1=mn2 is when delta = 0.5

#check in dataframe
eq_meanDF <- mean_df[mean_df$mn1 - mean_df$mn2 ==0 & mean_df$delta == 0.5, ]


#SE values increase as mean and sdev increase
plot(0, ylim = c(0.00705,1.00637), xlim = range(sdev),type = 'n', ylab = "SE", xlab = "sdev", main = "Delta I according to changes in sdev when mn1=mn2")
for(i in mn1){
	lines(sdev,eq_meanDF[eq_meanDF$mn1 == i, "sym"], type = 'l', col = rgb(0,1,0, 0.2), cex = 0.2)
	lines(sdev,eq_meanDF[eq_meanDF$mn1 == i, "ELT"], type = 'l', col = rgb(1,0,0, 0.2), cex = 0.2)
	lines(sdev,eq_meanDF[eq_meanDF$mn1 == i, "ERT"], type = 'l', col = rgb(0,0,1, 0.2), cex = 0.2)
}



plot(0, ylim = c(0.00705,0.0073), xlim = range(mn1),type = 'n', ylab = "SE", xlab = "mn1=mn2", main = "Delta I according to changes in mn", sub = "sdev =0.4")
lines(mn1,eq_meanDF[eq_meanDF$sdev == 0.4, "sym"], type = 'l', col = "green", )
lines(mn1,eq_meanDF[eq_meanDF$sdev == 0.4, "ELT"], type = 'l', col = "blue")
lines(mn1,eq_meanDF[eq_meanDF$sdev == 0.4, "ERT"], type = 'l', col = "red")
mtext("SE(0.9) - SE(0.1) =",adj = .03, padj=5, cex=0.3)
mtext(paste(round(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 0.4, "sym"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 0.4, "sym"], 11)), col = "green", adj = .03, padj=7, cex=0.3)
mtext(paste(format(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 0.4, "ELT"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 0.4, "ELT"],scientific = F, nsmall = 10)), col = "blue", adj = .03, padj=9, cex=0.3)
mtext(paste(format(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 0.4, "ERT"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 0.4, "ERT"], scientific = F, nsmall =10)), col = "red", adj = .03, padj=11, cex=0.3)
#mtext(paste("avg((SE_sym/SE_ERT)/SE_sym) =",round(mean(eq_meanDF$fracR[1:9]),8)), adj = 0.22, padj=5, cex=0.3)
#mtext(paste("avg(SE_sym - avg(SE_ERT, SE_ELT)) =",format(mean(c(mean(eq_meanDF$sym[1:9]-eq_meanDF$ERT[1:9]),mean(eq_meanDF$sym[1:9]-eq_meanDF$ELT[1:9]))),scientific=F,nsmall=7)), adj = 0.7, padj=5, cex=0.3)


plot(0, ylim = c(0.027,0.0286), xlim = range(mn1),type = 'n', ylab = "SE", xlab = "mn1=mn2", main = "Delta I according to changes in mn", sub = "sdev =0.8")
lines(mn1,eq_meanDF[eq_meanDF$sdev == 0.8, "sym"], type = 'l', col = "green", )
lines(mn1,eq_meanDF[eq_meanDF$sdev == 0.8, "ELT"], type = 'l', col = "blue")
lines(mn1,eq_meanDF[eq_meanDF$sdev == 0.8, "ERT"], type = 'l', col = "red")
mtext("SE(0.9) - SE(0.1) =",adj = .03, padj=5, cex=0.3)
mtext(paste(round(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 0.8, "sym"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 0.8, "sym"], 11)), col = "green", adj = .03, padj=7, cex=0.3)
mtext(paste(format(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 0.8, "ELT"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 0.8, "ELT"],scientific = F, nsmall = 10)), col = "blue", adj = .03, padj=9, cex=0.3)
mtext(paste(format(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 0.8, "ERT"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 0.8, "ERT"], scientific = F, nsmall =10)), col = "red", adj = .03, padj=11, cex=0.3)
#mtext(paste("avg((SE_sym/SE_ERT)/SE_sym) =",round(mean(eq_meanDF$fracR[10:18]),8)), adj = 0.22,padj=5, cex=0.3)
#mtext(paste("avg(SE_sym - avg(SE_ERT, SE_ELT)) =",format(mean(c(mean(eq_meanDF$sym[10:18]-eq_meanDF$ERT[10:18]),mean(eq_meanDF$sym[10:18]-eq_meanDF$ELT[10:18]))),scientific=F,nsmall=7)), adj = 0.7, padj=5, cex=0.3)

plot(0, ylim = c(0.096,0.107), xlim = range(mn1),type = 'n', ylab = "SE", xlab = "mn1=mn2", main = "Delta I according to changes in mn", sub = "sdev =1.6")
lines(mn1,eq_meanDF[eq_meanDF$sdev == 1.6, "sym"], type = 'l', col = "green", )
lines(mn1,eq_meanDF[eq_meanDF$sdev == 1.6, "ELT"], type = 'l', col = "blue")
lines(mn1,eq_meanDF[eq_meanDF$sdev == 1.6, "ERT"], type = 'l', col = "red")
mtext("SE(0.9) - SE(0.1) =",adj = .03, padj=5, cex=0.3)
mtext(paste(round(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 1.6, "sym"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 1.6, "sym"], 11)), col = "green", adj = .03, padj=7, cex=0.3)
mtext(paste(format(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 1.6, "ELT"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 1.6, "ELT"],scientific = F, nsmall = 10)), col = "blue", adj = .03, padj=9, cex=0.3)
mtext(paste(format(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 1.6, "ERT"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 1.6, "ERT"], scientific = F, nsmall =10)), col = "red", adj = .03, padj=11, cex=0.3)
#mtext(paste("avg((SE_sym/SE_ERT)/SE_sym) =",round(mean(eq_meanDF$fracR[19:27]),8)), adj = 0.22, padj=5, cex=0.3)
#mtext(paste("avg(SE_sym - avg(SE_ERT, SE_ELT)) =",format(mean(c(mean(eq_meanDF$sym[19:27]-eq_meanDF$ERT[19:27]),mean(eq_meanDF$sym[19:27]-eq_meanDF$ELT[19:27]))),scientific=F,nsmall=7)), adj = 0.7, padj=5, cex=0.3)

plot(0, ylim = c(0.29,0.353), xlim = range(mn1),type = 'n', ylab = "SE", xlab = "mn1=mn2", main = "Delta I according to changes in mn", sub = "sdev =3.2")
lines(mn1,eq_meanDF[eq_meanDF$sdev == 3.2, "sym"], type = 'l', col = "green", )
lines(mn1,eq_meanDF[eq_meanDF$sdev == 3.2, "ELT"], type = 'l', col = "blue")
lines(mn1,eq_meanDF[eq_meanDF$sdev == 3.2, "ERT"], type = 'l', col = "red")
mtext("SE(0.9) - SE(0.1) =",adj = .03, padj=5, cex=0.3)
mtext(paste(round(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 3.2, "sym"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 3.2, "sym"], 11)), col = "green", adj = .03, padj=7, cex=0.3)
mtext(paste(format(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 3.2, "ELT"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 3.2, "ELT"],scientific = F, nsmall = 10)), col = "blue", adj = .03, padj=9, cex=0.3)
mtext(paste(format(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 3.2, "ERT"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 3.2, "ERT"], scientific = F, nsmall =10)), col = "red", adj = .03, padj=11, cex=0.3)
#mtext(paste("avg((SE_sym/SE_ERT)/SE_sym) =",round(mean(eq_meanDF$fracR[28:36]),8)), adj = 0.22, padj=5, cex=0.3)
#mtext(paste("avg(SE_sym - avg(SE_ERT, SE_ELT)) =",format(mean(c(mean(eq_meanDF$sym[28:36]-eq_meanDF$ERT[28:36]),mean(eq_meanDF$sym[28:36]-eq_meanDF$ELT[28:36]))),scientific=F,nsmall=7)), adj = 0.7, padj=5, cex=0.3)

plot(0, ylim = c(0.7749,1.00637), xlim = range(mn1),type = 'n', ylab = "SE", xlab = "mn1=mn2", main = "Delta I according to changes in mn", sub = "sdev =6.4")
lines(mn1,eq_meanDF[eq_meanDF$sdev == 6.4, "sym"], type = 'l', col = "green", )
lines(mn1,eq_meanDF[eq_meanDF$sdev == 6.4, "ELT"], type = 'l', col = "blue")
lines(mn1,eq_meanDF[eq_meanDF$sdev == 6.4, "ERT"], type = 'l', col = "red")
mtext("SE(0.9) - SE(0.1) =",adj = .03, padj=5, cex=0.3)
mtext(paste(round(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 6.4, "sym"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 6.4, "sym"], 11)), col = "green", adj = .03, padj=7, cex=0.3)
mtext(paste(format(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 6.4, "ELT"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 6.4, "ELT"],scientific = F, nsmall = 10)), col = "blue", adj = .03, padj=9, cex=0.3)
mtext(paste(format(eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == 6.4, "ERT"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == 6.4, "ERT"], scientific = F, nsmall =10)), col = "red", adj = .03, padj=11, cex=0.3)
#mtext(paste("avg((SE_sym/SE_ERT)/SE_sym) =",round(mean(eq_meanDF$fracR[37:45]),8)), adj = 0.22, padj=5, cex=0.3)
#mtext(paste("avg(SE_sym - avg(SE_ERT, SE_ELT)) =",format(mean(c(mean(eq_meanDF$sym[37:45]-eq_meanDF$ERT[37:45]),mean(eq_meanDF$sym[37:45]-eq_meanDF$ELT[37:45]))),scientific=F,nsmall=7)), adj = 0.7, padj=5, cex=0.3)



#seems like increasing mean does not make a differnce in this case
#prove with histogram of SE(0.9)-SE(0.1)
eq_meanDF
symRange <- NA
ELTRange <- NA
ERTRange <- NA
for (i in 1:length(sdev)){
	symRange[i] <- eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == sdev[i], "sym"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == sdev[i], "sym"]
	ELTRange[i] <- eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == sdev[i], "ELT"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == sdev[i], "ELT"]
	ERTRange[i] <- eq_meanDF[eq_meanDF$mn1 == 0.9 & eq_meanDF$sdev == sdev[i], "ERT"] - eq_meanDF[eq_meanDF$mn1 == 0.1 & eq_meanDF$sdev == sdev[i], "ERT"]
}

Hsym<-hist(symRange,5, plot=F)
HELT <-hist(ELTRange,5, plot=F)
HERT<-hist(ERTRange,5, plot=F)

plot(Hsym, col =rgb(0,1,0, 0.1), xlim = c(-.005,.005), main = "Histogram of ranges of SE value between max and min mean when mn1=mn2 and delta=0.5 for differing sdev", xlab = "range value")
plot(HELT, col =rgb(0,0,1, 0.1) , add=T)
plot(HERT, col =rgb(1,0,0, 0.1) , add=T)
mtext("brown = green+red+blue", adj = 0.01, padj =3)
mtext("pink = red+blue",adj = 0.01, padj=4.5)
mtext("yellow = green+red",adj = 0.01, padj=6)
mtext("shows that mean value is negligible for SE value in this scenario", padj = 67, adj =0.01, cex = 0.6)
mtext("(mn1=mn2 and max SE from delta=0.5)", padj = 68.5, adj =0.01, cex=0.6)
mtext("consisent across sdev values and noise types", padj = 70, adj =0.01, cex = 0.6)









