#how increases and decreases in mdif increase and shift the delta value for max SE

#make column for mean diffrence
mean_df$mdif <- mean_df$mn1-mean_df$mn2

#take out columns unessecary for this analysis
dif_meanDF <- mean_df[, -c(8,10,13)]




#make list to store the dataframe broken into chunks by mean difference
#every list element has unique combo of mdif and sdev

dif_meanList <- vector(mode = 'list', length = nrow(dif_meanDF)/length(delta))

for (i in 0:(length(dif_meanList)-1)){
	dif_meanList[[i+1]] <- dif_meanDF[(i*11)+1:11,]
}



#list that finds that maximun SE for all three noise type
#for all elements of dif_meanList
#work with this first to make plots
listD <- lapply(dif_meanList, function(x){rbind(x[x$sym == max(x$sym),] , x[x$ELT == max(x$ELT),] , x[x$ELT == max(x$ELT),])})


#list that finds maximum absolute value of different btwn
#SE value of symmetric and extreme noise
listE <- lapply(dif_meanList, function(x){rbind(x[abs(x$ert) == max(abs(x$ert)),], x[abs(x$elt) == max(abs(x$elt)),])})


#check if maxima are ever higher in extreme noise than symmetric noise - not the case for maximal SE
listF <- lapply(dif_meanList, function(x) {print(x[x$sym < x$ERT | x$sym < x$ELT,])})


#turning listD into a dataframe
deltaD <- lapply(listD, function(x){print(x$delta)})
mdifD <- lapply(listD, function(x){print(x$mdif)})
sdevD <- lapply(listD, function(x){print(x$sdev)})
ELTD <- lapply(listD, function(x){print(x$ELT)})
ERTD <- lapply(listD, function(x){print(x$ERT)})
symD <- lapply(listD, function(x){print(x$sym)})

deltaMdifD <- data.frame(sdev = unlist(sdevD),delta = unlist(deltaD), mdif = unlist(mdifD), sym = unlist(symD), ELT = unlist(ELTD), ERT = unlist(ERTD))

#plotting delta against mdif to check for correlations
plot(y=deltaMdif$delta, x=deltaMdif$mdif)
#for the parameters we checked, there will never be a maximum at outside 0.3<delta<0.7



#for noise difference: turning listE into dataframe
#will ise later
deltaE <- lapply(listE, function(x){print(x$delta)})
mdifE <- lapply(listE, function(x){print(x$mdif)})
sdE <- lapply(listE, function(x){print(x$sdev)})
ertE <- lapply(listE, function(x){print(x$ert)})
eltE <- lapply(listE, function(x){print(x$elt)})

deltaMdifE <- data.frame(delta = unlist(deltaE), mdif = unlist(mdifE), sdev=unlist(sdE), ert=unlist(ertE), elt=unlist(eltE))


#-----------PLOTS----------------------------------------


#color
bcol <- brewer.pal(n=5, "Paired")


#use jitter to see all points(general SE)
#correlation btwn mdif and delta

par(bg="gray20", col.axis="lavender", col.lab="lightyellow", col.main="azure", bty="n")

plot(0, ylim = c(.2,.8), xlim =range(deltaMdifD$mdif),type = 'n', ylab = "DELTA", xlab = "MDIF", main = "DELTA at max SE vs Mdif", yaxt = "n", xaxt = "n")

axis(1, at=seq(-.8,.8,.1), col="lavender")
axis(2, at=delta, col="lavender")

for (i in 1:5){
	
	points(jitter(deltaMdifD[deltaMdifD$sdev==sdev[i], "mdif"]), jitter(deltaMdifD[deltaMdifD$sdev==sdev[i], "delta"]), col = bcol[i], pch=c(21,25,24))
}

legend("bottom",legend=sdev,fill=bcol, title="sdev", cex=1.7, horiz=T)
legend("center",legend=c("left","right","symmetric"), pch = c(25,24,21), col="lavender",title="noise type",cex=1.8)

#should convert into boxplots?
boxplot()


#jitter plot for noise difference
par(bg="gray20", col.axis="lavender", col.lab="lightyellow", col.main="azure", bty="n", mfrow=c(3,2))


plot(0, ylim = c(.2,.8), xlim =range(deltaMdifE$mdif),type = 'n', ylab = "DELTA", xlab = "MDIF", main = "DELTA at max SEdif vs Mdif", yaxt = "n", xaxt = "n")

for (i in 1:5){
	
	
	axis(1, at=seq(-.8,.8,.1), col="lavender")
	axis(2, at=delta, col="lavender")
	points(jitter(deltaMdifE[deltaMdifE$sdev==sdev[i], "mdif"]), jitter(deltaMdifE[deltaMdifE$sdev==sdev[i], "delta"]), col = bcol[i], pch=c(25,24))
}







#look at most extreme cases: 0.8 and -0.8
deltaMdifD[deltaMdifD$mdif == 0.8,]
for (i in -seq(0.3,0.8,.1)){
	plot(deltaMdifD[deltaMdifD$mdif == i,"sdev"], deltaMdifD[deltaMdifD$mdif == i,"sym"], col = 'green', type = 'l', main = paste("SE vs. sdev, mdif =",i), ylab="SE value", xlab="sdev")
	lines(deltaMdifD[deltaMdif$mdif == i,"sdev"], deltaMdif[deltaMdif$mdif == i,"ELT"], col = 'blue')
	lines(deltaMdifD[deltaMdif$mdif == i,"sdev"], deltaMdif[deltaMdif$mdif == i,"ERT"], col = 'red')
}


#histogram

col2rgb(c("tomato1", "goldenrod1", "lightgoldenrod1", "forestgreen", "dodgerblue1"))

mycols <- c(rgb(255,99,71,100,names=NULL,255), rgb(255,193,37,100,names=NULL,255), rgb(225,236,139,100,names=NULL,255), rgb(34,139,34,100,names=NULL,255), rgb(30,144,255,100,names=NULL,255))



maxDeltas <- sort(unique(deltaMdif$delta))


hist(deltaMdif[deltaMdif$delta == maxDeltas[1], "mdif"], breaks=seq(-1,1,.1), ylim = c(0,8), col = mycols[1])
for (i in 2:length(maxDeltas)){
	hist(deltaMdif[deltaMdif$delta == maxDeltas[i], "mdif"], add=T, breaks=seq(-1,1,.1),col = mycols[i])
}
#more negative mean differences are associated with max SE value at a lower delta






