# plot DeltaI v delta
plotDeltaI <- function(df, params, d, dif=T){
	
	#x <- df[1:11,4]
	yLT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],5]
	yRT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],6]
	ysm <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],7]
	
	if (dif == T){
		rlavg <- rowMeans(cbind(yLT, yRT))
		ysm <- ysm - rlavg
		yLT <- yLT - rlavg
		yRT <- yRT - rlavg
	}
	
	y <- c(yLT, yRT, ysm)
	miny <- min(y)-abs(0.1*min(y))
	maxy <- max(y)+0.1*max(y)
	range <- c(miny,maxy)
	
	plot(0, ylim=range, xlim=range(d), type='n', xlab=expression(delta), ylab="DeltaI")
	lines(d, yLT, col='red', lty = 2, lwd=1.5)
	lines(d, yRT, col='blue', lty = 2, lwd=1.5)
	lines(d, ysm, col='green', lty = 2, lwd=1.5)
	mtext(paste("sigma=", params[1]," mu1=",params[2]," mu2=",params[3]), side=1, line=-1)
	legend("topleft", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
}

lines1DeltaI <- function(df, params, pdf, dif=F){
	x <- pdf[1:11,4]
	df <- df[pdf[,1]==params[1] & pdf[,2]==params[2] & pdf[,3]==params[3],]

	
	for (i in 1:10){
		lt <- df[,0+i]
		rt <- df[,10+i]
		sym <- df[,20+i]
		
		if (dif ==T){
			avg <- rowMeans(cbind(lt,rt))
			sym <- sym - avg
			lt <- lt - avg
			rt <- rt - avg
		}
		
		lines(x, lt, col=rgb(1,0,0, 0.2))
		lines(x, rt, col=rgb(0,0,1, 0.2))
		lines(x, sym, col=rgb(0,1,0, 0.2))
	}
}
#------------------------------------------------------------------------

# plot co mean v delta 
plotCoM <- function(df, params, d = seq(0,1,0.1),log = F, dif = F){
	
	#x <- df[1:11,4]
	yLT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],8]
	yRT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],9]
	ysm <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],10]
	
	ylab <- "Co period length (mean)"
	if (log == T){
		yLT <- log(yLT+1)
		yRT <- log(yRT+1)
		ysm <- log(ysm+1)
		ylab <- "Co period length (mean), ln(y+1)"
	}
	
	if (dif == T){
		rlavg <- rowMeans(cbind(yLT, yRT))
		ysm <- ysm - rlavg
		yLT <- yLT - rlavg
		yRT <- yRT - rlavg
	}
	
	y <- c(yLT, yRT, ysm)
	miny <- min(y)-abs(0.05*min(y))
	maxy <- max(y)+0.05*max(y)
	range <- c(miny,maxy)
	
	plot(0, ylim=range, xlim=range(d), type='n', xlab=expression(delta), ylab=ylab)
	lines(d, yLT, col='red', lty = 2, lwd=1.5)
	lines(d, yRT, col='blue', lty = 2, lwd=1.5)
	lines(d, ysm, col='green', lty = 2, lwd=1.5)
	mtext(paste("sigma=", params[1]," mu1=",params[2]," mu2=",params[3]), side=1, line=-1)
	legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
	
}

linesCoM <- function(df, params, pdf, log=F, dif=F){
	x <- pdf[1:11,4]
	df <- df[pdf[,1]==params[1] & pdf[,2]==params[2] & pdf[,3]==params[3],]
	

	for (i in 1:10){
		
		lt <- df[,30+i]
		rt <- df[,40+i]
		sym <- df[,50+i]
		
		if (log ==T){
			lt <- log(lt+1)
			rt <- log(rt+1)
			sym <- log(sym+1)
		}
		
		if (dif ==T){
			avg <- rowMeans(cbind(lt,rt))
			sym <- sym - avg
			lt <- lt - avg
			rt <- rt - avg
		}
		
		lines(x, lt, col=rgb(1,0,0, 0.2))
		lines(x, rt, col=rgb(0,0,1, 0.2))
		lines(x, sym, col=rgb(0,1,0, 0.2))
	}
}

#------------------------------------------------------------------------

# plot co num v delta 
plotCoN <- function(df, params, d = seq(0,1,0.1),log = F, dif = F){
	
	#x <- df[1:11,4]
	yLT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],11]
	yRT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],12]
	ysm <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],13]
	
	ylab <- "amounts of co periods"
	if (log == T){
		yLT <- log(yLT+1)
		yRT <- log(yRT+1)
		ysm <- log(ysm+1)
		ylab <- "amounts of co periods, ln(y+1)"
	}
	
	if (dif == T){
		rlavg <- rowMeans(cbind(yLT, yRT))
		ysm <- ysm - rlavg
		yLT <- yLT - rlavg
		yRT <- yRT - rlavg
	}
	
	y <- c(yLT, yRT, ysm)
	miny <- min(y)-abs(0.05*min(y))
	maxy <- max(y)+0.05*max(y)
	range <- c(miny,maxy)
	
	plot(0, ylim=range, xlim=range(d), type='n', xlab=expression(delta), ylab=ylab)
	lines(d, yLT, col='red', lty = 2, lwd=1.5)
	lines(d, yRT, col='blue', lty = 2, lwd=1.5)
	lines(d, ysm, col='green', lty = 2, lwd=1.5)
	mtext(paste("sigma=", params[1]," mu1=",params[2]," mu2=",params[3]), side=1, line=-1)
	legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
	
}

linesCoN <- function(df, params, pdf, log=F, dif=F){
	x <- pdf[1:11,4]
	df <- df[pdf[,1]==params[1] & pdf[,2]==params[2] & pdf[,3]==params[3],]
	

	for (i in 1:10){
		
		lt <- df[,60+i]
		rt <- df[,70+i]
		sym <- df[,80+i]
		
		if (log ==T){
			lt <- log(lt+1)
			rt <- log(rt+1)
			sym <- log(sym+1)
		}
		
		if (dif ==T){
			avg <- rowMeans(cbind(lt,rt))
			sym <- sym - avg
			lt <- lt - avg
			rt <- rt - avg
		}
		
		lines(x, lt, col=rgb(1,0,0, 0.2))
		lines(x, rt, col=rgb(0,0,1, 0.2))
		lines(x, sym, col=rgb(0,1,0, 0.2))
	}
}

#------------------------------------------------------------------------

# plot co frac v delta 
plotCoF <- function(df, params, d = seq(0,1,0.1),log = F, dif = F){
	
	#x <- df[1:11,4]
	yLT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],5]
	yRT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],6]
	ysm <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],7]
	
	ylab <- "fraction of co periods"
	if (log == T){
		yLT <- log(yLT+1)
		yRT <- log(yRT+1)
		ysm <- log(ysm+1)
		ylab <- "fraction of co periods, ln(y+1)"
	}
	
	if (dif == T){
		rlavg <- rowMeans(cbind(yLT, yRT))
		ysm <- ysm - rlavg
		yLT <- yLT - rlavg
		yRT <- yRT - rlavg
	}
	
	y <- c(yLT, yRT, ysm)
	miny <- min(y)-abs(0.05*min(y))
	maxy <- max(y)+0.05*max(y)
	range <- c(miny,maxy)
	
	plot(0, ylim=range, xlim=range(d), type='n', xlab=expression(delta), ylab=ylab)
	lines(d, yLT, col='red', lty = 2, lwd=1.5)
	lines(d, yRT, col='blue', lty = 2, lwd=1.5)
	lines(d, ysm, col='green', lty = 2, lwd=1.5)
	mtext(paste("sigma=", params[1]," mu1=",params[2]," mu2=",params[3]), side=1, line=-1)
	legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
	
}

linesCoF <- function(df, params, pdf, log=F, dif=F){
	x <- pdf[1:11,4]
	df <- df[pdf[,1]==params[1] & pdf[,2]==params[2] & pdf[,3]==params[3],]
	

	for (i in 1:10){
		
		lt <- df[,0+i]
		rt <- df[,10+i]
		sym <- df[,20+i]
		
		if (log ==T){
			lt <- log(lt+1)
			rt <- log(rt+1)
			sym <- log(sym+1)
		}
		
		if (dif ==T){
			avg <- rowMeans(cbind(lt,rt))
			sym <- sym - avg
			lt <- lt - avg
			rt <- rt - avg
		}
		
		lines(x, lt, col=rgb(1,0,0, 0.2))
		lines(x, rt, col=rgb(0,0,1, 0.2))
		lines(x, sym, col=rgb(0,1,0, 0.2))
	}
}


#------------------------------------------------------------------------

# plot dom mean v delta 
plotDoM <- function(df, params, d = seq(0,1,0.1),log = F, dif = F){
	
	#x <- df[1:11,4]
	yLT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],17]
	yRT <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],18]
	ysm <- df[df[,1]==params[1] & df[,2]==params[2] & df[,3]==params[3] & df[,4]>=d[1],19]
	
	ylab <- "Dom period length (mean)"
	if (log == T){
		yLT <- log(yLT+1)
		yRT <- log(yRT+1)
		ysm <- log(ysm+1)
		ylab <- "Dom period length (mean), ln(y+1)"
	}
	
	if (dif == T){
		rlavg <- rowMeans(cbind(yLT, yRT))
		ysm <- ysm - rlavg
		yLT <- yLT - rlavg
		yRT <- yRT - rlavg
	}
	
	y <- c(yLT, yRT, ysm)
	miny <- min(y, na.rm=T)-abs(0.05*min(y, na.rm=T))
	maxy <- max(y, na.rm=T)+0.05*max(y, na.rm=T)
	range <- c(miny,maxy)
	
	plot(0, ylim=range, xlim=range(d), type='n', xlab=expression(delta), ylab=ylab)
	lines(d, yLT, col='red', lty = 2, lwd=1.5)
	lines(d, yRT, col='blue', lty = 2, lwd=1.5)
	lines(d, ysm, col='green', lty = 2, lwd=1.5)
	mtext(paste("sigma=", params[1]," mu1=",params[2]," mu2=",params[3]), side=1, line=-1)
	legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)
	
}



