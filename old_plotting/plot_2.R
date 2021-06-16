dflist[[1]][dflist[[1]]$sdev == 0.4 & dflist[[1]]$mn1 ==0.6 & dflist[[1]]$delta == 1.0,]

#range mn2 while mn1 = 0.6, sdev = 0.4, delta = 1.0

plot1_list <- vector(mode = 'list', length =10)
for (i in 1:10) {
	plot1_list[[i]] <- dflist[[i]][dflist[[i]]$sdev == 0.4 & dflist[[i]]$mn1 ==0.6 & dflist[[i]]$delta == 1.0,]
}


plot(0, ylim = c(-6e-04,4e-04), xlim = c(0, 1), type = 'n', ylab = "SE", xlab = "mn2", main = "Delta I according to changes in mn2", sub = "sdev = 0.4, mn1=0.6, delta=1")
legend("topleft", legend = c("extreme left tail", "symmetric", "extreme right tail"), col = c("red", "green", "blue"), box.lty =0)

for (i in 1:10) {
	lines(plot1_list[[i]]$mn2, plot1_list[[i]]$ELT, col = rgb(1,0,0, 0.2))
	lines(plot1_list[[i]]$mn2, plot1_list[[i]]$sym, col = rgb(0,1,0, 0.2))
	lines(plot1_list[[i]]$mn2, plot1_list[[i]]$ERT, col = rgb(0,0,1, 0.2))
	
}

lines(seq(.1,.9,.1), mean_df[mean_df$sdev == 0.4 & mean_df$mn1 == 0.6 & mean_df$delta == 1,]$ELT, col = 'red', lty = 2)
lines(seq(.1,.9,.1), mean_df[mean_df$sdev == 0.4 & mean_df$mn1 == 0.6 & mean_df$delta == 1,]$sym, col = 'green', lty = 2)
lines(seq(.1,.9,.1), mean_df[mean_df$sdev == 0.4 & mean_df$mn1 == 0.6 & mean_df$delta == 1,]$ERT, col = 'blue', lty = 2)

#range delta while mn1= 0.6, mn2= 0.1, sdev= 0.4
mn2 <- 0.9

plot2_list <- vector(mode = 'list', length =10)
for (i in 1:10) {
	plot2_list[[i]] <- dflist[[i]][dflist[[i]]$sdev == 0.4 & dflist[[i]]$mn1 ==0.6 & dflist[[i]]$mn2 == mn2,]
}


plot(0, ylim = c(-1e-03,0.0095), xlim = c(0, 1), type = 'n', ylab = "SE", xlab = "delta", main = "Delta I according to changes in delta", sub = paste("sdev = 0.4, mn1=0.6, mn2=", mn2))
legend("topleft", legend = c("extreme left tail", "symmetric", "extreme right tail"), col = c("red", "green", "blue"), box.lty =0)

for (i in 1:10) {
	lines(plot2_list[[i]]$delta, plot2_list[[i]]$ELT, col = rgb(1,0,0, 0.2))
	lines(plot2_list[[i]]$delta, plot2_list[[i]]$sym, col = rgb(0,1,0, 0.2))
	lines(plot2_list[[i]]$delta, plot2_list[[i]]$ERT, col = rgb(0,0,1, 0.2))
	
}

lines(seq(0,1.0,.1), mean_df[mean_df$sdev == 0.4 & mean_df$mn1 == 0.6 & mean_df$mn2 == mn2,]$ELT, col = 'red', lty = 2)
lines(seq(0,1.0,.1), mean_df[mean_df$sdev == 0.4 & mean_df$mn1 == 0.6 & mean_df$mn2 == mn2,]$sym, col = 'green', lty = 2)
lines(seq(0,1.0,.1), mean_df[mean_df$sdev == 0.4 & mean_df$mn1 == 0.6 & mean_df$mn2 == mn2,]$ERT, col = 'blue', lty = 2)
#------------------------------------------------------
plot_paramDelta <- function(sdev, mn1, mn2, list, mean_df, range){
	
	plot_list <- vector(mode = 'list', length = length(list))
	
	for(i in 1:length(list)) {
		plot_list[[i]] <- list[[i]][list[[i]]$sdev == sdev & list[[i]]$mn1 == mn1 & list[[i]]$mn2 == mn2,]
	}
	
	plot(0, ylim = range, xlim = c(0, 1), type = 'n', ylab = "SE", xlab = "delta", main = "Delta I according to changes in delta", sub = paste("sdev =",sdev," mn1=",mn1, "mn2=", mn2))
	legend("topleft", legend = c("extreme left tail", "symmetric", "extreme right tail"), col = c("red", "green", "blue"), box.lty =0)
	
	for (i in 1:10) {
		lines(plot_list[[i]]$delta, plot_list[[i]]$ELT, col = rgb(1,0,0, 0.2))
		lines(plot_list[[i]]$delta, plot_list[[i]]$sym, col = rgb(0,1,0, 0.2))
		lines(plot_list[[i]]$delta, plot_list[[i]]$ERT, col = rgb(0,0,1, 0.2))
	
	}
	
lines(seq(0,1.0,.1), mean_df[mean_df$sdev == sdev & mean_df$mn1 == mn1 & mean_df$mn2 == mn2,]$ELT, col = 'red', lty = 2)
lines(seq(0,1.0,.1), mean_df[mean_df$sdev == sdev & mean_df$mn1 == mn1 & mean_df$mn2 == mn2,]$sym, col = 'green', lty = 2)
lines(seq(0,1.0,.1), mean_df[mean_df$sdev == sdev & mean_df$mn1 == mn1 & mean_df$mn2 == mn2,]$ERT, col = 'blue', lty = 2)

return(plot_list)
}

for( i in seq(0.1,0.9,.1)){
	plot_paramDelta(sdev=0.8, mn1=.5, mn2=i, list=dflist, mean_df=mean_df, range = c(-0.009,.04))
}





