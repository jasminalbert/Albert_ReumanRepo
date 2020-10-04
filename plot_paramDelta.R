#function for plotting over delta with set params as args

#ARGS
#sdev
#mn1
#mn2
#list		list of data frames containing duplicate 			simulations
#mean_df	data frame of mean of duplicate sims
#range		range of y axis (vector length 2)



plot_paramDelta <- function(sdev, mn1, mn2, list, mean_df, range, xlim=c(0,1),...){
	
	plot_list <- vector(mode = 'list', length = length(list))
	
	for(i in 1:length(list)) {
		plot_list[[i]] <- list[[i]][list[[i]]$sdev == sdev & list[[i]]$mn1 == mn1 & list[[i]]$mn2 == mn2,]
	}
	
	plot(0, ylim = range, xlim = xlim, type = 'n', xlab = expression(delta),...)
	
		
	#legend("topleft", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0, cex = 1.25)
	
	for (i in 1:10) {
		lines(plot_list[[i]]$delta, plot_list[[i]]$ELT, col = rgb(1,0,0, 0.2))
		lines(plot_list[[i]]$delta, plot_list[[i]]$sym, col = rgb(0,1,0, 0.2))
		lines(plot_list[[i]]$delta, plot_list[[i]]$ERT, col = rgb(0,0,1, 0.2))
	
	}
	
lines(seq(0,1.0,.1), mean_df[mean_df$sdev == sdev & mean_df$mn1 == mn1 & mean_df$mn2 == mn2,]$ELT, col = 'red', lty = 2, lwd=1.5)
lines(seq(0,1.0,.1), mean_df[mean_df$sdev == sdev & mean_df$mn1 == mn1 & mean_df$mn2 == mn2,]$sym, col = 'green', lty = 2, lwd=1.5)
lines(seq(0,1.0,.1), mean_df[mean_df$sdev == sdev & mean_df$mn1 == mn1 & mean_df$mn2 == mn2,]$ERT, col = 'blue', lty = 2, lwd=1.5)

return(plot_list)
}


#use function to make multiple plots ranging over mn2
#for( i in seq(0.1,0.9,.1)){
	#plot_paramDelta(sdev=6.4, mn1=.5, mn2=i, list=dflist, mean_df=mean_df, range = c(-0.009,.04))
#}

#plot_paramDelta(sdev=0.4, mn1=.5, mn2=0.5, list=dflist, mean_df=mean_df, range = c(-0.007,0.0158))

#make the function only plot differences between types of noise and mean of symmetric 

plot_paramDelta2 <- function(sdev, mn1, mn2, list, mean_df, range,...){
	
	plot_list <- vector(mode = 'list', length = length(list))
	
	for(i in 1:length(list)) {
		plot_list[[i]] <- list[[i]][list[[i]]$sdev == sdev & list[[i]]$mn1 == mn1 & list[[i]]$mn2 == mn2,]
	}
	
	plot_list2 <- lapply(plot_list, function(x){print(x[,c("ELT","sym", "ERT")])})
	
	plot_list3 <- lapply(plot_list2, function(x) {print(apply(x, 2, function(y){y-mean_df[mean_df$sdev == sdev & mean_df$mn1 == mn1 & mean_df$mn2 == mn2,]$sym}))})
	
	plot(0, ylim = range, xlim = c(0, 1), type = 'n', ylab = "SE - mean(SE_sym)", xlab = expression(paste(delta)), main = expression(paste("SE - mean(SE_sym) vs. ",delta)),...)
	legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0)

	
	for (i in 1:10) {
		lines(plot_list[[i]]$delta, plot_list3[[i]][,"ELT"], col = rgb(1,0,0, 0.2))
		lines(plot_list[[i]]$delta, plot_list3[[i]][,"sym"], col = rgb(0,1,0, 0.2))
		lines(plot_list[[i]]$delta, plot_list3[[i]][,"ERT"], col = rgb(0,0,1, 0.2))
	
	}
	
	ELT <- matrix(NA, ncol = length(plot_list3), nrow = nrow(plot_list3[[1]]))
	ERT <- matrix(NA, ncol = length(plot_list3), nrow = nrow(plot_list3[[1]]))
	sym <- matrix(NA, ncol = length(plot_list3), nrow = nrow(plot_list3[[1]]))
	
	for (i in 1:length(plot_list3)){
	
	ELT[,i] <- plot_list3[[i]][,"ELT"]
	sym[,i] <- plot_list3[[i]][,"sym"]	
	ERT[,i] <- plot_list3[[i]][,"ERT"]	

}
	
	ELT_mean <- apply(ELT, MARGIN =1, mean)
	ERT_mean <- apply(ERT, MARGIN =1, mean)
	sym_mean <- apply(sym, MARGIN =1, mean)

	
lines(seq(0,1.0,.1), ELT_mean, col = 'red', lty = 2)
lines(seq(0,1.0,.1), sym_mean, col = 'green', lty = 2)
lines(seq(0,1.0,.1), ERT_mean, col = 'blue', lty = 2)

return(cbind(ELT_mean, sym_mean, ERT_mean))
}




#for( i in seq(0.1,0.9,.1)){
	#plot_paramDelta2(sdev=0.4, mn1=0.1, mn2=i, list=dflist, mean_df=mean_df, range = c(-0.001,.0012))
#}

#plot_paramDelta2(sdev=0.4, mn1=0.5, mn2=0.5, list=dflist, mean_df=mean_df, range = c(-0.00155,.00155))




