source("./plot_paramDelta.R")

params: vector of parameters for the plot c(sigma, mu1,mu2)

plot_SE_Delta <- function(params, df){
	delta <- seq(0, 1.0, 0.1)
	plot(delta, df[df[,1] == params[1] & df[,2] == params[2] & df[,3] == params[3],6], col='green', type='l')
	lines(delta, df[df[,1] == params[1] & df[,2] == params[2] & df[,3] == params[3],5], col='red')
	lines(delta, df[df[,1] == params[1] & df[,2] == params[2] & df[,3] == params[3],7], col='blue')
}

params <- c(6.4, 0.1, 0.9)
mean_df<- signif(mean_df,4)

plot_SE_Delta(params, mean.df2)
plot_SE_Delta(params, mean_df)


params_mu2 <- rep(c(0.1,0.5,0.9), 3*5)
params_df <- data.frame(sigma=NA, mu1=NA, mu2=params_mu2)
params_df$mu1 <- rep(sort(rep(c(0.1,0.5,0.9), 3)),5)
params_df$sigma <- sort(rep(c(0.4,0.8,1.6,3.2,6.4),9))

pdf('check_graphs.pdf')

for(i in 1:nrow(params_df)){
	plot_SE_Delta(as.numeric(params_df[i,]), mean.df2)
	title(paste(params_df[i,]))
	plot_SE_Delta(as.numeric(params_df[i,]), mean_df)
	title(paste(params_df[i,]))
}

dev.off()

mean.df2[mean.df2$sigma==params[1] & mean.df2$mu1==params[2] & mean.df2$mu2==params[3],1:7]

mean_df[mean_df$sdev==params[1] & mean_df$mn1==params[2] & mean_df$mn2==params[3],1:7]






