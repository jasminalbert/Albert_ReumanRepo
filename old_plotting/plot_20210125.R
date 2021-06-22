source("./plotting_fxns.R")
# for meeting 3 of 2021 
# plotting SE and coexistence metrics against delta 
# start delta at 0.1
# put coexistence metrics of log(x+1) scale 
# plot normal and sym minus avg(LT, RT)


# load data
# new sim with 10^6 timesteps
# df with all 10 reps and mean df

# SE:
res1<-read.csv("generated_data/run3/SEandCoMresults202101.csv")[,-1] #includes rate of change too
resSE <- res1[,1:7]
res1all <- read.csv("generated_data/run3/SE_results.csv")[,-1]
rseall <- res1all[,1:30] #only se (Delta I)
# comets:
res2<-read.csv("generated_data/run3/CoMetrics_results2.csv")[,-1]
res2m<-read.csv("generated_data/run3/CoMetrics_mean2.csv")[,-1]

# param df:
delta <- seq(0,1,0.1)
mu1 <- seq(0.1,0.9,0.1)
mu2 <- mu1
sigma <- c(0.4, 0.8, 1.6, 3.2, 6.4)
tot_sims <- length(sigma)*length(mu1)*length(mu2)*length(delta)

delta_sims <- rep(delta, tot_sims/length(delta))
mu2_sims <- sort(rep(mu2, length(delta)))
mu1_sims <- sort(rep(mu1, length(mu2_sims)))
sigma_sims <- sort(rep(sigma, length(mu1_sims)))

paramsdf1 <- data.frame(sigma=sigma_sims, mu1=mu1_sims, mu2=mu2_sims, delta=delta_sims)

# less mus
mu1 <- c(0.1,0.5,0.9)
mu2 <- mu1
tot_sims <- length(sigma)*length(mu1)*length(mu2)*length(delta)

delta_sims <- rep(delta, tot_sims/length(delta))
mu2_sims <- sort(rep(mu2, length(delta)))
mu1_sims <- sort(rep(mu1, length(mu2_sims)))
sigma_sims <- sort(rep(sigma, length(mu1_sims)))

paramsdf2 <- data.frame(sigma=sigma_sims, mu1=mu1_sims, mu2=mu2_sims, delta=delta_sims)

plotparam <- unique(paramsdf2[,-4])
plotsims<-dim(plotparam)[1]

# SE plots

pdf("run4_plotcheck_dif.pdf")
for(i in 1:plotsims){
	params <- as.numeric(plotparam[i,])
	plotDeltaI(res1, params=params, d=seq(0.1,1,0.1), dif=T)
	lines1DeltaI(rseall, params=params, pdf=paramsdf1, dif=T)
}
dev.off()


# co.mean plots 

pdf("run4_comean_plot_log_dif.pdf")
for (i in 1:plotsims){
	params <- as.numeric(plotparam[i,])
	plotCoM(df=res2m, params=params, d=seq(0.1,1,0.1), log=T, dif=T)
	linesCoM(df=res2, params=params, pdf=paramsdf2, log=T, dif=T)
}
dev.off()

# co.num plots 

pdf("run4_conum_plot_log_dif.pdf")
for (i in 1:plotsims){
	params <- as.numeric(plotparam[i,])
	plotCoN(df=res2m, params=params, d=seq(0.1,1,0.1), log=T, dif=T)
	linesCoN(df=res2, params=params, pdf=paramsdf2, log=T, dif=T)
}
dev.off()

# co.frac plots 

pdf("run4_cofrac_plot_dif.pdf")
for (i in 1:plotsims){
	params <- as.numeric(plotparam[i,])
	plotCoF(df=res2m, params=params, d=seq(0.1,1,0.1), log=F, dif=T)
	linesCoF(df=res2, params=params, pdf=paramsdf2, log=F, dif=T)
}
dev.off()

#dom.mean

dmsim <- unique(paramdf[index5,-4])
pdf("run4_dommean_plot_log_dif.pdf")
for (i in 2:30){
	params <- as.numeric(dmsim[i,])
	plotDoM(df=res4, params=params, d=c(0.2,0.5,0.8,1.0), log=T, dif=T)
}
dev.off()

res4[res4$sigma==3.2,17:19]






