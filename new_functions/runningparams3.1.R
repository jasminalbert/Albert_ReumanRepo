#running params, third times a charm
source("./getnoise.R")
source("./SE_lottery.R")
source("./popsim.R")
source("./co.periods.R")
source("./measure.co3.R")

#parameters for noise
delta <- seq(0,1,0.1)
mu1 <- seq(0.1,0.9,0.1)
mu2 <- mu1
sigma <- c(0.4, 0.8, 1.6, 3.2, 6.4)

corval <- 0.88
corRT <- getcorpar(0,.5,corval,numpts=100000,allowneg=FALSE,ploton=FALSE)

repeats <- 10

n <- 100


#make parameter data.frame to pull from
tot_sims <- length(sigma)*length(mu1)*length(mu2)*length(delta)

delta_sims <- rep(delta, tot_sims/length(delta))
mu2_sims <- sort(rep(mu2, length(delta)))
mu1_sims <- sort(rep(mu1, length(mu2_sims)))
sigma_sims <- sort(rep(sigma, length(mu1_sims)))

params <- data.frame(sigma=sigma_sims, mu1=mu1_sims, mu2=mu2_sims, delta=delta_sims)



#initialize storage
big.list <- vector(mode='list', length=100)

noise_b <- big.list
noise_b_sharp <- big.list
noise_B <- big.list
noise_B_sharp <- big.list
SE_list <- big.list
pop <- big.list
co.p <- big.list
metrics <- big.list

#start loop
for (i in 1:100){
	
	#generate noise
	noise_b[[i]] <- getnoise2(mu = c(params$mu1[i], params$mu2[i]), sigma = c(params$sigma[i],params$sigma[i]), n=n, corval=corval, corRT=corRT)
	noise_b_sharp[[i]] <- getnoise2(mu = c(params$mu1[i], params$mu2[i]), c(params$sigma[i],params$sigma[i]), n=n, corval=corval, corRT=corRT)
	
	#exponentiate noise
	noise_B[[i]] <- exp(noise_b[[i]])
	noise_B_sharp[[i]] <- exp(noise_b_sharp[[i]])
	
	#compute SE
	SE_list[[i]] <- SE_lottery(noise_B[[i]], noise_B_sharp[[i]], params$delta[i])
	
	#population simulation
	pop[[i]] <- popsim(noise_B[[i]], N=50, N1=25, params$delta[i])
	
	#coexistence periods
	co.p[[i]] <- co.periods(pop[[i]], dom=0.95, N=50)
	
	#coexistence metrics
	metrics[[i]] <- measure.co(co.p[[i]], T)
	
}

#store
deltaB_list <- lapply(SE_list, function(X){X[,-2:-4]}) 
rbar1_list <- lapply(SE_list, function(X){X[,1:2]})
rbar1_sharp_list <- lapply(SE_list, function(X){X[,c(1,3)]})
#write same for metrics

#rep loop
for(reps in 2:repeats){
	#param loop
	for (i in 1:100){
		#generate noise
		noise_b[[i]] <- getnoise2(mu = c(params$mu1[i], params$mu2[i]), sigma = c(params$sigma[i],params$sigma[i]), n=n, corval=corval, corRT=corRT)
		noise_b_sharp[[i]] <- getnoise2(mu = c(params$mu1[i], params$mu2[i]), c(params$sigma[i],params$sigma[i]), n=n, corval=corval, corRT=corRT)
	
		#exponentiate noise
		noise_B[[i]] <- exp(noise_b[[i]])
		noise_B_sharp[[i]] <- exp(noise_b_sharp[[i]])
	
		#compute SE
		SE_list[[i]] <- SE_lottery(noise_B[[i]], noise_B_sharp[[i]], params$delta[i])
	
		#population simulation
		pop[[i]] <- popsim(noise_B[[i]], N=50, N1=25, params$delta[i])
	
		#coexistence periods
		co.p[[i]] <- co.periods(pop[[i]], dom=0.95, N=50)
	
		#coexistence metrics
		metrics[[i]] <- measure.co(co.p[[i]], T)
		
		#--------------store
		deltaB_list[[i]][,rep+1] <- SE_list[[i]]$Delta_B1
		rabr1_list[[i]][,rep+1] <- SE_list[[i]]$rbar1
		rabr1_sharp_list[[i]][,rep+1] <- SE_list[[i]]$rbar1_sharp

	}
}