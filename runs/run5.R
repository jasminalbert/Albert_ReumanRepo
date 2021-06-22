# run5
# add dominance metrics and store population simulations
# shortened mean

source("./getnoise.R")
source("./popsim2.R")
source("./co.periods.R")
source("./measure.co3.R")

#parameters for noise
delta <- seq(0,1,0.1)
mu1 <- c(0.1,0.5,0.9)
mu2 <- mu1
sigma <- c(0.4, 0.8, 1.6, 3.2, 6.4)

# make parameter df
tot_sims <- length(sigma)*length(mu1)*length(mu2)*length(delta)

delta_sims <- rep(delta, tot_sims/length(delta))
mu2_sims <- sort(rep(mu2, length(delta)))
mu1_sims <- sort(rep(mu1, length(mu2_sims)))
sigma_sims <- sort(rep(sigma, length(mu1_sims)))

params <- data.frame(sigma=sigma_sims, mu1=mu1_sims, mu2=mu2_sims, delta=delta_sims)

# correlation
corval <- 0.88
corRT <- getcorpar(0,.5,corval,numpts=100000,allowneg=FALSE,ploton=FALSE)

# other params
repeats <- 10
n <- 10^6
N <- 50
N1 <- 25

sim_mat <- matrix(0, nrow = n, ncol = (tot_sims/length(sigma)*3))
index1 <- sort(rep(1:(tot_sims/length(sigma)),3))
index2 <- 1:length(index1)
index.v <- rep(1:(tot_sims/length(sigma)*3),5)
index.m <- matrix(index.v, ncol=3, byrow=TRUE)


set.seed(2727)

for(rep in 0:(repeats-1)){
	
	for (i in 1:tot_sims){
		
		#noise - generate
		noise_b <- getnoise2(mu=c(params$mu1[i], params$mu2[i]), sigma = c(params$sigma[i], params$sigma[i]), n=n, corval=corval, corRT=corRT)
		
		# - exponentiate
		noise_B <- exp(noise_b)
		
		
		#population simulation
		popLT <- popsim2(B=cbind(noise_B$LT1, noise_B$LT2), N=N, N1=N1, delta=params$delta[i], n=n)
		popRT <- popsim2(B=cbind(noise_B$RT1, noise_B$RT2), N=N, N1=N1, delta=params$delta[i], n=n) 
		popsym <- popsims2(B=cbind(noise_B$sym1, noise_B$sym2), N=N, N1=N1, delta=params$delta[i], n=n)
		
		sim_mat[,index.m[i,1]] <- popLT$N1
		sim_mat[,index.m[i,2]] <- popRT$N1
		sim_mat[,index.m[i,3]] <- popsym$N1
		
		if (i%%99 == 0){
			write.csv(sim_mat, file = paste((i/99)*10+rep,"sim_mat.csv", sep=""))
			sim_mat <- matrix(0, nrow = n, ncol = (tot_sims/length(sigma)*3))

		}
	}
}





















