source("./getnoise.R")
source("./popsim2.R")
source("./co.periods.R")
source("./measure.co3.R")

#parameters for noise
delta <- seq(0,1,0.1)
mu1 <- c(0.1,0.5,0.9)
mu2 <- mu1
sigma <- c(0.4, 0.8, 1.6, 3.2, 6.4)

corval <- 0.88
corRT <- getcorpar(0,.5,corval,numpts=100000,allowneg=FALSE,ploton=FALSE)

repeats <- 10
N <- 50
N1 <- 25
n <- 10^6

tot_sims <- length(sigma)*length(mu1)*length(mu2)*length(delta)

delta_sims <- rep(delta, tot_sims/length(delta))
mu2_sims <- sort(rep(mu2, length(delta)))
mu1_sims <- sort(rep(mu1, length(mu2_sims)))
sigma_sims <- sort(rep(sigma, length(mu1_sims)))

params <- data.frame(sigma=sigma_sims, mu1=mu1_sims, mu2=mu2_sims, delta=delta_sims)

poplist <- vector(mode='list', length=tot_sims)
popreps <- vector(mode='list', length=10)
CoMres <- matrix(0, nrow=tot_sims, ncol=90)
Sys.time()
#rep loop
set.seed(2727)
for(rep in 0:(repeats-1)){
	#param loop
	for (i in 1:tot_sims){
		#generate noise
		noise_b <- getnoise2(mu = c(params$mu1[i], params$mu2[i]), sigma = c(params$sigma[i],params$sigma[i]), n=n, corval=corval, corRT=corRT)
		
		#exponentiate noise
		noise_B <- exp(noise_b)
			
		#population simulation
		popLT <- popsim2(B=cbind(noise_B$LT1, noise_B$LT2), N=N, N1=N1, delta = params$delta[i], n=n)
		popsym <- popsim2(B=cbind(noise_B$sym1, noise_B$sym2), N=N, N1=N1, delta = params$delta[i], n=n)
		popRT <-popsim2(B=cbind(noise_B$RT1, noise_B$RT2), N=N, N1=N1, delta = params$delta[i], n=n)
		
		pop <- cbind(popLT, popsym, popRT)
	
		#coexistence periods
		co.p <- co.periods(pop, dom=0.95, N=50)
	
		#coexistence metrics
		metrics <- measure.co(co.p, n)
		metvec <- c(metrics, recursive=T)
		
		#--------------store
		CoMres[i, (1:9)+rep*9] <- metvec	
		
		if (i%%40 == 0) {
			print(i) #for keeping track of run 
		}
	#popreps[[rep+1]] <- poplist
	}
}
Sys.time()


noise3 <- rep(rownames(metrics),3)
Cometpernoise<-rep(colnames(metrics),3)
Comet<-factor(Cometpernoise, levels=colnames(metrics))
colnames(CoMres) <- rep(paste(sort(Comet), noise3, sep=""),10)
CoMres <- CoMres[,order(colnames(CoMres))]

CoMdf <- data.frame(CoMres)
cn_co <- unique(colnames(CoMres))
CoMmeandf <- data.frame(matrix(0, nrow=tot_sims, ncol=9))
colnames(CoMmeandf) <- cn_co

for(i in 1:9){
	CoMmeandf[,i] <- rowMeans(CoMdf[,colnames(CoMres)==cn_co[i]])
}

res <- cbind(params, CoMmeandf)

write.csv(res, "CoMetrics_mean2.csv")
write.csv(CoMres, "CoMetrics_results2.csv")

#save(popreps, file="popreps.RData")
#load("popreps.RData")
