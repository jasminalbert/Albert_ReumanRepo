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

n <- 10^6


#make parameter data.frame to pull from
tot_sims <- length(sigma)*length(mu1)*length(mu2)*length(delta)

delta_sims <- rep(delta, tot_sims/length(delta))
mu2_sims <- sort(rep(mu2, length(delta)))
mu1_sims <- sort(rep(mu1, length(mu2_sims)))
sigma_sims <- sort(rep(sigma, length(mu1_sims)))

params <- data.frame(sigma=sigma_sims, mu1=mu1_sims, mu2=mu2_sims, delta=delta_sims)



#initialize storage
#big.list <- vector(mode='list', length=tot_sims)


#initailize storage

SEres <- matrix(0, nrow=tot_sims, ncol=120)
CoMres <- matrix(0, nrow=tot_sims, ncol=90)


#rep loop
set.seed(2727)
for(rep in 0:(repeats-1)){
	#param loop
	for (i in 1:tot_sims){
		#generate noise
		noise_b <- getnoise2(mu = c(params$mu1[i], params$mu2[i]), sigma = c(params$sigma[i],params$sigma[i]), n=n, corval=corval, corRT=corRT)
		noise_b_sharp <- getnoise2(mu = c(params$mu1[i], params$mu2[i]), c(params$sigma[i],params$sigma[i]), n=n, corval=corval, corRT=corRT)
	
		#exponentiate noise
		noise_B <- exp(noise_b)
		noise_B_sharp <- exp(noise_b_sharp)
	
		#compute SE
		SE <- SE_lottery(noise_B, noise_B_sharp, params$delta[i])
		SEvec <- c(SE, recursive=T)
	
		#population simulation
		pop <- popsim(noise_B, N=50, N1=25, params$delta[i], n=1000)
	
		#coexistence periods
		co.p <- co.periods(pop, dom=0.95, N=50)
	
		#coexistence metrics
		metrics <- measure.co(co.p, n)
		metvec <- c(metrics, recursive=T)
		
		#--------------store
		SEres[i, (1:12)+rep*12] <- SEvec
		CoMres[i, (1:9)+rep*9] <- metvec	
		
		if (i%%400 == 0) {
			print(i) #for keeping track of run 
		}
		
	}
}

noise4<-rep(rownames(SE), 4)
SEmetpernoise<-rep(colnames(SE),3)
SEmet<-factor(SEmetpernoise, levels=colnames(SE))
colnames(SEres) <- rep(paste(sort(SEmet), noise4, sep=""),10)
SEres <- SEres[,order(colnames(SEres))]

noise3 <- rep(rownames(metrics),3)
Cometpernoise<-rep(colnames(metrics),3)
Comet<-factor(Cometpernoise, levels=colnames(metrics))
colnames(CoMres) <- rep(paste(sort(Comet), noise3, sep=""),10)
CoMres <- CoMres[,order(colnames(CoMres))]



#find averages of reps 
SEdf <- data.frame(SEres)
cn_se <- unique(colnames(SEres)) #colnames SEres
SEmeandf <- data.frame(matrix(0, nrow=tot_sims, ncol=12))
colnames(SEmeandf) <- cn_se

for(i in 1:12){
	SEmeandf[,i]<-rowMeans(SEdf[,colnames(SEres)==cn_se[i]])
}


CoMdf <- data.frame(CoMres)
cn_co <- unique(colnames(CoMres))
CoMmeandf <- data.frame(matrix(0, nrow=tot_sims, ncol=9))
colnames(CoMmeandf) <- cn_co

for(i in 1:9){
	CoMmeandf[,i] <- rowMeans(CoMdf[,colnames(CoMres)==cn_co[i]])
}

#combine in big df 
res <- cbind(params, SEmeandf, CoMmeandf)

write.csv(SEres, "SE_results.csv")
write.csv(CoMres, "CoMetrics_results.csv")
write.csv(res, "SEandCoMresults202101.csv")







