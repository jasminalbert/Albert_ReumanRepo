#need histogram of abundance as fraction of N
#weak coexistence = extreme bimodal
# 1 - (fraction of time where species is dominant)
# 0.1% threshold

#generate 1000 sets of 3 noise runs simulations
#function computes measure of coexistence
#store and repeat

#feed time series to one-way ANOVA to check difference

source("./generate_noise.R")
source("./lottery_normcop.R")
source("./pop.sim_function.R")

time <- 10000

d <- 1
sigma <- 1.6
mu1 <- 0.1
mu2 <- 0.1

set.seed(1223)
noise <-get_noise(mn=c(mu1,mu2),sdev=c(sigma,sigma),n=time,check = F)

resL<- lottery_normcop(exp(noise$B_ELT), exp(noise$B_ELT_sharp), d, 1)
resS<- lottery_normcop(exp(noise$B_sym), exp(noise$B_sym_sharp), d, 1)
resR<- lottery_normcop(exp(noise$B_ERT), exp(noise$B_ERT_sharp), d, 1)

round(resL,6)
round(resS,6)
round(resR,6)

#popsim
popELT <- pop.sim(b=noise$B_ELT, N=50, N1=25, d=1, time=10000)
popSYM <- pop.sim(b=noise$B_sym, N=50, N1=25, d=1, time=10000)
popERT <- pop.sim(b=noise$B_ERT, N=50, N1=25, d=1, time=10000)

#plot N1
plot(popELT[,1], type='l')
plot(popSYM[,1], type='l')
plot(popERT[,1], type='l')

#quantify dominance time 
nrow(popELT[popELT[,1] > 0.999*N,])
sum(popELT[,1] > 0.999*N)
popELT[popELT[,1] > 0.999*N,]
#TRUE = dominating
X <- popELT[,1] > 0.999*N 
runs <- rle(X)
dom_periods <- runs$lengths[runs$values == TRUE]


popELT[coexist]

#dominance period fucntion
dom.t <- function(series, N, threshold,...) {
	dominate <- series[,1] > threshold*N
	RLE <- rle(dominate)
	dom_periods <- RLE$lengths[RLE$values == TRUE]
	hist(dom_periods, main="histogram of dominance period length",...)
	return(dom_periods)	
	
}

#coexistence period fucntion
co.t <- function(series, N, hi, lo,...) {
	coexist <- series[,1] < hi*N & series[,1] > lo*N	
	RLE <- rle(coexist)
	co_periods <- RLE$lengths[RLE$values == TRUE]
	hist(co_periods, main="histogram of coexistence period length",...)
	return(co_periods)	
	
}

domELT<-dom.t(popELT, N=50, threshold=0.999, breaks = 25)
sum(domELT)/time
coELT<-co.t(popELT, 50, 0.999, 0.001)
sum(coELT)/time

domSYM<-dom.t(popSYM, N=50, threshold=0.999, breaks = 25)
sum(domSYM)/time
coSYM<-co.t(popSYM, 50, 0.999, 0.001)
sum(coSYM)/time

domERT<-dom.t(popERT, N=50, threshold=0.999, breaks = 25)
sum(domERT)/time
coERT<-co.t(popERT, 50, 0.999, 0.001)
sum(coERT)/time


#abundance as fraction of N
hist(popELT[,1]/N)
hist(popSYM[,1]/N)
hist(popERT[,1]/N)







