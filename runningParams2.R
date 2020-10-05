source("./generate_noise2.R")
source("./lottery_normcop.R")
source("./pop.sim_function.R")
#patching together param_sim.R and coexistence measure code from figure 8 (FIGURE.R; line 177)
#run code to:
	#generate noise for three types of TAs
	#calculate storage effect using Ellner method
	#simulate population time series
	#calculate coexistence metrics from those time series 
#should keep these goals/functions separate instead of combining into one function like in NoiseAndSE.R
#keeping separate for when we want to visualize the noise or population ts 

#structure same as param_sim.R? List of x elements, each element is repeated run that outputs matrix of unique parameters and calculation

#parameters
delta <- seq(0,1,0.1) #0 to 1 by 0.1
mu1 <- seq(.1,.9,.1) #0.1 to 0.9 by 0.1
mu2 <- seq(.1,.9,.1) #0.1 to 0.9 by 0.1
sigma <- c(.4,.8,1.6, 3.2,6.4) #doubling from 0.4 to 6.4
x <- 2 #number of run repeats

tot_sims <- length(sigma)*length(mu1)*length(mu2)*length(delta)
# for above paramters, there will be 4455 unique runs


#Structure for nest: sdev<mn1<mn2<delta

#organize parameters to follow nest structure from terminals inward and keep with length of tot_sims

delta_sims <- rep(delta, tot_sims/length(delta))
#repeat delta to fill tot_sims

mu2_sims <- sort(rep(mu2, length(delta)))
#repeat one mn2 value for every value of delta

mu1_sims <- sort(rep(mu1, length(mu2_sims)))
#repeat one mn1 value for every unique combination of mn2 and delta

sigma_sims <- sort(rep(sigma, length(mu1_sims)))
#repeat one sdev value for every unique combination of mn1, mn2, and delta 

#these vectors will be used to fill columns of dataframe tot_sims long
#vectors that did not reach tot_sims length will automatically repeat to reach that length


#for data storage: a list of dataframes, each dataframe is a run repeat
dflist <- vector(mode = 'list', length = x)

empty<-lapply(dflist, function(X){return(data.frame(sigma=sigma_sims, mu1=mu1_sims, mu2=mu2_sims, delta=delta_sims, se.ELT=NA, cf.ELT=NA, cm.ELT=NA, cn.ELT=NA, se.sym=NA, cf.sym=NA, cm.sym=NA, cn.sym=NA, se.ERT=NA, cf.ERT=NA, cm.ERT=NA, cn.ERT=NA))})
#data.frame with columns specifying the parameter values and columns to be filled with storage effect for ELT, sym, ERT

#GOAl: fill dflist with x number of simdf

#step 1: generate noise
#use parameters from simdf to feed to noise generating function
#use lapply to cut out section from data.frame
noise.params.only <- lapply(empty, function(X){return(X[,1:3])})[[1]]
NPO <- noise.params.only

#structure each unique param combo into list element and feed to noise function
longlist<-split(1:tot_sims, 1:tot_sims)
n <- 10000
Alist<- lapply(longlist, function(X){return(c(NPO$mu1[X], NPO$mu2[X], NPO$sigma[X], n))})

#now ready to start loop

for (i in 1:x){
	
	#make noise
	noise <- lapply(Alist, function(X){return(get_noise2(X))})
	#remove unnecessary element
	noise <- lapply(noise, function(X){return(X[-7])})
	#exponentiate noise
	xnoise <- lapply(noise, function(X){return(lapply(X, exp))})
	
	#add delta values as element in xnoise to make lapply simpler in next step
	for(X in 1:tot_sims){
		xnoise[[X]]$delta <- delta_sims[x]
	}
	
	#apply lottery model function to calculate storage effect for each noise type and store in data.frame
	se.list <- lapply(xnoise, function(X){return(data.frame(ELT=lottery_normcop(X$B_ELT, X$B_ELT_sharp, X$delta,1), sym=lottery_normcop(X$B_sym, X$B_sym_sharp, X$delta,1),ERT=lottery_normcop(X$B_ERT, X$B_ERT_sharp, X$delta,1)))})
	
	#fill empty data.frame with storage effect values
	empty[[i]]$se.ELT <- signif(unlist(lapply(se.list, function(X){return(X["Delta.Ib1","ELT"])})),3)
	empty[[i]]$se.sym <- signif(unlist(lapply(se.list, function(X){return(X["Delta.Ib1","sym"])})),3)
	empty[[i]]$se.ERT <- signif(unlist(lapply(se.list, function(X){return(X["Delta.Ib1","ERT"])})),3)
	
	#now take the noise to make simulation to measure coexistence
	#first remove sharps and delta from xnoise
	co.list <- lapply(xnoise, function(X){X[-c(2,4,6,7)]})
	
	#separate noise types to average
	co_ELT <- lapply(co.list, function(X){return(X$B_ELT)})
	co_sym <- lapply(co.list, function(X){return(X$B_sym)})
	co_ERT <- lapply(co.list, function(X){return(X$B_ERT)})
	
	
	

}









lottery_normcop(exp(noise[[1]]$B_ELT), exp(noise[[1]]$B_ELT_sharp), 0, 1)












