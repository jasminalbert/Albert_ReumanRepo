source("./NoiseAndSE.R")
#This is code for running through the storage effect calculations for various sets of parameters as set below a number of x times
#Parameters include delta (death rate), mn1 and mn2 (mean birth rate of sp1 and sp2) and sdev (standard deviation of birth rate; equal for both species)


delta <- seq(0,1,0.1) #0 to 1 by 0.1
mn1 <- seq(.1,.9,.1) #0.1 to 0.9 by 0.1
mn2 <- seq(.1,.9,.1) #0.1 to 0.9 by 0.1
sdev <- c(.4,.8,1.6, 3.2,6.4) #doubling from 0.4 to 6.4
x <- 10 #number of run repeats
n <- 10^6

#noise_SE(mn1, mn2, sdev, n, delta)
# ^the function we will use for this run; a function that generates noise and then calculates SE


tot_sims <- length(sdev)*length(mn1)*length(mn2)*length(delta)
# for above paramters, there will be 4455 unique runs


#Structure for nest: sdev<mn1<mn2<delta

#organize parameters to follow nest structure from terminals inward and keep with length of tot_sims

delta_sims <- rep(delta, tot_sims/length(delta))
#repeat delta to fill tot_sims

mn2_sims <- sort(rep(mn2, length(delta)))
#repeat one mn2 value for every value of delta

mn1_sims <- sort(rep(mn1, length(mn2_sims)))
#repeat one mn1 value for every unique combination of mn2 and delta

sdev_sims <- sort(rep(sdev, length(mn1_sims)))
#repeat one sdev value for every unique combination of mn1, mn2, and delta 

#these vectors will be used to fill columns of dataframe tot_sims long
#vectors that did not reach tot_sims length will automatically repeat to reach that length


#for data storage: a list of dataframes, each dataframe is a run repeat

dflist <- vector(mode = 'list', length = x)

simdfx <- data.frame(sdev=sdev_sims, mn1=mn1_sims, mn2=mn2_sims, delta=delta_sims, ELT=NA, sym=NA, ERT=NA)
#data.frame with columns specifying the parameter values and columns to be filled with storage effect for ELT, sym, ERT

#fill dflist with x number of simdfx

for (m in 1:length(dflist)){
	for (i in 1:dim(simdfx)[1]) {
		resx <- noise_SE(simdfx$mn1[i], simdfx$mn2[i], simdfx$sdev[i], n, simdfx$delta[i])
		#this function outputs matrix of rbar, rbarsharps, and deltaIb1 (storage effect) for each noise
		#we only want the last
		
		simdfx$ELT[i] <- resx["ELT","deltaIb1"]
		simdfx$sym[i] <- resx["sym","deltaIb1"]
		simdfx$ERT[i] <- resx["ERT","deltaIb1"]
	
		if (i%%400 == 0) {
			print(i) #for keeping track of run 
		}
	} 
	#after filling one matrix for all unique params do again x-1 more times
	
	dflist[[m]] <- simdfx

}

#next loop is for:
	#saving elements of dflist
	#naming elements of dflist
	#mean across list elements

#initializing empty vector and matrices to be filled in loop
names <- NA
ELT <- matrix(NA, ncol = length(dflist), nrow = nrow(simdfx))
ERT <- matrix(NA, ncol = length(dflist), nrow = nrow(simdfx))
sym <- matrix(NA, ncol = length(dflist), nrow = nrow(simdfx))

for (i in 1:length(dflist)){
	
	#store each element of list (a d.f) as its own csv
	write.csv(dflist[[i]], paste("sim_df", i, sep =""))
	
	#filling vector for naming dflist 
	names[i] <- paste("sim_df", i, sep ="")
	
	#separating storage effect value by noise type 
	#into sep matrices
	#to be used to find means across x number of runs
	ELT[,i] <- dflist[[i]]$ELT	
	sym[,i] <- dflist[[i]]$sym	
	ERT[,i] <- dflist[[i]]$ERT	

}
names(dflist) <- names #naming list with vector we just created


#apply mean function across each noise matrix to find avg
ELT_mean <- apply(ELT, MARGIN =1, mean)
ERT_mean <- apply(ERT, MARGIN =1, mean)
sym_mean <- apply(sym, MARGIN =1, mean)

#plug these averages into a new data.frame simular to data.frames in list but shows average of x runs
mean_df <- data.frame(sdev=sdev_sims, mn1=mn1_sims, mn2=mn2_sims, delta=delta_sims,ELT = ELT_mean, sym = sym_mean, ERT = ERT_mean)

#also save this data.frame as csv
write.csv(mean_df, "sim_mean")

#-------------------------testing-----------------------
z <- sample(1:dim(simdfx)[1], 1)
z
noise_SE(mean_df$mn1[z], mean_df$mn2[z], mean_df$sdev[z], n, mean_df$delta[z])
mean_df[z,]
#-------------------------------------------------------






