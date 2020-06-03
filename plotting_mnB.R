source("./generate_noise.R") 

#varying mean birth rate of invader B[,1]

mn1 <- seq(0,1,0.05)
mn2 <- rep(0.5, times = length(mn1))
mn <- matrix(c(mn1,mn2), ncol =2)

#other parameters
sdev <- c(0.8, 0.8)
n <- 10^6

sim_noise2 <- vector(mode = 'list', length(mn1))
for (i in 1:length(mn1)) {
	sim_noise2[[i]]<-lapply(get_noise(mn = mn[i,], sdev = sdev, n=n, check=FALSE), exp)
} #each sub list has varying mn1

#ELT
mnELT <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)
mn_test_ELT <- list(mnELT = mnELT)
mn_test_ELT <- rep(mn_test_ELT[1], times = length(mn1))

#computes storage affect for every pair of means
for (i in 1:length(mn1)){
	mn_test_ELT[[i]] <- lottery_normcop(sim_noise2[[i]]$B_ELT, sim_noise2[[i]]$B_ELT_sharp, delta = 0.75, q=1)
}
I_ELTmn <- vector(length = length(mn1))
for (i in 1:length(mn1)){
	I_ELTmn[i] <- mn_test_ELT[[i]][4]
}

#sym
mnsym <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)
mn_test_sym <- list(mnsym = mnsym)
mn_test_sym <- rep(mn_test_sym[1], times = length(mn1))

#computes storage affect for every pair of means
for (i in 1:length(mn1)){
	mn_test_sym[[i]] <- lottery_normcop(sim_noise2[[i]]$B_sym, sim_noise2[[i]]$B_sym_sharp, delta = 0.75, q=1)
}
I_symmn <- vector(length = length(mn1))
for (i in 1:length(mn1)){
	I_symmn[i] <- mn_test_sym[[i]][4]
}

#ERT
mnERT <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)
mn_test_ERT <- list(mnERT = mnERT)
mn_test_ERT <- rep(mn_test_ERT[1], times = length(mn1))

#computes storage affect for every pair of means
for (i in 1:length(mn1)){
	mn_test_ERT[[i]] <- lottery_normcop(sim_noise2[[i]]$B_ERT, sim_noise2[[i]]$B_ERT_sharp, delta = 0.75, q=1)
}
I_ERTmn <- vector(length = length(mn1))
for (i in 1:length(mn1)){
	I_ERTmn[i] <- mn_test_ERT[[i]][4]
}


plot(mn1, I_ELTmn, type = 'l')
lines(mn1, I_symmn, col = "blue")
lines(mn1, I_ERTmn, col = "red")
