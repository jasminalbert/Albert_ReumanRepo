source("./generate_noise.R") 

#set parameters
sdev <- c(0.8,0.8)
mn <- c(0.5, 0.6)
n <- 10^6

#generate x number of sets of noise
#one set of noise is extreme left tail, symmetric, and extreme right tail and their corresponding sharped distributions
x<- 10

sim_noise <- vector(mode = 'list', x)
for (m in 1:x){
	sim_noise[[m]] <- lapply(get_noise (mn = mn, sdev = sdev, n=n, check = FALSE), exp)
	
}


#computing storage effect for each set and each type of noise
source("./lottery_normcop.R")

delta <- seq(0,1,.05) #range of death rate values

#ELT
#initializing list to store computations for each x number of sets
deltELT <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)
delta_test_ELT <- list(deltELT = deltELT)
delta_test_ELT <- rep(delta_test_ELT[1], times = x)

for (m in 1:x) {
	for (j in delta){
		delta_test_ELT[[m]][((j*100)/5)+1,] <- lottery_normcop(sim_noise[[m]]$B_ELT, sim_noise[[m]]$B_ELT_sharp, delta = j, q12 =1)
	}
}

#sym
#initializing list to store computations for each x number of sets
deltsym <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)
delta_test_sym <- list(deltsym = deltsym)
delta_test_sym <- rep(delta_test_sym[1], times = x)

for (m in 1:x) {
	for (j in delta){
		delta_test_sym[[m]][((j*100)/5)+1,] <- lottery_normcop(sim_noise[[m]]$B_sym, sim_noise[[m]]$B_sym_sharp, delta = j, q12 =1)
	}
}

#ERT
#initializing list to store computations for each x number of sets
deltERT <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)
delta_test_ERT <- list(deltERT = deltERT)
delta_test_ERT <- rep(delta_test_ERT[1], times = x)

for (m in 1:x) {
	for (j in delta){
		delta_test_ERT[[m]][((j*100)/5)+1,] <- lottery_normcop(sim_noise[[m]]$B_ERT, sim_noise[[m]]$B_ERT_sharp, delta = j, q12 =1)
	}
}

#plot
plot(0, xlim = c(0,1), ylim = c(0, .03), type = 'n', ylab = "delta I", xlab = "death rate", main = "Delta I according to changes in death rate", sub = "sdev = (0.8,0.8), mn = (0.5, 0.6)")
legend("topleft", legend = c("extreme left tail", "symmetric", "extreme right tail"), col = c("red", "green", "blue"), box.lty =0)

for (m in 1:x) {
	lines(delta, delta_test_ELT[[m]][,4], col = rgb(1,0,0, 0.2))
	lines(delta, delta_test_sym[[m]][,4], col = rgb(0,1,0, 0.2))
	lines(delta, delta_test_ERT[[m]][,4], col = rgb(0,0,1, 0.2))
}

#add average lines
#finding means

#ELT
I_ELT <- data.frame(matrix(ncol = x, nrow = length(delta)))
for (m in 1:x) {
	I_ELT[,m] <- delta_test_ELT[[m]][,4]
}
meanELT <- apply(I_ELT, MARGIN = 1, FUN = mean)

#sym
I_sym <- data.frame(matrix(ncol = x, nrow = length(delta)))
for (m in 1:x) {
	I_sym[,m] <- delta_test_sym[[m]][,4]
}
meansym <- apply(I_sym, MARGIN = 1, FUN = mean)

#ERT
I_ERT <- data.frame(matrix(ncol = x, nrow = length(delta)))
for (m in 1:x) {
	I_ERT[,m] <- delta_test_ERT[[m]][,4]
}
meanERT <- apply(I_ERT, MARGIN = 1, FUN = mean)

#add mean lines
lines(delta, meanELT, col = "red", lty = 2)
lines(delta, meansym, col = "green", lty = 2)
lines(delta, meanERT, col = "blue", lty = 2)







