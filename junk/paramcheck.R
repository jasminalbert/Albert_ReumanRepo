source("./generate_noise.R") 

sdev <- c(0.8,0.8)
mn <- c(0.5, 0.6)
n <- 10^6

noise <- get_noise (mn = mn, sdev = sdev, n=n, check = FALSE)

names <- names(noise)

#checking plots
for (i in 1:6) {
	plot(noise[[i]][1:1000,1], noise[[i]][1:1000,2], main = names[i])
	i <- i + 1 
}

#exponentiate 
xnoise <- lapply(noise, exp)

#check plots again
for (i in 1:6) {
	plot(xnoise[[i]][1:1000,1], xnoise[[i]][1:1000,2], main = names[i])
	i <- i + 1 
}

#make list of storage effect values for each noise
source("./lottery_normcop.R")


SE <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)

i <- 1
for (i in seq(1,5,2)) {
	SE[(i+1)/2,] <- lottery_normcop(xnoise[[i]], xnoise[[i+1]], delta = 0.25, q12 =1)
}
row.names(SE) <- c("ELT", "sym", "ERT")

#checking parameters
delta <- seq(0,1,.05)

#ELT
deltELT <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)
for (i in delta) {
	deltELT[((i*100)/5)+1,] <- lottery_normcop(xnoise$B_ELT, xnoise$B_ELT_sharp, delta = i, q12 =1)

}
row.names(deltELT) <- as.character(delta)

#sym
deltsym <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)

lottery_normcop(xnoise$B_ELT, xnoise$B_ELT_sharp, delta = .95, q12 =1)

deltELT <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)
deltsym <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)
deltERT <- data.frame(rbar1=NA, rsharp1=NA, rsharp2=NA, deltaIb1=NA)
delta_test <- list(deltELT = deltELT, deltsym = deltsym, deltERT = deltERT)

for (i in seq(1,5,2)) {
	for (j in delta) {
		delta_test[[(i+1)/2]][((j*100)/5)+1,] <- lottery_normcop(xnoise[[i]], xnoise[[i+1]], delta = j, q12 =1)
	}
}

plot(delta, delta_test$deltELT[,4], type = 'l', col = "blue")
lines(delta, delta_test$deltsym[,4], type = 'l', col = "red")
lines(delta, delta_test$deltERT[,4], type = 'l', col = "green")
legend("topright", legend = c("extreme left tail", "symmetric", "extreme righ tail"), col = c("blue", "red", "green"), box.lty =0)








