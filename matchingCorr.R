source("./generate_noise.R") 
source("./newcop.r")

#set parameters
sdev <- c(0.8,0.8)
mn <- c(0.5, 0.6)
n <- 10^6

#generate noise
noise <- get_noise(mn = mn, sdev = sdev, n=n, check = FALSE)

#check correlation - same
lapply(noise, cor)

#exponentiate
xnoise <- lapply(noise, exp)

#check correlation - very different
lapply(xnoise, cor)

#manipulate tail associations so that correlations of exponentiated distributions are the same
#match to correlation of symmetric noise

###ELT
cop_ELT <- newcop(n,2,0,.887,1)
#plot(res[,1],res[,2],type="p")

cop_ELT[,1] <- qnorm(cop_ELT[,1], 0.5, 0.8)
cop_ELT[,2] <- qnorm(cop_ELT[,2], 0.6, 0.8)

xcop_ELT <- exp(cop_ELT)
#plot(xcop_ELT[,1],xcop_ELT[,2],type="p")
cor(xcop_ELT)

###ERT
cop_ERT <- newcop(n,2,0,0,.911)
#plot(cop_ERT[1:1000,1],cop_ERT[1:1000,2],type="p")

cop_ERT[,1] <- qnorm(cop_ERT[,1], 0.5, 0.8)
cop_ERT[,2] <- qnorm(cop_ERT[,2], 0.6, 0.8)

xcop_ERT <- exp(cop_ERT)
#plot(xcop_ERT[1:1000,1],xcop_ERT[1:1000,2],type="p")
cor(xcop_ERT)

