source("./generate_noise.R")
source("./lottery_normcop.R")

#need example simulation for population size and coexistence of 2 species and different tail associations
#simulate N_i for resident and invader 
#N1 is invader; initial abundance <10^-8
#N2 is resident; initial abundance set to 50?

#
time <- 1000

d <- 0.5
sigma <- 1.6
mu1 <- 0.1
mu2 <- 0.1

noise <-get_noise(mn=c(mu1,mu2),sdev=c(sigma,sigma),n=time,check = F)

lottery_normcop(exp(noise$B_ELT), exp(noise$B_ELT_sharp), d, 1)
lottery_normcop(exp(noise$B_sym), exp(noise$B_sym_sharp), d, 1)
lottery_normcop(exp(noise$B_ERT), exp(noise$B_ERT_sharp), d, 1)

B1_ELT <- exp(noise$B_ELT[,1])
B2_ELT <- exp(noise$B_ELT[,2])

B1_sym <- exp(noise$B_sym[,1])
B2_sym <- exp(noise$B_sym[,2])

B1_ERT <- exp(noise$B_ERT[,1])
B2_ERT <- exp(noise$B_ERT[,2])

N <- 50
N1 <- 9e-09
N2 <- N-N1[1]
r1 <- NA
r2 <- NA

pdf('pop_sim.pdf')

#Extreme left tail
for (t in 1:time) {
	#N2[t] <- N-N1[t]
	N1[t+1] <- (1-d)*N1[t] + d*N*((B1_ELT[t]*N1[t])/								((B1_ELT[t]*N1[t])+(B2_ELT[t]*N2[t])))
	N2[t+1] <- (1-d)*N2[t] + d*N*((B2_ELT[t]*N2[t])/								((B1_ELT[t]*N1[t])+(B2_ELT[t]*N2[t])))
	
	r1[t] <- log(N1[t+1]/N1[t])
	r2[t] <- log(N2[t+1]/N2[t])
	
}

N1_ELT <- N1
N2_ELT <- N2

plot(N1_ELT[1:1000], type = 'l', ylim=c(0,50))
lines(N2_ELT, type = 'l', col = 'red')
plot(r1, type = 'l')
plot(r2, type = 'l')
r1_ELT <- mean(r1)
r2_ELT <- mean(r2)


#symmetric
for (t in 1:time) {
	N1[t+1] <- (1-d)*N1[t] + d*N*((B1_sym[t]*N1[t])/								((B1_sym[t]*N1[t])+(B2_sym[t]*N2[t])))
	N2[t+1] <- (1-d)*N2[t] + d*N*((B2_sym[t]*N2[t])/								((B1_sym[t]*N1[t])+(B2_sym[t]*N2[t])))
	
	r1[t] <- log(N1[t+1]/N1[t])
	r2[t] <- log(N2[t+1]/N2[t])
}

N1_sym <- N1
N2_sym <- N2

plot(N1_sym[1:1000], type = 'l', ylim=c(0,50))
lines(N2_sym, col = 'red')
plot(r1, type = 'l')
plot(r2, type = 'l')

r1_sym <- mean(r1)
r2_sym <- mean(r2)

#Extreme right tail
for (t in 1:time) {
	N1[t+1] <- (1-d)*N1[t] + d*N*((B1_ERT[t]*N1[t])/								((B1_ERT[t]*N1[t])+(B2_ERT[t]*N2[t])))
	N2[t+1] <- (1-d)*N2[t] + d*N*((B2_ERT[t]*N2[t])/								((B1_ERT[t]*N1[t])+(B2_ERT[t]*N2[t])))
	
	r1[t] <- log(N1[t+1]/N1[t])
	r2[t] <- log(N2[t+1]/N2[t])
}


N1_ERT <- N1
N2_ERT <- N2

plot(N1_ERT[1:1000], type = 'l', ylim =c(0,50))
lines(N2_ERT, type = 'l', col = 'red')


plot(r1, type = 'l')
plot(r2, type = 'l')
r1_ERT <- mean(r1)
r2_ERT <- mean(r2)

##################################################

LTGR <- data.frame(r1 = 1:3, r2 = 1:3)

LTGR$r1 <- c(r1_ELT,r1_sym,r1_ERT)
LTGR$r2 <- c(r2_ELT,r2_sym,r2_ERT)


lottery_normcop(exp(noise$B_ELT), exp(noise$B_ELT_sharp), d, 1)
lottery_normcop(exp(noise$B_sym), exp(noise$B_sym_sharp), d, 1)
lottery_normcop(exp(noise$B_ERT), exp(noise$B_ERT_sharp), d, 1)

#plot(B1_ELT, type = 'l')
#lines(B2_ELT, col = 'red')
#plot(B1_sym, type = 'l')
#lines(B2_sym, col = 'red')
#plot(B1_ERT, type = 'l')
#lines(B2_ERT, col = 'red')

pdf('pop_sim.pdf')
par(mfrow = c(3,2))

plot(noise$B_ELT[1:1000,1], type = 'l')
lines(noise$B_ELT[1:1000,2], col = 'red')

plot(N1_ELT[1:300000], type = 'l', ylim=c(0,50))
lines(N2_ELT, type = 'l', col = 'red')

plot(noise$B_sym[1:1000,1], type = 'l')
lines(noise$B_sym[1:1000,2], col = 'red')

plot(N1_sym[1:300000], type = 'l', ylim=c(0,50))
lines(N2_sym, col = 'red')

plot(noise$B_ERT[1:1000,1], type = 'l')
lines(noise$B_ERT[1:1000,2], col = 'red')


plot(N1_ERT[1:300000], type = 'l', ylim =c(0,50))
lines(N2_ERT, type = 'l', col = 'red')


dev.off()

#########################################################


#r1[t] <- log((1-d) + ((d*N*B1_ERT[t])/((B1_ERT[t]*N1[t])+												(B2_ERT[t]*N2[t]))))
#r2[t] <- log((1-d) + ((d*N*B2_ERT[t])/((B1_ERT[t]*N1[t])+												(B2_ERT[t]*N2[t]))))


#a case where tail association causes coexistence

