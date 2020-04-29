source("./ExtremeTailDep.R")
source("./ModTailAssociatedNoise.R")
source("./generate_noise.R")
source("./lottery_normcop.R")

mn <- c(.5,.6)
sdev <- c(.8,.8)
n <- 10^6

res<-get_noise(mn=mn,sdev=sdev,n=n,check = FALSE) #generate left extreme, right extreme and symmetric

B_MLT <- ModTail(mu=mn, sd=sdev, totT=n, tail='left')
B_MLT_sharp <- ModTail(mu=mn, sd=sdev, totT=n, tail='left')

B_MRT <- ModTail(mu=mn, sd=sdev, totT=n, tail='right')
B_MRT_sharp <- ModTail(mu=mn, sd=sdev, totT=n, tail='right')

#test storage effect 

#symmetric
lottery_normcop(exp(res$B_sym), exp(res$B_sym_sharp), delta = .25, q12 =1)

#extreme left
lottery_normcop(exp(res$B_ELT), exp(res$B_ELT_sharp), delta = .25, q12 =1)

#extreme right
lottery_normcop(exp(res$B_ERT), exp(res$B_ERT_sharp), delta = .25, q12 =1)

#moderate left
lottery_normcop(exp(B_MLT), exp(B_MLT_sharp), delta = .25, q12 =1)

#moderate right
lottery_normcop(exp(B_MRT), exp(B_MRT_sharp), delta = .25, q12 =1)

