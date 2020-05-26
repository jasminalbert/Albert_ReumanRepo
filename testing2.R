source("./ExtremeTailDep.R")
source("./ModTailAssociatedNoise.R")
source("./generate_noise.R")
source("./lottery_normcop.R")

mn <- c(.5,.6)
sdev <- c(.8,.8)
n <- 10^6
set.seed(1212)
res1<-get_noise(mn=mn,sdev=sdev,n=n,check = FALSE) #generate left extreme, right extreme and symmetric
res2 <- Modnoise(mu = mn, sd = sdev, totT =n)
#generate moderate noise
res <- c(res1, res2) #combine into one list

lapply(res, head)
res <- res[c(1,2,7,8,3,4,9,10,5,6)] #reorder list
names <- names(res)


#checking plots
for (i in 1:10) {
	plot(res[[i]][1:1000,1], res[[i]][1:1000,2], main = names[i])
	i <- i + 2 
}

xres <- lapply(res, exp)
#lapply(expres, head)

#test storage effect 

SEres <- vector(mode = 'list',10)
names(SEres) <- c("ELT", "ELT", "MLT", "MLT", "sym","sym", "MRT","MRT", "ERT","ERT")

i<-1
for (i in 1:9) {

	SEres[[i]]<- lottery_normcop(xres[[i]], 			 xres[[i+1]], delta = 0.25, q12 =1)
	
	i <- i+2
}


SE<-SEres[c(1,3,5,7,9)]


#VVVCHECKINGVVV#
#extreme left
lottery_normcop(exp(res$B_ELT), exp(res$B_ELT_sharp), delta = .25, q12 =1)

lottery_normcop(xres[[1]], xres[[1+1]], delta = 0.25, q12 =1)

SEres[[1]]

#moderate left
lottery_normcop(exp(res$B_MLT), exp(res$B_MLT_sharp), delta = .25, q12 =1)

lottery_normcop(xres[[3]], xres[[3+1]], delta = 0.25, q12 =1)

SEres[[3]]

#symmetric
lottery_normcop(exp(res$B_sym), exp(res$B_sym_sharp), delta = .25, q12 =1)

lottery_normcop(xres[[5]], xres[[5+1]], delta = 0.25, q12 =1)

SEres[[5]]

#moderate right
lottery_normcop(exp(res$B_MRT), exp(res$B_MRT_sharp), delta = .25, q12 =1)

lottery_normcop(xres[[7]], xres[[7+1]], delta = 0.25, q12 =1)

SEres[[7]]

#extreme right
lottery_normcop(exp(res$B_ERT), exp(res$B_ERT_sharp), delta = .25, q12 =1)

lottery_normcop(xres[[9]], xres[[9+1]], delta = 0.25, q12 =1)

SEres[[9]]
#---------------------------------------------------------









