#check plots

#noise_b
plot(noise_b[[1]]$LT1[1:10000], noise_b[[1]]$LT2[1:10000])
plot(noise_b[[1]]$sym1[1:10000], noise_b[[1]]$sym2[1:10000])
plot(noise_b[[1]]$RT1[1:10000], noise_b[[1]]$RT2[1:10000])
hist(noise_b[[1]]$LT1[1:10000])
hist(noise_b[[1]]$LT2[1:10000])
hist(noise_b[[1]]$sym1[1:10000])
hist(noise_b[[1]]$sym2[1:10000])
hist(noise_b[[1]]$RT1[1:10000])
hist(noise_b[[1]]$RT2[1:10000]) #all normal


#noise_b_sharp
plot(noise_b_sharp[[1]]$LT1[1:10000], noise_b_sharp[[1]]$LT2[1:10000])
plot(noise_b_sharp[[1]]$sym1[1:10000], noise_b_sharp[[1]]$sym2[1:10000])
plot(noise_b_sharp[[1]]$RT1[1:10000], noise_b_sharp[[1]]$RT2[1:10000])
hist(noise_b_sharp[[1]]$LT1[1:10000])
hist(noise_b_sharp[[1]]$LT2[1:10000])
hist(noise_b_sharp[[1]]$sym1[1:10000])
hist(noise_b_sharp[[1]]$sym2[1:10000])
hist(noise_b_sharp[[1]]$RT1[1:10000])
hist(noise_b_sharp[[1]]$RT2[1:10000]) #all normal

#noise_B
plot(noise_B[[1]]$LT1[1:10000], noise_B[[1]]$LT2[1:10000])
plot(noise_B[[1]]$sym1[1:10000], noise_B[[1]]$sym2[1:10000])
plot(noise_B[[1]]$RT1[1:10000], noise_B[[1]]$RT2[1:10000])
hist(noise_B[[1]]$LT1[1:10000])
hist(noise_B[[1]]$LT2[1:10000])
hist(noise_B[[1]]$sym1[1:10000])
hist(noise_B[[1]]$sym2[1:10000])
hist(noise_B[[1]]$RT1[1:10000])
hist(noise_B[[1]]$RT2[1:10000])

#noise_B
plot(noise_B[[1]]$LT1[1:10000], noise_B[[1]]$LT2[1:10000])
plot(noise_B[[1]]$sym1[1:10000], noise_B[[1]]$sym2[1:10000])
plot(noise_B[[1]]$RT1[1:10000], noise_B[[1]]$RT2[1:10000])
hist(noise_B[[1]]$LT1[1:10000])
hist(noise_B[[1]]$LT2[1:10000])
hist(noise_B[[1]]$sym1[1:10000])
hist(noise_B[[1]]$sym2[1:10000])
hist(noise_B[[1]]$RT1[1:10000])
hist(noise_B[[1]]$RT2[1:10000])

#noise_B
plot(noise_B_sharp[[1]]$LT1[1:10000], noise_B_sharp[[1]]$LT2[1:10000])
plot(noise_B_sharp[[1]]$sym1[1:10000], noise_B_sharp[[1]]$sym2[1:10000])
plot(noise_B_sharp[[1]]$RT1[1:10000], noise_B_sharp[[1]]$RT2[1:10000])
hist(noise_B_sharp[[1]]$LT1[1:10000])
hist(noise_B_sharp[[1]]$LT2[1:10000])
hist(noise_B_sharp[[1]]$sym1[1:10000])
hist(noise_B_sharp[[1]]$sym2[1:10000])
hist(noise_B_sharp[[1]]$RT1[1:10000])
hist(noise_B_sharp[[1]]$RT2[1:10000])

SE_list[[1]]

#pop
plot(pop[[1]]$N1_ELT, type='l')
lines(pop[[1]]$N2_ELT, col='red')
plot(pop[[1]]$N1_sym, type='l')
lines(pop[[1]]$N2_sym, col='red')
plot(pop[[1]]$N1_ERT, type='l')
lines(pop[[1]]$N2_ERT, col='red')


#exploring how to set seed

ss.list <- vector(mode='list', length=2)


for (s in 1:3){
	set.seed(123)
	ss.list[[s]]<-getnoise2(mu = c(params$mu1[i], params$mu2[i]), sigma = c(params$sigma[i],params$sigma[i]), n=50, corval=corval, corRT=corRT)
}

replist <- vector(mode='list', length=3)

set.seed(2)
for (s in 1:3){
	for (i in 1:2){
		ss.list[[i]]<-getnoise2(mu = c(params$mu1[i], params$mu2[i]), sigma = c(params$sigma[i],params$sigma[i]), n=20, corval=corval, corRT=corRT)
	}
	replist[[s]] <- ss.list
}
replist2<-replist



SE_list[[i]]["ELT",c("Delta_B1", "rbar1","rbar1_sharp")]





