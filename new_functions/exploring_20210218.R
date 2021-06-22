source("./new_functions/getnoise.r")
source("./new_functions/SE_lottery.R")
source("./new_functions/measure.co3.R")
source("./new_functions/popsim2.R")
source("./new_functions/co.periods.R")

#do asymmetrically associated environmental fluctuations and thus birth rates also produce asymmetrically associated population sizes?

params <- c(0.8, 0.8, 3.2)
corval <- 0.88
corRT <- getcorpar(0,.5,corval,numpts=100000,allowneg=FALSE,ploton=FALSE)

bnoise <- getnoise2(mu=params[1:2], sigma=rep(params[3],2), n=10^6, corval=corval, corRT=corRT)
Bnoise <- exp(bnoise)

# environmental noise
plot(bnoise$LT1[1:10000],bnoise$LT2[1:10000], main="left tailed environmental variation")
plot(bnoise$RT1[1:10000],bnoise$RT2[1:10000], main="right tailed environmental variation")
plot(bnoise$sym1[1:10000],bnoise$sym2[1:10000], main="symmetric tailed environmental variation")
hist(bnoise$LT1);mean(bnoise$LT1);sd(bnoise$LT1)
hist(bnoise$LT2);mean(bnoise$LT2);sd(bnoise$LT2)
hist(bnoise$RT2);mean(bnoise$RT2);sd(bnoise$RT2)
hist(bnoise$RT1);mean(bnoise$RT1);sd(bnoise$RT1)
hist(bnoise$sym2);mean(bnoise$sym2);sd(bnoise$sym2)
hist(bnoise$sym1);mean(bnoise$sym1);sd(bnoise$sym1)


# environmentally dependent parameter
plot(Bnoise$LT1[1:10000],Bnoise$LT2[1:10000], main="left tailed environmentally dependent paramter = B (birth rate)")
plot(Bnoise$RT1[1:10000],Bnoise$RT2[1:10000], main="right tailed environmentally dependent paramter = B (birth rate)")
plot(Bnoise$sym1[1:10000],Bnoise$sym2[1:10000], main="right tailed environmentally dependent paramter = B (birth rate)")
hist(Bnoise$LT1);mean(Bnoise$LT1);sd(Bnoise$LT1)
hist(Bnoise$LT2);mean(Bnoise$LT2);sd(Bnoise$LT2)
hist(Bnoise$RT2);mean(Bnoise$RT2);sd(Bnoise$RT2)
hist(Bnoise$RT1);mean(Bnoise$RT1);sd(Bnoise$RT1)
hist(Bnoise$sym2);mean(Bnoise$sym2);sd(Bnoise$sym2)
hist(Bnoise$sym1);mean(Bnoise$sym1);sd(Bnoise$sym1)

#population
popLT <- popsim2(Bnoise[,c('LT1', 'LT2')], 50, 25, 0.5, 10^6)
popRT <- popsim2(Bnoise[,c('RT1', 'RT2')], 50, 25, 0.5, 10^6)
popsym <- popsim2(Bnoise[,c('sym1', 'sym2')], 50, 25, 0.5, 10^6)

plot(popRT$N1[1:10000], popRT$N2[1:10000])
plot(popLT$N1[1:1000], type = 'l')
plot(popRT$N1[1:1000], type='l')
plot(popsym$N1[1:1000], type='l')


#one variable

#left tailed
plot(bnoise$LT1[500:1500], main="left tailed environmental variation",type='l', ylim=c(-10,10))
plot(bnoise$sym1[500:1500], main="symmetrc tailed environmental variation",type='l',ylim=c(-10,10))
plot(bnoise$RT1[500:1500], main="right tailed environmental variation",type='l',ylim=c(-10,10))


plot(Bnoise$LT1[500:1500], main="left tailed environmentally dependent parameter1", type='l')
plot(Bnoise$LT2[500:1500], main="left tailed environmentally dependent parameter2", type='l')
plot(Bnoise$sym1[500:1500], main="symmetrc tailed environmentally dependent parameter1", type='l')
plot(Bnoise$sym2[500:1500], main="symmetrc tailed environmentally dependent parameter2", type='l')
plot(Bnoise$RT1[500:1500], main="right tailed environmentally dependent parameter1", type='l')
plot(Bnoise$RT2[500:1500], main="right tailed environmentally dependent parameter2", type='l')

#competition 
# competition = (B1[t]*N1[t]) + (B2[t]*N2[t])/(delta*N) = B2[t]/delta
CLT <- competition(Bnoise[,c('LT1', 'LT2')], 50, 25, 0.5, 10^6)
CRT <- competition(Bnoise[,c('RT1', 'RT2')], 50, 25, 0.5, 10^6)
Csym <- competition(Bnoise[,c('sym1', 'sym2')], 50, 25, 0.5, 10^6)

CLT2 <- Bnoise$LT2/delta


plot(CLT[500:1500], type = 'l', ylim=c(0,200000))
plot(CLT2[500:1500], type = 'l', ylim=c(0,200000))
plot(CLT2, type = 'l')
plot(CLT, type = 'l')


plot(CRT[500:1500], type='l', ylim=c(0,200000))
plot(Csym[500:1500], type='l', ylim=c(0,200000))
mean(CLT); mean(CRT); mean(Csym)
sd(CLT); sd(CRT); sd(Csym)

hist(log(CLT), breaks=75, xlim=c(-10,15), ylim=c(0,75000))
hist(log(CRT), breaks=75, xlim=c(-10,15), ylim=c(0,75000))
hist(log(Csym), breaks=75, xlim=c(-10,15), ylim=c(0,75000))
mean(log(CLT)); mean(log(CRT)); mean(log(Csym))
sd(log(CLT)); sd(log(CRT)); sd(log(Csym))

hist(CLT, breaks=1000000)
hist(log(Bnoise$LT1/CLT), breaks=40,xlim=c(-10,5))
hist(log(Bnoise$RT1/CRT) , breaks=40, xlim=c(-10,5))
hist(log(Bnoise$sym1/Csym) , breaks=20, xlim=c(-10,5))



plot(popLT$N1[500:1500], type = 'l')
plot(popRT$N1[500:1500], type='l')
plot(popsym$N1[500:1500], type='l')




left <- data.frame(B1=Bnoise$LT1, B2=Bnoise$LT2, C=CLT, EC=Bnoise$LT1/CLT, N1=popLT$N1[-1000001], N2=popLT$N2[-1000001])

left[left$C==max(left$C),]
left[538405:538415,]

#plot IGR1 vs IGR2 

params <- c(0.4, 0.5, 3.2)
corval <- 0.88
corRT <- getcorpar(0,.5,corval,numpts=100000,allowneg=FALSE,ploton=FALSE)

bnoise2 <- getnoise2(mu=params[1:2], sigma=rep(params[3],2), n=10^6, corval=corval, corRT=corRT)
Bnoise2 <- exp(bnoise2)

bnoise2sharp <- getnoise2(mu=params[1:2], sigma=rep(params[3],2), n=10^6, corval=corval, corRT=corRT)
Bnoise2sharp <- exp(bnoise2sharp)

bnoise3 <- getnoise2(mu=params[2:1], sigma=rep(params[3],2), n=10^6, corval=corval, corRT=corRT)
Bnoise3 <- exp(bnoise3)

bnoise3sharp <- getnoise2(mu=params[2:1], sigma=rep(params[3],2), n=10^6, corval=corval, corRT=corRT)
Bnoise3sharp <- exp(bnoise3sharp)

cov(Bnoise3$LT1, Bnoise3$LT2); cor(Bnoise3$LT1, Bnoise3$LT2)
cov(Bnoise3$sym1, Bnoise3$sym2); cor(Bnoise3$sym1, Bnoise3$sym2)
cov(Bnoise3$RT1, Bnoise3$RT2); cor(Bnoise3$RT1, Bnoise3$RT2)

cov(bnoise3$LT1, bnoise3$LT2); cor(bnoise3$LT1, bnoise3$LT2)
cov(bnoise3$sym1, bnoise3$sym2); cor(bnoise3$sym1, bnoise3$sym2)
cov(bnoise3$RT1, bnoise3$RT2); cor(bnoise3$RT1, bnoise3$RT2)

sum(abs(bnoise3$LT1/bnoise3$LT2)) #5,724,192
sum(abs(bnoise3sharp$LT1/bnoise3$LT2)) #11,095,135
sum(abs(bnoise3$sym1/bnoise3$sym2)) #4,426,064
sum(abs(bnoise3sharp$sym1/bnoise3$sym2)) #8,361,638
sum(abs(bnoise3$RT1/bnoise3$RT2)) #1,451,903
sum(abs(bnoise3sharp$RT1/bnoise3$RT2)) #10,187,198


plot(bnoise3$sym1[1:10000], bnoise3$sym2[1:10000])
plot(bnoise3$LT1[1:10000], bnoise3$LT2[1:10000])
plot(bnoise3$RT1[1:10000], bnoise3$RT2[1:10000])



SE2 <- SE_lottery(Bnoise2, Bnoise2sharp, 0.5)
SE3 <- SE_lottery(Bnoise3, Bnoise3sharp, 0.5)

IGR1 <- 0
IGR2 <- 0

for (t in 1:10^5){
	IGR1[t] <- log(1 - delta + ((delta*Bnoise2$LT1[t])/Bnoise2$LT2[t]))
	IGR2[t] <- log(1 - delta + ((delta*Bnoise3$LT1[t])/Bnoise3$LT2[t]))
	cat("at", t, "IGR1 is", IGR1[t], "IGR2 is", IGR2[t])
}

plot(IGR1)
plot(IGR2)
plot(IGR1, IGR2) #uncorrelated?
cor(IGR1,IGR2); cov(IGR1,IGR2)
mean(IGR1)
mean(IGR2)
SE2;SE3












