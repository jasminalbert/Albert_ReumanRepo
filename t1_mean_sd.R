
mu <- seq(0.1,1,0.1)
mu1 <- sample(mu,1)
mu2 <- sample(mu,1)
sdev <- seq(0.4,6.4,.4)
sigma <- rep(sample(sdev,1),2)

bnoise <- getnoise2(mu=c(mu1,mu2), sigma=sigma, n=10^6, corval=corval, corRT=corRT)

L1 <- data.frame(b1=bnoise$LT1[bnoise$LT1<mu1], b2=bnoise$LT2[bnoise$LT2<mu2])
L2 <- data.frame(b1=bnoise$LT1[bnoise$LT1>mu1], b2=bnoise$LT2[bnoise$LT2>mu2])
D <- L1$b1 - L1$b2

cat("parameters: mu1=",mu1, " mu2=",mu2," sigma=", sigma[1], "\n\n")
cat("D = b1t1-b2t1 \n\n")
cat("mean(D):",mean(D), "sd(D):",sd(D) )

#source("./t1_mean_sd.R")
