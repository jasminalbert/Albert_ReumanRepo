#----------------might be useless 
# just shows that if b1>b2, the r1<0 obvi

#probalility that b1>b2
load("./noise_etc.RData")
source("./transform.R")
source("./r_function.R")

sigma <- 1.6
mu <- c(0.1,0.9)
delta <- 0.1
rv <- transform(b_tilde, u_tilde, rho, sigma, mu) 
list2env(rv, globalenv())


#b_s1 <- sigma*b_tilde$s[,1] + mu[1]
#b_s2 <- sigma*b_tilde$s[,2] + mu[2]
 






length(b_l1[b_l1>b_l2])/M
l <- r_mets(b_l1-b_l2, delta, M)
l$r[b_l1>b_l2]

#head(cbind(b_l1>b_l2, b_l1, r1_l))

fact <- as.factor(b_l1>b_l2)
plot(b_l1>b_l2, ylim=c(min(l$r[1:80]),max(l$r[1:80])), xlim=c(0,80))
#points(b_l1, col="green", cex=0.7)
points(l$r[1:80], cex=0.7, col='red', bg=c("black","red")[fact], pch=21)


length(b_r1[b_r1>b_r2])/M
r1_r <- r(b_r1-b_r2, delta)

fact <- as.factor(b_r1>b_r2)
plot(b_r1>b_r2, ylim=c(min(r1_r[1:80]),0.1), xlim=c(0,80))
points(r1_r[1:80], cex=0.7, col='red', bg=c("black","red")[fact], pch=21)

length(u_s[u_s>0])/M #probability in symmetric case is higher
r1_s <- r(u_s, delta)

fact <- as.factor(u_s>0)
plot(u_s>0, ylim=c(min(r1_s[1:80]),0.5), xlim=c(0,80))
points(r1_s[1:80], cex=0.7, col='red', bg=c("black","red")[fact], pch=21)

#head(cbind(u_s,u_s>0, r1_s,c("black","red")[fact]))

#E(r1|r1>0)

mean(r1_l[r1_l>0])
mean(r1_l[b_l1>b_l2])

mean(r1_r[r1_r>0])
mean(r1_r[b_r1>b_r2])

mean(r1_s[r1_s>0])
mean(r1_s[u_s>0])

hist(r1_s)







