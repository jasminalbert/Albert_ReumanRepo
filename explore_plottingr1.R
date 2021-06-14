#density plots
load("./noise_etc.RData")
source("./plotr1_fxn.R")
library(sm)

sigma <- 3.2
mu <- c(0.1, 0.9)
delta <- 0.3
n <- transform(b_tilde, u_tilde, rho, sigma, mu)
b_s1 <- sigma*b_tilde$s[,1] + mu[1]
b_s2 <- sigma*b_tilde$s[,2] + mu[2]

l <- r_mets(n$b_l1-n$b_l2, delta, M)
r <- r_mets(n$b_r1-n$b_r2, delta, M)
s <- r_mets(n$u_s, delta, M)
sharp1 <- r_mets(n$u_1, delta, M)
sharp2 <- r_mets(n$u_2, delta, M)

DeltaI <- c(l$rbar_hat - sharp1$rbar_hat + sharp2$rbar_hat, 
            r$rbar_hat - sharp1$rbar_hat + sharp2$rbar_hat,
            s$rbar_hat - sharp1$rbar_hat + sharp2$rbar_hat)
DeltaI

invPrb <- c(length(n$b_l1[n$b_l1>n$b_l2])/M, 
            length(n$b_r1[n$b_r1>n$b_r2])/M, 
            length(n$u_s[n$u_s>0])/M)
invPrb

invE <- c(mean(l$r[l$r>0]),
          mean(r$r[r$r>0]),
          mean(s$r[s$r>0]))
invE
#symmetric has higher storage effect even though is has lower mean invasion rates 
#but higher invasion years
#so magnitude does not matter as much as yes or no (binary)
#when magnitude is higher invasion is faster? true or not?
#high invE means faster invasion -> no need for sequential invasive years to invade

#plot histogram
green <- rgb(0,1,0,0.3)
blue <- rgb(0,0,1,0.3)
red <- rgb(1,0,0,0.3)

round(max(l$r)-min(l$r))/0.2
round(max(r$r)-min(r$r))/0.2
round(max(s$r)-min(s$r))/0.2

hist(l$r, breaks=55, xlim=c(-0.5,10), col=green, border="white")
hist(r$r, breaks=55, xlim=c(-0.5,10), col=blue, border='white')
hist(s$r, breaks=35, xlim=c(-0.5,10), col=red, border='white')

r1hist <- function(r1,...){
  br <- round(max(r1)-min(r1))/0.2
  hist(r1, breaks=br, col='grey',border="white",...)
}

r1hist(l$r)

r1hist3 <- function(lr,rr,sr){
  
}


#biv distribution
#r1>0 when b1>b2


plotdis(n$b_l1[1:750],n$b_l2[1:750], main="left tail asymmetry")
plotdis(n$b_r1[1:750],n$b_r2[1:750], main="right tail asymmetry")
plotdis(b_s1[1:750],b_s2[1:750], main="symmetric")

pdf("rbar1_distribution.pdf")
for (i in 1:nrow(res$params)){
  s <- res$params[i,1]
  m <- as.numeric(res$params[i, 2:3])
  d <- res$params[i,4]
  
  plot_r1(s, m, d, plottype=2)
  
}
dev.off()





#looking at when rbar is negative
#theory is that even when rbar is negative there are still invasive years

lflag <- res$rbar$rbar1hat_l<0
rflag <- res$rbar$rbar1hat_r<0
sflag <- res$rbar$rbar1hat_s<0

Pflag <- res$params[lflag | rflag | sflag,]



pdf("negativerbar1.pdf")
for (i in 1:nrow(Pflag)){
  s <- Pflag[i,1]
  m <- as.numeric(Pflag[i, 2:3])
  d <- Pflag[i,4]
  
  plot_r1(s, m, d, plottype=1)
}
dev.off()
