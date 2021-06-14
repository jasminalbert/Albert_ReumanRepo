#probably junk
load("./noise_etc.RData")
readRDS("/results_numeric/rbar_DeltaI.RDS")

lflag <- res$rbar$rbar1hat_l<0
rflag <- res$rbar$rbar1hat_r<0
sflag <- res$rbar$rbar1hat_s<0

rbarlflag <- res$rbar$rbar1hat_l[lflag]
rbarrflag <- res$rbar$rbar1hat_r[rflag]
rbarsflag <- res$rbar$rbar1hat_s[sflag]

sdlflag <- res$sd_r$sd_r1_l[lflag]
sdrflag <- res$sd_r$sd_r1_r[rflag]
sdsflag <- res$sd_r$sd_r1_s[sflag]


plflag <- res$params[lflag | rflag | sflag,]
prflag <- res$params[rflag,]
psflag <- res$params[sflag,]

Pflag <- res$params[lflag | rflag | sflag,]

ldat <- data.frame(rbar = rbarlflag, sd=sdlflag, plflag)
rdat <- data.frame(rbar = rbarrflag, sd=sdrflag, prflag)
sdat <- data.frame(rbar = rbarsflag, sd=sdsflag, psflag)

#left tail
largesd <- ldat[order(ldat$sd, decreasing=F),]; largesd
lsd_ordered <- largesd[order(largesd$rbar),]; lsd_ordered

sigma <- 1.6
mu <- c(0.1,0.9)
delta <- 0.1
b_l1 <- sigma*b_tilde$l[,1] + mu[1]
b_l2 <- sigma*b_tilde$l[,2] + mu[2]
r1_l <- r(b_l1-b_l2, delta)

plot(b_l1, b_l2)
hist(b_l1)
hist(b_l2)
plot(r1_l[1:100], type='l', xlab='timestep', ylab='r1(L)', col='blue')
abline(h=mean(r1_l), lty=3, col='red')
points(b_l1[1:100]-b_l2[1:100], cex=0.75)


hist(r1_l)

#right tail

largesd <- rdat[order(rdat$sd,decreasing=TRUE),]
lsd_ordered <- largesd[order(largesd$rbar),]

sigma <- 6.4
mu <- c(0.1,0.9)
delta <- 1
b_r1 <- sigma*b_tilde$r[,1] + mu[1]
b_r2 <- sigma*b_tilde$r[,2] + mu[2]
r1_r <- r(b_r1-b_r2, delta)

plot(r1_r[1:100], type='l', xlab='timestep', ylab='r1', col='blue')
points(b_r1[1:100]-b_r2[1:100], cex=0.75)

#symmetric

largesd <- sdat[order(sdat$sd,decreasing=TRUE),]
lsd_ordered <- largesd[order(largesd$rbar),]
sigma <- 1.6
mu <- c(0.1,0.9)
delta <- 1
b_s1 <- sigma*b_tilde$s[,1] + mu[1]
b_s2 <- sigma*b_tilde$s[,2] + mu[2]
r1_s <- r(b_s1-b_s2, delta)


plot(r1_s[1:100], type='l', xlab='timestep', ylab='r1', col='blue')
points(b_s1[1:100]-b_s2[1:100], cex=0.75)


source("./plotr1_fxn.R")
pdf("negativerbar1.pdf")
for (i in 1:nrow(Pflag)){
  s <- Pflag[i,1]
  m <- as.numeric(Pflag[i, 2:3])
  d <- Pflag[i,4]
  
  plot_r1(s, m, d, plottype=1)
}
dev.off()
i <- 1

r1b <- cbind(r1_l, b_l1, b_l2)[1:20,]
i <-3
r1b[3,]
log(1-delta+delta*exp(b_l1[i]-b_l2[i]))
log(exp(b_l1[i]-b_l2[i]))
exp(b_l1[i]-b_l2[i])
b_l1[i]-b_l2[i]

log(0.449329)