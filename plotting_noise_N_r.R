source("./plotr1_fxn.R")
# we want to know if there will ever be cases where extreme climatic events (tail associations)
# can ever cause a case of no coexistence
res <- readRDS("./results_numeric/rbar_DeltaI.RDS")
list2env(res, globalenv())

flag <- rbar$X1<0 & rbar$X2<0 & rbar$X3>0
example <- cbind(params[flag,], rbar[flag,1:3], DeltaI[flag,], epECdot[flag,])
colnames(example) <- c("sigma","mu1","mu2","delta","rbarl","rbarr","rbars",
                       "DeltaIl","DeltaIr","DeltaIs","epECdotl","epECdotr","epECdots")
round(example,4)

rv <- transform(b_tilde, u_tilde, rho, 6.4, c(0.1,2.0), b_s=T) #makes random vars defined by parameters
list2env(rv, globalenv())
l <- r_mets(b_l1-b_l2, 0.6, M)
r <- r_mets(b_r1-b_r2, 0.6, M)
s <- r_mets(b_s1-b_s2, 0.6, M)

r$rbar_hat;l$rbar_hat;s$rbar_hat



plot(r$r[1:100], type='l', col="grey")
#abline(h=0, lty=3, lwd=0.7)
abline(h=r$rbar_hat)
#lines(r$r[1:100], col=z'grey')
lines(s$r[1:100], col='blue')
abline(h=s$rbar_hat, col='darkblue')

pop<- popsim(b_l1, b_l2, 1000, 1, 0.6, M)
pops <- popsim(b_s1, b_s2, 1000, 1, 0.6, M)

plot_r1(6.4, c(0.1,2.0), 0.6, plottype=2)
plot_r1(6.4, c(0.1,2.0), 0.6, plottype=3)
plot_r1(6.4, c(0.1,2.0), 0.6, plottype=1)


mean(log(pop$N1[2:10001]/pop$N1[1:10000]))
mean(log(pops$N1[2:10001]/pops$N1[1:10000]))


  