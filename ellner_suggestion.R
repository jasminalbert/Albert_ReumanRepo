#doing ellner suggestions
#plot IGR1 vs sigma 
#plot deltaI vs sigma

res1<-read.csv("generated_data/run3/SEandCoMresults202101.csv")[,-1]

str(res1)

res1 <- res1[,c(1:7,11:13)] #only deltai and rbar1

mu <- c(0.4,0.5)

res <- res1[res1$mu1 == mu[1] & res1$mu2 == mu[2] & res1$delta == 0.5,]

sigma <- 0.4*2^seq(0,4,1)

plot(res$sigma,res$rbar1sym, type='l', col='green', ylim=c(-0.06,1))
lines(res$sigma, res$rbar1ERT, col='blue')
lines(res$sigma, res$rbar1ELT, col='red')

lines(res$sigma,res$Delta_B1sym, col='green', lty=2)
lines(res$sigma, res$Delta_B1ERT, col='blue', lty=2)
lines(res$sigma, res$Delta_B1ELT, col='red', lty=2)
abline(h=0, lty=3, lwd=0.75)

mtext(expression(paste(r_1(t),"=",1-delta + delta*B_1(t)/B_2(t))), side=3, line=2)
mtext(expression(paste(Delta*I_1,"=",bar(r_1) - bar(r_1^"#")+bar(r_2^"#"))), side=3, line=0)

