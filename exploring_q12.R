source("./makenoise.R") 
load("./noise_etc.RData")
m1 <- 0.3; m2 <- 0.9; sig <- 6.4

rv <- transform(b_tilde, u_tilde, rho, sig, c(m1,m2), b_s=TRUE)
list2env(rv, globalenv())
d <- 0.4
#bar B
q12_l <- mean(exp(b_l1))/((1-d)*mean(exp(b_l2))+d*mean(exp(b_l1)))

q12_r <- mean(exp(b_r1))/((1-d)*mean(exp(b_r2))+d*mean(exp(b_r1)))

q12_s <- mean(exp(b_s1))/((1-d)*mean(exp(b_s2))+d*mean(exp(b_s1)))
B<-c(q12_l, q12_r, q12_s)

#exp(bar b + sd(b)^2/2)
q12_l <- exp(mean(b_l1)+var(b_l1)/2)/((1-d)*exp(mean(b_l2)+var(b_l2)/2)+d*exp(mean(b_l1)+var(b_l1)/2))

q12_r <- exp(mean(b_r1)+var(b_r1)/2)/((1-d)*exp(mean(b_r2)+var(b_r2)/2)+d*exp(mean(b_r1)+var(b_r1)/2))

q12_s <- exp(mean(b_s1)+var(b_s1)/2)/((1-d)*exp(mean(b_s2)+var(b_s2)/2)+d*exp(mean(b_s1)+var(b_s1)/2))
b<-c(q12_l, q12_r, q12_s)

#exp(mu + sig^2/2)
q12_l <- exp(m1+(sig^2)/2)/((1-d)*exp(m2+(sig^2)/2)+d*exp(m1+(sig^2)/2))

q12_r <- exp(m1+(sig^2)/2)/((1-d)*exp(m2+(sig^2)/2)+d*exp(m1+(sig^2)/2))

q12_s <- exp(m1+(sig^2)/2)/((1-d)*exp(m2+(sig^2)/2)+d*exp(m1+(sig^2)/2))
mu<-c(q12_l, q12_r, q12_s)

rbind(B,b,mu)


#str(res)
#round(head(cbind(res$params,res$DeltaI, resq$DeltaI)),3)
#round(head(cbind(res$params,res$epECdot[,1:2], resq$epECdot[,1:2]),100),3)
