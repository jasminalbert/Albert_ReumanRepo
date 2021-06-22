#estimate rbar1
  #left
b1 <- sigma*b1_tilde + mu1
b2 <- sigma*b2_tilde + mu2
rbar1hat <- mean(log(1-delta+delta*exp(b1-b2)))
se_rbar1hat = sd(log(1-delta+delta*exp(b1-b2)))/sqrt(M)

  #right
b1 <- sigma*b1_tilde + mu1
b2 <- sigma*b2_tilde + mu2
rbar1hat <- mean(log(1-delta+delta*exp(b1-b2)))
se_rbar1hat = sd(log(1-delta+delta*exp(b1-b2)))/sqrt(M)

  #symmetric
u <- sqrt((2*sigma^2 - 2*rho)*u_tilde)+mu1-mu2
rbar1hat <- mean(log(1-delta+delta*exp(u)))
se_rbar1hat <- sd(log(1-delta+delta*exp(u)))/sqrt(M)


#rbar1sharp 

rbar1sharphat <- mean(log(1-delta+delta*exp(u)))






