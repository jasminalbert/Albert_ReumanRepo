# loop through parameters



delta <- seq(0.1,1,0.1)
mu1 <- seq(.1,.9,.1) 
mu2 <- mu1 
sigma <- c(1.6, 3.2,6.4)

#mu1>mu2 
mu <- c(NA,NA)
for (i in 1:length(mu1)){
  for (j in 1:length(mu2)){
    if(mu1[i]<=mu2[j]){
      mu <- rbind(mu, cbind(mu1[i],mu2[j]))
    }
  }
}
mu <- mu[-1,]
#print(mu) #45x2

M <- 100000
b_tilde <- makenoise(M)
rho <- cor(b_tilde$l)[1,2];rho
u_tilde <- rnorm(M)

rbar <- c(NA,NA,NA,NA,NA)
se_rbar <- rbar
DeltaI <- c(NA,NA,NA)
se_DeltaI <- DeltaI
params <- rep(NA, 4)

for (s in 1:length(sigma)){
  for (m in 1:nrow(mu)){
    #print <- append(print,paste(mu1[b],mu2[b]))
    b_l1 <- sigma[s]*b_tilde$l[,1] + mu[m,1]
    b_l2 <- sigma[s]*b_tilde$l[,2] + mu[m,2]
    
    b_r1 <- sigma[s]*b_tilde$r[,1] + mu[m,1]
    b_r2 <- sigma[s]*b_tilde$r[,2] + mu[m,2]
    
    u_s <- sqrt(2*sigma[s]^2 - 2*rho)*u_tilde + (mu[m,1]-mu[m,2]) 
    u_1 <- sqrt(2*sigma[s]^2)*u_tilde + (mu[m,1]-mu[m,2])
    u_2 <- sqrt(2*sigma[s]^2)*u_tilde
    
    for (d in 1:length(delta)){
      #rbar1
      #left tail (1)
      rbar1hat_l <- mean(log(1-delta[d]+delta[d]*exp(b_l1-b_l2)))
      
      se_rbar1hat_l <- sd(log(1-delta[d]+delta[d]*exp(b_l1-b_l2)))/sqrt(M)
      
      #righ tail (2)
      rbar1hat_r <- mean(log(1-delta[d]+delta[d]*exp(b_r1-b_r2)))
      
      se_rbar1hat_r <- sd(log(1-delta[d]+delta[d]*exp(b_r1-b_r2)))/sqrt(M)
      
      #symmetric (3)
      rbar1hat_s <- mean(log(1-delta[d]+delta[d]*exp(u_s)))
      
      se_rbar1hat_s <- sd(log(1-delta[d]+delta[d]*exp(u_s)))/sqrt(M)
      
      #rbar1sharp (4)
      rbar1sharphat <- mean(log(1-delta[d]+delta[d]*exp(u_1)))
      
      se_rbar1sharphat <- sd(log(1-delta[d]+delta[d]*exp(u_1)))/sqrt(M)
      
      #rbar2sharp (5)
      rbar2sharphat <- mean(log(1-delta[d]+delta[d]*exp(u_2)))
      
      se_rbar2sharphat <- sd(log(1-delta[d]+delta[d]*exp(u_2)))/sqrt(M)   
      
      #DeltaI (6)
      #left
      DeltaIhat_l <- rbar1hat_l - rbar1sharphat + rbar2sharphat
      se_DeltaIhat_l <- se_rbar1hat_l + se_rbar1sharphat + se_rbar2sharphat
      
      #right
      DeltaIhat_r <- rbar1hat_r - rbar1sharphat + rbar2sharphat
      se_DeltaIhat_r <- se_rbar1hat_r + se_rbar1sharphat + se_rbar2sharphat
      
      #symmetric
      DeltaIhat_s <- rbar1hat_s - rbar1sharphat + rbar2sharphat
      se_DeltaIhat_s <- se_rbar1hat_s + se_rbar1sharphat + se_rbar2sharphat
      
      #saving
      rbar <- rbind(rbar, cbind(rbar1hat_l,rbar1hat_r,rbar1hat_s,rbar1sharphat,rbar2sharphat))
      se_rbar <- rbind(se_rbar, cbind(se_rbar1hat_l,se_rbar1hat_r,se_rbar1hat_s,se_rbar1sharphat,se_rbar2sharphat))
      DeltaI <- rbind(DeltaI, cbind(DeltaIhat_l, DeltaIhat_r, DeltaIhat_s))
      se_DeltaI <- rbind(se_DeltaI, cbind(se_DeltaIhat_l, se_DeltaIhat_r, se_DeltaIhat_s))
      params <- rbind(params, cbind(sigma[s],mu[m,1],mu[m,2],delta[d]))
    }
  }
}
res <- list(params=params[-1,], rbar=rbar[-1,], se_rbar=se_rbar[-1,], DeltaI=DeltaI[-1,], se_DeltaI=se_DeltaI[-1,])
lapply(res,head,10)