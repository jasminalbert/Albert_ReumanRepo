# loop through parameters

load("./params.RData")

load("./noise_etc.RData")

source("./r_function.R")

rbar <- rep(NA, 5)
se_rbar <- rbar
DeltaI <- rep(NA,3)
se_DeltaI <- DeltaI
sd_r <- DeltaI
epECdot <- DeltaI
params <- rep(NA, 4)

for (s in 1:length(sigma)){
  for (m in 1:nrow(mu)){

    b_l1 <- sigma[s]*b_tilde$l[,1] + mu[m,1]
    b_l2 <- sigma[s]*b_tilde$l[,2] + mu[m,2]
    
    b_r1 <- sigma[s]*b_tilde$r[,1] + mu[m,1]
    b_r2 <- sigma[s]*b_tilde$r[,2] + mu[m,2]
    
    b_s1 <- sigma[s]*b_tilde$r[,1] + mu[m,1]
    b_s2 <- sigma[s]*b_tilde$r[,2] + mu[m,2]
    
    u_s <- sqrt(2*sigma[s]^2 - 2*rho*sigma^2)*u_tilde + (mu[m,1]-mu[m,2]) 
    u_1 <- sqrt(2*sigma[s]^2)*u_tilde + (mu[m,1]-mu[m,2])
    u_2 <- sqrt(2*sigma[s]^2)*u_tilde
    
    for (d in 1:length(delta)){
      #rbar1
      #left tail (1)
      r1_l <- r(b_l1-b_l2, delta[d])
      rbar1hat_l <- mean(r1_l)
      
      sd_r1_l <- sd(r1_l)
      se_rbar1hat_l <- sd_r1_l/sqrt(M)
      
      #righ tail (2)
      r1_r <- r(b_r1-b_r2, delta[d])
      rbar1hat_r <- mean(r1_r)
      
      sd_r1_r <- sd(r1_r)
      se_rbar1hat_r <- sd_r1_r/sqrt(M)
      
      #symmetric (3)
      r1_s <- r(u_s, delta[d])
      rbar1hat_s <- mean(r1_s)
      
      sd_r1_s <- sd(r1_s)
      se_rbar1hat_s <- sd_r1_s/sqrt(M)
      
      #rbar1sharp (4)
      r1_sharp <- r(u_1, delta[d])
      rbar1sharphat <- mean(r1_sharp)
      
      sd_r1sharp <- sd(r1_sharp)
      se_rbar1sharphat <- sd_r1sharp/sqrt(M)
      
      #rbar2sharp (5)
      r2_sharp <- r(u_2, delta[d])
      rbar2sharphat <- mean(r2_sharp)
      
      sd_r2sharp <- sd(r2_sharp)
      se_rbar2sharphat <- sd_r2sharp/sqrt(M)   
      
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
      
      #Contribution of asymmetry (7)
      #left
      epECdot_l <- DeltaIhat_l - DeltaIhat_s
      
      #right
      epECdot_r <- DeltaIhat_r - DeltaIhat_s
      
      #symmetric
      epECdot_s <- DeltaIhat_s - DeltaIhat_s
      
      #save in matrix
      rbar <- rbind(rbar, cbind(rbar1hat_l,rbar1hat_r,rbar1hat_s,rbar1sharphat,rbar2sharphat))
      se_rbar <- rbind(se_rbar, cbind(se_rbar1hat_l,se_rbar1hat_r,se_rbar1hat_s,se_rbar1sharphat,se_rbar2sharphat))
      sd_r <- rbind(sd_r, cbind(sd_r1_l, sd_r1_r, sd_r1_s))
      DeltaI <- rbind(DeltaI, cbind(DeltaIhat_l, DeltaIhat_r, DeltaIhat_s))
      se_DeltaI <- rbind(se_DeltaI, cbind(se_DeltaIhat_l, se_DeltaIhat_r, se_DeltaIhat_s))
      epECdot <- rbind(epECdot, cbind(epECdot_l, epECdot_r, epECdot_s))
      params <- rbind(params, cbind(sigma[s],mu[m,1],mu[m,2],delta[d]))
    }
  }
}
res <- list(params=params[-1,], rbar=rbar[-1,], se_rbar=se_rbar[-1,], sd_r=sd_r[-1,], DeltaI=DeltaI[-1,], se_DeltaI=se_DeltaI[-1,], epECdot=epECdot[-1,])
res <- lapply(res, data.frame)
lapply(res,head,10)

#save

if (dir.exists("./results_numeric")==FALSE){
  dir.create("./results_numeric")
}

resloc <- "./results_numeric/"

saveRDS(res, paste(resloc,"rbar_DeltaI.RDS"))
