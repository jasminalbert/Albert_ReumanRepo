# loop through parameters
# computations with q12 expression and not q12=1

load("./params.RData")

load("./noise_etc.RData")

source("./transform2.R")

source("./growth_rate_metrics.R")

noisename <- c("left","right","sym")

rbar <- rep(NA, 5)
names(rbar) <- c(noisename, "sharp1","sharp2")
se_rbar <- rbar
DeltaI <- rep(NA,3)
names(DeltaI) <- noisename
se_DeltaI <- DeltaI
sd_r <- DeltaI
epECdot <- DeltaI
invPrb <- DeltaI
invE <- DeltaI
q12 <- DeltaI
params <- rep(NA, 4)
names(params) <- c("sigma","mu1","mu2","delta")

for (g in 1:length(sigma)){
  for (m in 1:length(mudif)){
    
    rv <- transform(b_tilde, u_tilde, rho, sigma[g], mudif[m], b_s=TRUE) #makes random vars defined by parameters
    list2env(rv, globalenv())
    
    #probability r1>0
    invPrb_l <- length(b_l1[b_l1>b_l2])/M
    invPrb_r <- length(b_r1[b_r1>b_r2])/M
    invPrb_s <- length(u_s[u_s>0])/M
    
    for (d in 1:length(delta)){
      #rbar1
      #left tail (1)
      l <- r_mets(b_l1-b_l2, delta[d], M)
      
      invE_l <- mean(l$r[l$r>0])
      
      q12_l <- mean(exp(b_l1))/((1-delta[d])*mean(exp(b_l2))+delta[d]*mean(exp(b_l1)))
      
      #righ tail (2)
      r <- r_mets(b_r1-b_r2, delta[d], M)
      
      invE_r <- mean(r$r[r$r>0])
      
      q12_r <- mean(exp(b_r1))/((1-delta[d])*mean(exp(b_r2))+delta[d]*mean(exp(b_r1)))
      
      #symmetric (3)
      s <- r_mets(u_s, delta[d], M)
      
      invE_s <- mean(s$r[s$r>0])
      
      q12_s <- mean(exp(b_s1))/((1-delta[d])*mean(exp(b_s2))+delta[d]*mean(exp(b_s1)))
      
      #rbar1sharp (4)
      sharp1 <- r_mets(u_1, delta[d], M)
      
      #rbar2sharp (5)
      sharp2 <- r_mets(u_2, delta[d], M)
      
      #DeltaI (6)
      #left
      DeltaIhat_l <- l$rbar_hat - sharp1$rbar_hat + q12_l*sharp2$rbar_hat
      se_DeltaIhat_l <- l$se_r + sharp1$se_r + sharp2$se_r
      
      #right
      DeltaIhat_r <- r$rbar_hat - sharp1$rbar_hat + q12_r*sharp2$rbar_hat
      se_DeltaIhat_r <- r$se_r + sharp1$se_r + sharp2$se_r
      
      #symmetric
      DeltaIhat_s <- s$rbar_hat - sharp1$rbar_hat + q12_s*sharp2$rbar_hat
      se_DeltaIhat_s <- s$se_r + sharp1$se_r + sharp2$se_r
      
      #Contribution of asymmetry (7)
      #left
      epECdot_l <- DeltaIhat_l - DeltaIhat_s
      
      #right
      epECdot_r <- DeltaIhat_r - DeltaIhat_s
      
      #symmetric
      epECdot_s <- DeltaIhat_s - DeltaIhat_s
      
      #save in matrix
      rbar <- rbind(rbar, c(l$rbar_hat,r$rbar_hat,s$rbar_hat,sharp1$rbar_hat,sharp2$rbar_hat))
      se_rbar <- rbind(se_rbar, c(l$se_r, r$se_r, s$se_r, sharp1$se_r, sharp2$se_r))
      sd_r <- rbind(sd_r, c(l$sd_r, r$sd_r, s$sd_r))
      q12 <- rbind(q12, c(q12_l, q12_r, q12_s))
      DeltaI <- rbind(DeltaI, c(DeltaIhat_l, DeltaIhat_r, DeltaIhat_s))
      se_DeltaI <- rbind(se_DeltaI, c(se_DeltaIhat_l, se_DeltaIhat_r, se_DeltaIhat_s))
      epECdot <- rbind(epECdot, c(epECdot_l, epECdot_r, epECdot_s))
      invPrb <- rbind(invPrb, c(invPrb_l, invPrb_r, invPrb_s))
      invE <- rbind(invE, c(invE_l, invE_r, invE_s))
      params <- rbind(params, cbind(sigma[g],mudif[m],delta[d]))
    }
  }
}
resq <- list(params=params[-1,], invPrb=invPrb[-1,], invE=invE[-1,], rbar=rbar[-1,], se_rbar=se_rbar[-1,], sd_r=sd_r[-1,], DeltaI=DeltaI[-1,], se_DeltaI=se_DeltaI[-1,], epECdot=epECdot[-1,])
resq <- lapply(resq, data.frame)
lapply(resq,head,10)

#save

if (dir.exists("./results_numeric")==FALSE){
  dir.create("./results_numeric")
}

resloc <- "./results_numeric/"

saveRDS(resq, paste(resloc,"rbar_DeltaIwq.RDS", sep=""))
#saveRDS(res, "./results_numeric/rbar_DeltaI.RDS")
