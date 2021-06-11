# transform noise by sigma and mu to make all the noise needed to compute DeltaI and related quantities

#b_tilde - 2 sets bivariate noise 
#u_tilde - univariate noise
#rho - common correlation of bivariate noise sets
#sigma - common sd of bivariate noise sets
#mu - vector length 2 for bivariate noise

transform <- function
.(b_tilde, u_tilde, rho, sigma, mu){
  b_l1 <- sigma*b_tilde$l[,1] + mu[1]
  b_l2 <- sigma*b_tilde$l[,2] + mu[2]
  
  b_r1 <- sigma*b_tilde$r[,1] + mu[1]
  b_r2 <- sigma*b_tilde$r[,2] + mu[2]
  
  u_s <- sqrt(2*sigma^2 - 2*rho*sigma^2)*u_tilde + (mu[1]-mu[2]) 
  u_1 <- sqrt(2*sigma[s]^2)*u_tilde + (mu[m,1]-mu[m,2])
  u_2 <- sqrt(2*sigma[s]^2)*u_tilde  
  
  return(list(b_l1=b_l1, b_l2=b_l2, b_r1=b_r1, u_s=u_s, u_1=u_1, u_2=u_2))
}




