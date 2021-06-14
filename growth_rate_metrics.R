#growth rate metrics


r_mets <- function(u, delta, M){
  
  r <- log(1-delta+delta*exp(u))
  rbar_hat <- mean(r)
  sd_r <- sd(r)
  se_r <- sd_r/M
  
  return(list(r=r, rbar_hat=rbar_hat, sd_r=sd_r, se_r=se_r))
  
}