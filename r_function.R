#growth rate function
#u can be subbed with b1-b2
r <- function(u, delta){
  r <- log(1-delta+delta*exp(u))
  return(r)
}