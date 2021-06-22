source("./makenoise_fxn.R")

M <- 100000
b_tilde <- makenoise(M)
u_tilde <- rnorm(M)

rho <- cor(b_tilde$l)[1,2]

cat("M=",M, " rho=", rho)

save(M,b_tilde,u_tilde,rho, file = "noise_etc.RData")