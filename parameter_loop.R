# loop through parameters

#mu1>mu2 

delta <- seq(0,1,0.1)
mu1 <- seq(.1,.9,.1) 
mu2 <- mu1 
sigma <- c(.4,.8,1.6, 3.2,6.4)

tilde <- makenoise(100)


for (i in 1:length(sigma)){
  for (j in 1:length(mu1)){
    for (k in 1:length(mu2)){
      b_l1 <- sigma[i]*tilde$b_l[,1] + mu1[j]
    }
  }
}