source("./decomp2_plotting.R")

delta <- c(1, 0.8, 0.5)
sigma <- c(2, 4 ,6)

par(mfrow=c(3,3))

res <- vector(mode='list',length=1)
for (s in 1:length(sigma)){
  for (d in 1:length(delta)){
    res <- append(res,dePlot2(sigma[s], delta[d], xaxt="n"))
    axis(1, labels=FALSE, tick=TRUE)
  }
}