load("./params.RData")
source("./decomp_plotting.R")
base <- par()

pdf("fig3.pdf")

par(mfrow=c(4,4))

res <- vector(mode='list',length=1)
for (m in 1:length(mudif)){
  for (d in 1:length(delta)){
    res <- append(res,dePlot(mudif[m], delta[d]))
   }
}

dev.off()
res <- res[-1]