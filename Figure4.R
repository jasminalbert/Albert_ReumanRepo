source("./decomp2_plotting.R")

delta <- c(1, 0.8, 0.5)
sigma <- c(2, 4 ,5,6)

pdf("fig4.pdf")

par(mfrow=c(1,1), mar = c(1,2,1,1), oma=c(3,2,3,1))

layout(matrix(c(1,2,3,4,
                5,9,10,11,	
                6,12,13,14,
                7,15,16,17,
                8,18,19,20), ncol=4, byrow=TRUE), 
       heights=c(1,3,3,3,3), widths=c(1,2.5,2.5,2.5))


plot.new() #1
legend("center", 
       legend=c(expression(r), expression(Delta["[E||C]"])
                ,expression(r-Delta["[E||C]"])), 
       col = c("orange","red","navy"),
       lty = c(1,1,5), bty="n", cex=0.6)

#2-4
for (d in 1:length(delta)){
  plot.new()
  text(0.5,0.3,labels=paste(delta[d]),font=2)
}

#5-8
for (s in 1:length(sigma)){
  plot.new()
  text(0.8,0.5,labels=paste(sigma[s]), font=2)
}

#9-20
res <- vector(mode='list',length=1)
m <- 1
for (s in 1:length(sigma)){
  for (d in 1:length(delta)){
    res <- append(res,dePlot2(mudif_list[[m]],sigma[s], delta[d]))
    m <- m+1
  }
}


title(ylab=expression(sigma), outer=TRUE, line=-2.5, font.lab=2, cex.lab=1.5)
title(xlab=expression(delta), outer=TRUE, line=-48, font.lab=2, cex.lab=1.5)
title(xlab=expression(mu[1]-mu[2]), outer=TRUE, line=1.4, cex.lab=1.3)



dev.off()
