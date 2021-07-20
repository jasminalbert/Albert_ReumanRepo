load("./params.RData")
source("./decomp_plotting.R")
base <- par()

pdf("fig3.pdf")

par(mfrow=c(1,1), mar = c(1,2,1,1), oma=c(1.5,3,3,1))

				
layout(matrix(c(1,2,3,4,5,
				6,10,11,12,13,	
				7,14,15,16,17,
				8,18,19,20,21,
				9,22,23,24,25), ncol=5, byrow=TRUE), 
				heights=c(1,3,3,3,3), widths=c(1,2.5,2.5,2.5,2.5))
#layout.show(n=25)
				
#plot(0, bty="n")
plot.new() #1

#2-5
for (d in 1:length(delta)){
	plot.new()
	text(0.5,0.5,labels=paste(delta[d]),font=2)
}

#6-9
for (m in 1:length(mudif)){
	plot.new()
	text(0.3,0.5,labels=paste(mudif[m]), font=2)
}

#10-25
res <- vector(mode='list',length=1)
for (m in 1:length(mudif)){
  for (d in 1:length(delta)){
    res <- append(res,dePlot(mudif[m], delta[d], xaxt="n"))
    axis(1, labels=FALSE, tick=TRUE)
   }
}

title(ylab=expression(mu[1]-mu[2]), outer=TRUE, line=0, font.lab=2, cex.lab=1.5)
title(xlab=expression(delta), outer=TRUE, line=-50, font.lab=2, cex.lab=1.5)
title(xlab=expression(sigma), outer=TRUE, line=0, cex.lab=1.3)

layout(matrix(c(1)))
plot.new() #1
legend("center", 
       legend=c(expression(Delta[0]), expression(Delta[E]),
                expression(Delta[C]), expression(Delta[("E#C")]),
                expression(Delta["[EC]"]), expression(Delta["[E||C]"])
                ,expression(r)), 
       col = c("black","black","black","black","blue","red","orange"),
       lty = c(1,2,4,3,1,1,1), bty="n", cex=2)

dev.off()
res <- res[-1]


