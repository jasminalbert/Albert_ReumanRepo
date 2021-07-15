load("./params.RData")
source("./decomp_plotting.R")
base <- par()

#pdf("fig3.pdf")

par(mfrow=c(1,1), mar = c(2,2,2,1))

				
layout(matrix(c(1,2,3,4,5,
				6,10,11,12,13,	
				7,14,15,16,17,
				8,18,19,20,21,
				9,22,23,24,25), ncol=5, byrow=TRUE), 
				heights=c(1,2,2,2,2), widths=c(1,2.5,2.5,2.5,2.5))
#layout.show(n=25)
				
plot(0, bty="n")
plot.new() #1

#2-5
for (d in 1:length(delta)){
	plot.new()
	text(4,-0.5,labels=paste(delta[d]))
}

#6-9
for (m in 1:length(mudif)){
	plot.new()
	text(0.5,0,labels=paste(mudif[m]))
}

#10-25
res <- vector(mode='list',length=1)
for (m in 1:length(mudif)){
  for (d in 1:length(delta)){
    res <- append(res,dePlot(mudif[m], delta[d]))
   }
}

#dev.off()
res <- res[-1]


