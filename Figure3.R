load("./params.RData")
load("./noise_etc.RData")
source("./decomp1_plotting.R")


pdf("fig3_legend.pdf")

par(mgp=c(3,0.5,0), mar = c(1,1,1,1), oma=c(3,5,3,2), xpd=TRUE)


layout(matrix(c(2,3,4,5,1,26,
                10,11,12,13,6,26,	
                14,15,16,17,7,26,
                18,19,20,21,8,26,
                22,23,24,25,9,26), ncol=6, byrow=TRUE), 
       heights=c(1,3,3,3,3), widths=c(2.5,2.5,2.5,2.5,1.5,2))
#layout.show(n=25)

#plot(0, bty="n")
plot.new() #1

#2-5
for (d in 1:length(delta)){
  plot.new()
  text(0.5,0.3,labels=paste(delta[d]),font=2)
}

#6-9
for (m in 1:length(mudif)){
  plot.new()
  text(0.3,0.5,labels=paste(mudif[m]), font=2)
}

#10-25
res <- vector(mode='list',length=1)
n <- 1
for (m in 1:length(mudif)){
  for (d in 1:length(delta)){
    res <- append(res,dePlot1(mudif[m], delta[d], xaxt="n"))
    axis(1, labels=ifelse(n>12, yes=TRUE, no=FALSE), tick=TRUE)
    n <- n+1
  }
}

mtext("contribution to coexistence", side=2, outer=TRUE, line=1.5, font=2, cex=1, at=0.46)
mtext(expression(mu[1]-mu[2]), side=4, outer=TRUE, line=-8, font=2, cex=1.5, at=0.46)
mtext(expression(delta), side=3, outer=TRUE, line=-0.5, font=2, cex=1.5, at=0.37)
mtext(expression(sigma), outer=TRUE, side=1, line=0.5, cex.lab=1.3, at=0.37)

#layout(matrix(c(1)))
plot.new() #1
legend("topright", 
       legend=c(expression(Delta[0]), expression(Delta[E]),
                expression(Delta[C]), expression(Delta[("E#C")]),
                expression(Delta["[EC]"]), expression(Delta["[E||C]"])
                ,expression(r)), 
       col = c("black","black","black","black","blue","red","orange"),
       lty = c(1,2,4,3,1,1,1), bty="n", cex=1.5, inset=c(0,0))

dev.off()
#res <- res[-1]


