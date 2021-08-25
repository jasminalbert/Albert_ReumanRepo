source("./decomp2_plotting.R")

delta <- c(1, 0.8, 0.5)
sigma <- c(2, 4 ,5,6)

pdf("results_figs/fig4_legend.pdf")

par(mgp=c(3,0.5,0), mar = c(1,1,1,1), oma=c(3,5,3,2), xpd=TRUE)

layout(matrix(c(2,3,4,1,21,
                9,10,11,5,21,	
                12,13,14,6,21,
                15,16,17,7,21,
                18,19,20,8,21), ncol=5, byrow=TRUE), 
       heights=c(1,3,3,3,3), widths=c(2.5,2.5,2.5,1,2))


plot.new() #1


#2-4
for (d in 1:length(delta)){
  plot.new()
  text(0.5,0.3,labels=paste(delta[d]),font=2)
}

#5-8
for (s in 1:length(sigma)){
  plot.new()
  text(0.3,0.5,labels=paste(sigma[s]), font=2)
}

#9-20
res <- vector(mode='list',length=1)
m <- 1
for (s in 1:length(sigma)){
  for (d in 1:length(delta)){
    res <- append(res,dePlot2(mudif_list[[m]],sigma[s], delta[d], xaxt="n"))
    axis(1, labels=ifelse(m>9, yes=TRUE, no=FALSE), tick=TRUE)
    m <- m+1
  }
}

mtext("contribution to coexistence", side=2, outer=TRUE, line=1.5, font=2, cex=1, at=0.46)
mtext(expression(sigma), side=4, outer=TRUE, line=-8, font=2, cex=1.5, at=0.46)
mtext(expression(delta), side=3, outer=TRUE, line=-0.5, font=2, cex=1.5, at=0.476)
mtext(expression(mu[1]-mu[2]), outer=TRUE, side=1, line=0.5, cex.lab=1.3, at=0.476)

plot.new() #21
legend("topright", 
       legend=c(expression(IGR),expression(IGR-Delta["[E||C]"]), expression(Delta["[E||C]"])),
       col = c("orange","navy", "red"),
       lty = c(1,1,5), bty="n", cex=1.2, inset=c(0,0))

dev.off()

pdf("fig4_qij_legend.pdf")

par(mgp=c(3,0.5,0), mar = c(1,1,1,1), oma=c(3,5,3,2), xpd=TRUE)

layout(matrix(c(2,3,4,1,21,
                9,10,11,5,21,	
                12,13,14,6,21,
                15,16,17,7,21,
                18,19,20,8,21), ncol=5, byrow=TRUE), 
       heights=c(1,3,3,3,3), widths=c(2.5,2.5,2.5,1,2))


plot.new() #1


#2-4
for (d in 1:length(delta)){
  plot.new()
  text(0.5,0.3,labels=paste(delta[d]),font=2)
}

#5-8
for (s in 1:length(sigma)){
  plot.new()
  text(0.3,0.5,labels=paste(sigma[s]), font=2)
}

#9-20
res <- vector(mode='list',length=1)
m <- 1
for (s in 1:length(sigma)){
  for (d in 1:length(delta)){
    res <- append(res,dePlot2(mudif_list[[m]],sigma[s], delta[d], xaxt="n"))
    axis(1, labels=ifelse(m>9, yes=TRUE, no=FALSE), tick=TRUE)
    m <- m+1
  }
}

mtext("contribution to coexistence", side=2, outer=TRUE, line=1.5, font=2, cex=1, at=0.46)
mtext(expression(sigma), side=4, outer=TRUE, line=-8, font=2, cex=1.5, at=0.46)
mtext(expression(delta), side=3, outer=TRUE, line=-0.5, font=2, cex=1.5, at=0.476)
mtext(expression(mu[1]-mu[2]), outer=TRUE, side=1, line=0.5, cex.lab=1.3, at=0.476)

plot.new() #21
legend("topright", 
       legend=c(expression(IGR),expression(IGR-Delta^"[E||C]"), expression(Delta^"[E||C]")),
       col = c("orange","navy", "red"),
       lty = c(1,1,5), bty="n", cex=1.2, inset=c(0,0))

dev.off()