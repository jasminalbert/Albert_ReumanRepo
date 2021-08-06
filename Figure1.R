source("./makenoise.R")




P <- c(cor(b_tilde$l, method='pearson')[1,2], cor(b_tilde$s, method='pearson')[1,2], cor(b_tilde$r, method='pearson')[1,2])


S <- c(cor(b_tilde$l, method='spearman')[1,2],cor(b_tilde$s, method='spearman')[1,2],cor(b_tilde$r, method='spearman')[1,2])

col2rgb("darkblue")
blue <- rgb(0,0,0.545,0.3)

pdf("fig1.pdf")
png("fig1.png", height=560, width=1600,pointsize=22)
par(mar=c(0.5,0.5,0,0), oma=c(4,4,4,2))
layout(matrix(c(4,10,10,6,11,11,8,12,
                1,5 ,13,2,7 ,14,3,9 ),nrow=2,byrow=T),
       heights=c(0.3,1), widths=c(1,0.25,0.25,1,0.25,0.25,1,0.25))
#1-3
plot(b_tilde$l[1:1000,1],b_tilde$l[1:1000,2], col=blue, ylab=NA, xlab=NA, xlim=c(-4,4), ylim=c(-4,4), pch=16, cex=1.5)
text(x=4,y=-2,labels=paste("P = ", round(P[1],4)), adj=1)
text(x=4,y=-2.5,labels=paste("S = ", round(S[1],4)), adj=1)
plot(b_tilde$s[1:1000,1],b_tilde$s[1:1000,2], col=blue, xlab=NA, ylab=NA, xlim=c(-4,4), ylim=c(-4,4), pch=16, cex=1.5)
text(x=4,y=-2,labels=paste("P = ", round(P[2],4)), adj=1)
text(x=4,y=-2.5,labels=paste("S = ", round(S[2],4)), adj=1)
plot(b_tilde$r[1:1000,1],b_tilde$r[1:1000,2], col=blue, xlab=NA, ylab=NA, xlim=c(-4,4), ylim=c(-4,4), pch=16, cex=1.5)
text(x=4,y=-2,labels=paste("P = ", round(P[3],4)), adj=1)
text(x=4,y=-2.5,labels=paste("S = ", round(S[3],4)), adj=1)
title(xlab=expression(var[1]), outer=TRUE, line=2.2, cex.lab=1.5)
title(ylab=expression(var[2]), outer=TRUE, line=2.5, cex.lab=1.5)
#mtext(expression(rho==0.8258437), 1, outer=TRUE)

#marginals
#4-9
d <- density(b_tilde$l[,1])
plot(d$x, d$y, xlab=NA, ylab=NA, sub=NA, main=NA, bty='n',type='l', xaxt='n', yaxt='n',
     ylim=c(0,max(d$y)*1.25))
title(main='Left-tailed', line=-1, cex.main=1.5)
d <- density(b_tilde$l[,2])
plot(d$y, d$x, xlab=NA, ylab=NA, sub=NA, main=NA, bty='n', type='l',xaxt='n', yaxt='n')

d <- density(b_tilde$s[,1])
plot(d$x, d$y, xlab=NA, ylab=NA, sub=NA, main=NA, bty='n',type='l', xaxt='n', yaxt='n',
     ylim=c(0,max(d$y)*1.25))
title(main='Symmetric', line=-1, cex.main=1.5)
d <- density(b_tilde$s[,2])
plot(d$y, d$x, xlab=NA, ylab=NA, sub=NA, main=NA, bty='n', type='l',xaxt='n', yaxt='n')

d <- density(b_tilde$r[,1])
plot(d$x, d$y, xlab=NA, ylab=NA, sub=NA, main=NA, bty='n',type='l', xaxt='n', yaxt='n',
     ylim=c(0,max(d$y)*1.25))
title(main='Right-tailed', line=-1, cex.main=1.5)
d <- density(b_tilde$r[,2])
plot(d$y, d$x, xlab=NA, ylab=NA, sub=NA, main=NA, bty='n', type='l',xaxt='n', yaxt='n')

#title(main='Left-tailed Symmetric Right-tailed', outer=TRUE)

dev.off()





png("noise_figure.png", 800,325, res=110)
par(mar=c(5,4.5,4,0), oma=c(1,2,1,2))
layout(matrix(c(1,2,3,
                1,2,3,
                1,2,3),nrow=3,byrow=T))
plot(b_tilde$l[,1],b_tilde$l[,2], col="darkblue", ylab=NA, xlab=NA, xlim=c(-4,4), ylim=c(-4,4),main='Left-tailed', cex.lab=1.5)
plot(b_tilde$s[,1],b_tilde$s[,2], col="darkblue", xlab=NA, ylab=NA, xlim=c(-4,4), ylim=c(-4,4),main='Symmetric',cex.lab=1.5)
plot(b_tilde$r[,1],b_tilde$r[,2], col="darkblue", xlab=NA, ylab=NA, xlim=c(-4,4), ylim=c(-4,4),main='Right-tailed',cex.lab=1.5)
title(xlab=expression(var[1]), outer=TRUE, line=-1)
title(ylab=expression(var[2]), outer=TRUE, line=-1)
#mtext(expression(rho==0.8258437), 1, outer=TRUE)

dev.off()







