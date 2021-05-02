source("./makenoise.R")

b <- makenoise(1000)

x<-cor(b$b_l)[1,2]
y<-cor(b$b_s)[1,2]
z<-cor(b$b_r)[1,2]
r <- c(x,y,z)



png("noise_figure.png", 800,325, res=110)
par(mar=c(5,4,4,0), oma=c(1,2,1,2))
layout(matrix(c(1,2,3,1,2,3,1,2,3),nrow=3,byrow=T))
plot(b$b_l[,1],b$b_l[,2], col="darkblue", ylab='b_1', xlab=NA, xlim=c(-4,4), ylim=c(-4,4),main='Left-tailed', cex.lab=1.5)
plot(b$b_s[,1],b$b_s[,2], col="darkblue", xlab='b_2', ylab=NA, xlim=c(-4,4), ylim=c(-4,4),main='Symmetric',cex.lab=1.5)
plot(b$b_r[,1],b$b_r[,2], col="darkblue", xlab=NA, ylab=NA, xlim=c(-4,4), ylim=c(-4,4),main='Right-tailed',cex.lab=1.5)
#mtext(expression(rho==0.8258437), 1, outer=TRUE)
dev.off()


