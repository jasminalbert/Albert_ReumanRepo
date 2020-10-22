source("./mean.df.R")

head(mean.df)

seq(1,4455,(4455/5))

plot(mean.df[mean.df[,"sigma"]==6.4,"cn.sym"], ylab = "number of co periods",type = 'l', col = "olivedrab1")
lines(mean.df[mean.df[,"sigma"]==6.4,"cn.ERT"], col = 'royalblue')
lines(mean.df[mean.df[,"sigma"]==6.4,"cn.ELT"], col = 'red')

lines(mean.df[mean.df[,"sigma"]==3.2,"cn.sym"], type = 'l', col = "olivedrab2")
lines(mean.df[mean.df[,"sigma"]==3.2,"cn.ERT"], col = 'royalblue1')
lines(mean.df[mean.df[,"sigma"]==3.2,"cn.ELT"], col = 'red1')

lines(mean.df[mean.df[,"sigma"]==1.6,"cn.sym"], type = 'l', col = "olivedrab3")
lines(mean.df[mean.df[,"sigma"]==1.6,"cn.ERT"], col = 'royalblue2')
lines(mean.df[mean.df[,"sigma"]==1.6,"cn.ELT"], col = 'red2')

lines(mean.df[mean.df[,"sigma"]==0.8,"cn.sym"], type = 'l', col = "olivedrab4")
lines(mean.df[mean.df[,"sigma"]==0.8,"cn.ERT"], col = 'royalblue3')
lines(mean.df[mean.df[,"sigma"]==0.8,"cn.ELT"], col = 'red3')

lines(mean.df[mean.df[,"sigma"]==0.4,"cn.sym"], type = 'l', col = "olivedrab")
lines(mean.df[mean.df[,"sigma"]==0.4,"cn.ERT"], col = 'royalblue4')
lines(mean.df[mean.df[,"sigma"]==0.4,"cn.ELT"], col = 'red4')

abline(v=1, lty=4)
abline(v=11, lty=4)
mtext("mu1=mu2=0.1",3,at=1)

abline(v=111, lty=4)
abline(v=121, lty=4)
mtext("mu1=mu2=0.2",3,at=111)

abline(v=221, lty=4)
abline(v=231, lty=4)
mtext("mu1=mu2=0.3",3,at=221)

abline(v=331, lty=4)
abline(v=341, lty=4)
mtext("mu1=mu2=0.4",3,at=331)

abline(v=441, lty=4)
abline(v=451, lty=4)
mtext("mu1=mu2=0.5",3,at=441)

abline(v=551, lty=4)
abline(v=561, lty=4)
mtext("mu1=mu2=0.6",3,at=551)

abline(v=661, lty=4)
abline(v=671, lty=4)
mtext("mu1=mu2=0.7",3,at=661)

abline(v=771, lty=4)
abline(v=781, lty=4)
mtext("mu1=mu2=0.8",3,at=771)

abline(v=881, lty=4)
abline(v=891, lty=4)
mtext("mu1=mu2=0.9",3,at=881)

mean.df[mean.df[,"mu1"]==mean.df[,"mu2"] & mean.df[,"mu1"]==0.3,]
mean.df[mean.df[,"sigma"]==3.2,]


