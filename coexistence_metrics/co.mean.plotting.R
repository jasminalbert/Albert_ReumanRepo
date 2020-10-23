source("./mean.df.R")

head(mean.df)

plot(mean.df[mean.df[,"sigma"]==6.4,"cm.sym"], ylab = "mean of co period length",ylim=c(0,75),type = 'l', col = "olivedrab1")
lines(mean.df[mean.df[,"sigma"]==6.4,"cm.ERT"], col = 'royalblue')
lines(mean.df[mean.df[,"sigma"]==6.4,"cm.ELT"], col = 'red')

lines(mean.df[mean.df[,"sigma"]==3.2,"cm.sym"], type = 'l', col = "olivedrab2")
lines(mean.df[mean.df[,"sigma"]==3.2,"cm.ERT"], col = 'royalblue1')
lines(mean.df[mean.df[,"sigma"]==3.2,"cm.ELT"], col = 'red1')

lines(mean.df[mean.df[,"sigma"]==1.6,"cm.sym"], type = 'l', col = "olivedrab3")
lines(mean.df[mean.df[,"sigma"]==1.6,"cm.ERT"], col = 'royalblue2')
lines(mean.df[mean.df[,"sigma"]==1.6,"cm.ELT"], col = 'red2')

lines(mean.df[mean.df[,"sigma"]==0.8,"cm.sym"], type = 'l', col = "olivedrab4")
lines(mean.df[mean.df[,"sigma"]==0.8,"cm.ERT"], col = 'royalblue3')
lines(mean.df[mean.df[,"sigma"]==0.8,"cm.ELT"], col = 'red3')

lines(mean.df[mean.df[,"sigma"]==0.4,"cm.sym"], type = 'l', col = "olivedrab")
lines(mean.df[mean.df[,"sigma"]==0.4,"cm.ERT"], col = 'royalblue4')
lines(mean.df[mean.df[,"sigma"]==0.4,"cm.ELT"], col = 'red4')