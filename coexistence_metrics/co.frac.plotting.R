source("./mean.df.R")

head(mean.df)

sigma <- c(.4,.8,1.6, 3.2,6.4)

olive <- rev(c("olivedrab1","olivedrab2","olivedrab3","olivedrab4","olivedrab"))

blue <- rev(c('royalblue', 'royalblue1','royalblue2','royalblue3','royalblue4'))

red<-c('red4','red3','red2','red1','red')

plot(mean.df[mean.df[,"sigma"]==6.4,"cf.sym"], ylab = "coexistence fraction", ylim = c(0,.4),type = 'l', col = "olivedrab1")

for (i in 4:1){
	lines(mean.df[mean.df[,"sigma"]==sigma[i],"cf.sym"],col = olive[i])
}

for (i in 1:5){
	lines(mean.df[mean.df[,"sigma"]==sigma[i],"cf.ERT"],col = blue[i])
}

for (i in 1:5){
	lines(mean.df[mean.df[,"sigma"]==sigma[i],"cf.ELT"],col = red[i])
}

