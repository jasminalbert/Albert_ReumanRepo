library(tidyverse)

gg_df<-gather(mean_df, key = "noise", value="SE", sym, ERT, ELT)

aovDF <- gg_df[gg_df$noise != "sym",]

aov <- aov(SE ~ noise, data=aovDF)
summary(aov)
str(aov)

sim <- lapply(dflist, function(x){x[,-c(8,9,10)]})
sim<-unlist(sim)
head(sim,50)
sim<-matrix(sim, nrow=4455)
sim<-rbind(sim[,1:7], sim[,8:14], sim[,15:21], sim[,22:28], sim[,29:35], sim[,36:42], sim[,43:49], sim[,50:56], sim[,57:63], sim[,64:70])

sim<- data.frame(sim)

names(sim)<- c("sdev","mn1","mn2","delta","ELT","sym","ERT")
ncol(sim)

sim <- sim[order(sim$sdev,sim$mn1,sim$mn2,sim$delta),]

sim<-sim[-c(1:10),]

simdf <- gather(sim, key = "noise", value="SE", sym, ERT, ELT)


simdf <- simdf[order(simdf$sdev,simdf$mn1,simdf$mn2,simdf$delta),]
head(simdf,50)

simdf <- simdf[simdf$noise != "sym",]
simdf <- simdf[simdf$delta != 0,]

aov <- aov(SE ~ noise, data=simdf[501:520,])
summary(aov)
summary(aov)[[1]][["Pr(>F)"]][[1]]


aovlist <- vector(mode = 'list', length = 4050)
for(i in 1:4050) {
	aov <- aov(SE ~ noise, data=simdf[(i*20-19):(i*20),])
	aovlist[[i]] <- summary(aov)[[1]][["Pr(>F)"]][[1]]

}

aov <- unlist(aovlist)

hist(aov,30)

aov[aov < 0.05]

length(aov[aov < 0.05])



i <- 45
simdf[(i*20-19):(i*20),]

simdf$SE[(i*20-19):(i*20)] ==0






