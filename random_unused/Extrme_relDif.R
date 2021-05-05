#trying to find relative difference of extreme tails to symmetric 
#ERT-ELT/(sym - mean(ERT,ELT))
#difference between tail direction relative to difference between symmetric and avg of tailed noise

#useful? maybe no if already have anova

Edif <- matrix(NA, ncol = length(dflist), nrow = nrow(simdfx))

for (i in 1:length(dflist)){
	dflist[[i]]$Emean <- rowMeans(cbind(dflist[[i]]$ERT, dflist[[i]]$ELT))
	dflist[[i]]$Edif <-abs(dflist[[i]]$ERT - dflist[[i]]$ELT)/abs(dflist[[i]]$sym - dflist[[i]]$Emean)
	
	Edif[,i] <- dflist[[i]]$Edif
	
}

mean_df$Edif <- rowMeans(Edif)
#----------------------------------------------------


head(mean_df)

plot(mean_df$sdev, mean_df$Edif)

hist(mean_df$Edif,500)

b <-mean_df[which(mean_df$Edif==max(mean_df$Edif, na.rm = TRUE)),]
mean(c(b$ELT, b$ERT))
(b$ERT-b$ELT)/abs(b$sym-mean(c(b$ELT, b$ERT)))
