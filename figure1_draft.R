source("./plot_paramDelta.R")

abc <- mean_df[mean_df$mdif ==0,]

abc <- abc[,-c(8,9,10)]
head(abc)
abc[abc$sym > 0 & abc$ELT<0 & abc$ERT<0, ]
abc[abc$sym < 0 & abc$ELT>0 & abc$ERT>0, ]


lapply(dflist, function(x){print(x[x$sym>0 & x$ERT<0 & x$ELT<0 & x$mn1 - x$mn2 ==0,-c(8,9,10)])})

pdf("figure2.pdf")

par(mar=c(5,3,2,0.8), mgp = c(3,1,0), mfrow = c(1,2), oma = c(6,2,6,1))

#sym = extinct
plot_paramDelta(sdev=6.4, mn1=.8, mn2=0.8, list=dflist, mean_df=mean_df, range = c(-0.02,.04), xlim = c(.99,1), xaxt ="n", yaxt = "n",cex.lab=1.5, cex.main=1.2,main=expression(paste(sigma, "=",6.4,", ", mu,"1=",0.8,", ", mu,"2=", 0.8)))
axis(1, cex.axis=1.2)
axis(2, cex.axis=1.2)
abline(h=0, lty=3)

#TASS = extinct 
plot_paramDelta(sdev=1.6, mn1=.1, mn2=0.1, list=dflist, mean_df=mean_df, range = c(-0.005,.01), xlim = c(.99,1), xaxt ="n", yaxt = "n",cex.lab=1.5, cex.main=1.2,main=expression(paste(sigma, "=",1.6,", ", mu,"1=",0.1,", ", mu,"2=", 0.1)))
axis(1, cex.axis=1.2)
axis(2, cex.axis=1.2)
abline(h=0, lty=3)

title(main = expression(paste("Storage effect according to changes in ",delta)),cex.main=1.5, outer = T, line=1)
legend("topright", legend = c("extreme left tail", "symmetric", "extreme right tail"), fill = c("red", "green", "blue"), box.lty =0, cex = 1.1)

mtext("SE", 2, outer = T, line =0.5, cex = 1.25)
dev.off()


