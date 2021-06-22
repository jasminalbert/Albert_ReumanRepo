library(RColorBrewer)
res <- readRDS("./results_numeric/rbar_DeltaI.RDS")
list2env(res, globalenv())

#invasion events and invasion magnitude - positively correlated
plot(invPrb$X1, invE$X1)
points(invPrb$X1[1:90], invE$X1[1:90], col=rainbow(10))
points(invPrb$X1[91:100], invE$X1[91:100]) #values are repeated for same delta
plot(invPrb$X2, invE$X2)
plot(invPrb$X3, invE$X3)



invE$X1[params$X2-params$X3==-0.1 & params$X4==0.1 & params$X1==1.6 ]
# mean values do not matter only the difference
# different values but same difference make the same invE value 


#plot invPrb vs DeltaI
plot(invPrb$X1, DeltaI$X1)
plot(invPrb$X2, DeltaI$X2)
plot(invPrb$X3, DeltaI$X3, col=rainbow(10)) 

#look into one sigma to get closer look
params$mudif <- params$X2-params$X3
mufac <- as.factor(params$mudif)
d <- as.factor(params$X4)
rb <- rainbow(10)
lvl <- as.character(8:0)

plot(0, xlim=c(0.2,0.5), ylim=c(-0.1, 1.1), ylab="DeltaI", xlab="invPrb")
for (i in 1:length(sigma)){
  points(invPrb$X3[params$X1==sigma[i]], DeltaI$X3[params$X1==sigma[i]], 
         col=rainbow(10), pch=lvl[mufac])
}
legend("topleft", legend=levels(d), fill=rainbow(10), cex=.5, bty='n', border="white")
#invasion probability always decreases with larger mean differences #makes sense
#larger mean differences = weaker invader = invader less likely to invade
#sometimes deltaI and invPrb positive sometimes negative correlated, depends on delta?
#why

#split by delta
col <- brewer.pal(9, "YlOrRd") #colors for mu dif
ncol <- c("green","red","blue") #noise colors

pdf("DeltaI_invPrb.pdf")
for (i in 1:length(delta)){
  
  flag1 <- params$X4==delta[i]
  plot(0, xlab=expression(paste(Delta,"I")), ylab="P[r1>0]", 
        xlim= c( min(DeltaI$X2[flag1]), max(DeltaI$X3[flag1]) ),
        ylim = c( min(invPrb$X2[flag1]), max(invPrb$X3[flag1]) ))
  
  for(n in 1:3){
    for (j in 1:length(sigma)){
      
      flag2 <- params$X1==sigma[j] & params$X4==delta[i]
      mufac <- as.factor(params$mudif[flag2])
      
      lines(DeltaI[,n][flag2][order(unique(mufac))], invPrb[,n][flag2][order(unique(mufac))],
            col=ncol[n])
      points(DeltaI[,n][flag2], invPrb[,n][flag2],
             col=col[mufac], cex=0.5, pch=16)
    }
  }
  
  title(main= paste("delta=", delta[i]), cex.main=0.75, font.main=1, line=1)
  if (i == length(delta)){
    legend("bottomleft", legend=levels(mufac), fill=col, cex=.5, bty='n', border="white")
  } else{
    legend("bottomright", legend=levels(mufac), fill=col, cex=.5, bty='n', border="white")
  }
  
}
dev.off()

#the way that DeltaI is related to invPrb and also mean difference depends on delta
# negative and then posotive relationship after 0.6 
# so the relationship changes with delta but im not sure why
# right out equations to see
# also: left is always more than right
# asymmetric always has less invPrb than symmetric
# 
pdf("DeltaI_invE.pdf")
for (i in 1:length(delta)){
  
  flag1 <- params$X4==delta[i]
  plot(0, xlab=expression(paste(Delta,"I")), ylab="E(r1|r1>0)", 
       xlim= c( min(DeltaI$X2[flag1]), max(DeltaI$X3[flag1]) ),
       ylim = c( min(invE$X3[flag1]), max(invE$X1[flag1]) ))
  
  for(n in 1:3){
    for (j in 1:length(sigma)){
      
      flag2 <- params$X1==sigma[j] & params$X4==delta[i]
      mufac <- as.factor(params$mudif[flag2])
      
      lines(DeltaI[,n][flag2][order(unique(mufac))], invE[,n][flag2][order(unique(mufac))],
            col=ncol[n])
      points(DeltaI[,n][flag2], invE[,n][flag2],
             col=col[mufac], cex=0.5, pch=16)
    }
  }
  
  title(main= paste("delta=", delta[i]), cex.main=0.75, font.main=1, line=1)
  if (i == length(delta)){
    legend("bottomleft", legend=levels(mufac), fill=col, cex=.5, bty='n', border="white")
  } else{
    legend("bottomright", legend=levels(mufac), fill=col, cex=.5, bty='n', border="white")
  }
  
}
dev.off()
#negatively correlated and then positive when delta>0.6



pdf("invE_invPrb.pdf")
for (i in 1:length(delta)){
  
  flag1 <- params$X4==delta[i]
  plot(0, xlab="E(r1|r1>0)", ylab="P[r1>0]", 
       xlim= c( min(invE$X3[flag1]), max(invE$X1[flag1]) ),
       ylim = c( min(invPrb$X2[flag1]), max(invPrb$X3[flag1]) ))
  
  for(n in 1:3){
    for (j in 1:length(sigma)){
      
      flag2 <- params$X1==sigma[j] & params$X4==delta[i]
      mufac <- as.factor(params$mudif[flag2])
      
      lines(invE[,n][flag2][order(unique(mufac))], invPrb[,n][flag2][order(unique(mufac))],
            col=ncol[n])
      points(invE[,n][flag2], invPrb[,n][flag2],
             col=col[mufac], cex=0.5, pch=16)
    }
  }
  title(main= paste("delta=", delta[i]), cex.main=0.75, font.main=1, line=1)
  legend("bottomright", legend=levels(mufac), fill=col, cex=.5, bty='n', border="white")
}
dev.off()
#mean maginitude of invasion is positively correlated with invasion probability 