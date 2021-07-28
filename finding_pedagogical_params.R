source("./Figure3.R")
load("./params.RData")
D <- lapply(res, function(X){(X$D)})
D1 <- unlist(D)
D2 <- matrix(D1, nrow=7)
row.names(D2) <- c("null","E","C","E_C","EC","E__C","r")
woATA <- D2[7,] - D2[6,]
D3 <- rbind(D2, woATA)
D3[5:8,]
D4 <- t(D3)
df <- data.frame(D4)
df[df$r <0 & df$woATA >0,]
df[df$r >0 & df$woATA <0,]
md <- sort(rep(mudif,4), decreasing = T)
d <- rep(delta,4)
p <- cbind(md,d)
p1 <- rbind(p,p,p,p,p,p,p,p)
s <- sort(rep(0:7,16))
p2 <- cbind(p1,s)

p.df <- data.frame(p2)
p2.df <- p.df[order(p.df$md, p.df$d),]
p3.df <- p2.df[order(p2.df$md, decreasing=T),]
dat <- cbind(p3.df, df)



dat[dat$r<0 & df$woATA >0,] #impeding

#plot1<-dePlot(-0.8,0.8, legend=T)

dat[dat$r>0 & df$woATA <0,] #facilitating

dat[dat$r == max(dat$r),]  #max r

S#most negatice r
head(dat[order(dat$r),])

#most negative [E||C]
head(dat[order(dat$E__C),])
#most postive
head(dat[order(dat$E__C, decreasing=T),])


