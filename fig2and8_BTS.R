#coding out to find parameters for figure 2 and figure 8

#for figure 2 we wanted asym pos and sym neg and vice versa:

abc <- mean_df[mean_df$mdif ==0,]

abc <- abc[,-c(8,9,10)]
head(abc)
round(abc[abc$sym > 0 & abc$ELT<0 & abc$ERT<0, ],5)
round(abc[abc$sym < 0 & abc$ELT>0 & abc$ERT>0, ],5)


lapply(dflist, function(x){print(x[x$sym>0 & x$ERT<0 & x$ELT<0 & x$mn1 - x$mn2 ==0,-c(8,9,10)])})

##########################################################

#for figure 8 we wanted to find maximal differnces between SEasym and SEsym 

mean_df$elt == mean_df$ERT-mean_df$sym
names(mean_df)

mean_df[mean_df$ert==min(mean_df$ert),] #(6.4,0.9,0.9,0.5)
mean_df[mean_df$elt==min(mean_df$elt),] #(6.4,0.4,0.5,0.5)
head(round(mean_df[order(mean_df$ert),],4))
head(round(mean_df[order(mean_df$elt),],4))

#(6.4,0.9,0.9,0.5)
ex1<-co.hist(time=10000, d=0.5, sigma=6.4, mu1=0.9, mu2=0.9, N=50, N1=25, dom=0.95, n=1000)
ex1m<-measure.co(10000, 0.5, 6.4, 0.9, 0.9, 50, 25, 0.95, popsim=F,hist=F)


#(6.4, 0.8, 0.8, 0.5)
ex2<-co.hist(time=10000, d=0.5, sigma=6.4, mu1=0.8, mu2=0.8, N=50, N1=25, dom=0.95, n=1000)
ex2m<-measure.co(10000, 0.5, 6.4, 0.8, 0.8, 50, 25, 0.95, popsim=T,hist=T)

#(6.4, 0.7, 0.3, 0.4)
ex3<-co.hist(time=10000, d=0.4, sigma=6.4, mu1=0.7, mu2=0.3, N=50, N1=25, dom=0.95, n=1000)
ex3m<-measure.co(10000, 0.4, 6.4, 0.7, 0.3, 50, 25, 0.95, popsim=T,hist=T)

#(6.4,0.1,0.8,0.7)
ex4<-co.hist(time=10000, d=0.7, sigma=6.4, mu1=0.1, mu2=0.8, N=50, N1=25, dom=0.95, n=1000)
ex4m<-measure.co(10000, 0.7, 6.4, 0.1, 0.8, 50, 25, 0.95, popsim=T,hist=T)

##(6.4,0.2,0.5,0.6)
ex5<-co.hist(time=10000, d=0.6, sigma=6.4, mu1=0.2, mu2=0.6, N=50, N1=25, dom=0.95, n=1000)
ex5m<-measure.co(10000, 0.6, 6.4, 0.2, 0.5, 50, 25, 0.95, popsim=T,hist=T)

#(6.4,0.1,0.1,0.5)
ex6<-co.hist(time=10000, d=0.5, sigma=6.4, mu1=0.1, mu2=0.1, N=50, N1=25, dom=0.95, n=1000)
ex6m<-measure.co(10000, 0.5, 6.4, 0.1, 0.1, 50, 25, 0.95, popsim=T,hist=T)


#(6.4,0.4,0.5,0.5)
ex7<-co.hist(time=10000, d=0.5, sigma=6.4, mu1=0.4, mu2=0.5, N=50, N1=25, dom=0.95, n=1000)
ex7m<-measure.co(10000, 0.5, 6.4, 0.4, 0.5, 50, 25, 0.95, popsim=T,hist=T)

#(6.4,0.3,0.6,0.6)
ex8<-co.hist(time=10000, d=0.6, sigma=6.4, mu1=0.3, mu2=0.6, N=50, N1=25, dom=0.95, n=1000)
ex8m<-measure.co(10000, 0.6, 6.4, 0.3, 0.6, 50, 25, 0.95, popsim=T,hist=T)

#(6.4,0.6,0.8,0.5)
ex9<-co.hist(time=10000, d=0.5, sigma=6.4, mu1=0.6, mu2=0.8, N=50, N1=25, dom=0.95, n=1000)
ex9m<-measure.co(10000, 0.5, 6.4, 0.6, 0.8, 50, 25, 0.95, popsim=T,hist=T)

#(6.4,0.4,0.2,0.4)
ex10<-co.hist(time=10000, d=0.4, sigma=6.4, mu1=0.4, mu2=0.2, N=50, N1=25, dom=0.95, n=1000)
ex10m<-measure.co(10000, 0.4, 6.4, 0.4, 0.2, 50, 25, 0.95, popsim=T,hist=T)

#(6.4,0.6,0.2,0.4)
ex11<-co.hist(time=10000, d=0.4, sigma=6.4, mu1=0.6, mu2=0.2, N=50, N1=25, dom=0.95, n=1000)

#(6.4,0.2,0.9,0.7)
ex12<-co.hist(time=10000, d=0.5, sigma=6.4, mu1=0.9, mu2=0.9, N=50, N1=25, dom=0.95, n=1000)

rn<-paste(rep(c("co.ratio","co.mean"),12), c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12), sep="")

exDF<-data.frame(matrix(c(ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,ex10,ex11,ex12), nrow=12, byrow=T))

exDFm <- data.frame(matrix(c(ex1m[,2],ex2m[,2],ex3m[,2],ex4m[,2],ex5m[,2],ex6m[,2],ex7m[,2],ex8m[,2],ex9m[,2],ex10m[,2],ex1m[,2],ex1m[,2]),nrow=12,byrow=T))

exDF<-cbind(exDF,exDFm)


exDF[,7]<-c(round(mean_df[order(mean_df$ert),"sdev"],4)[1:6])
exDF[,8] <- c(round(mean_df[order(mean_df$ert),"mn1"],4)[1:6],round(mean_df[order(mean_df$elt),"mn1"],4)[1:6])
exDF[,9]<-c(round(mean_df[order(mean_df$ert),"mn2"],4)[1:6],round(mean_df[order(mean_df$elt),"mn2"],4)[1:6])
exDF[,10]<-c(round(mean_df[order(mean_df$ert),"delta"],4)[1:6],round(mean_df[order(mean_df$elt),"delta"],4)[1:6])

names(exDF)<-c('ELT.r','sym.r','ERT.r','ELT.m','sym.m','ERT.m','mu1','mu2','sigma','delta')
exDF





