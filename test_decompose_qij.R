

msample <- sample(mudif,1)
ssample <-sample(1:7, 1)
dsample <- sample(delta, 1)
cat("mudif:",msample,"sigma:",ssample,"delta:",dsample,"\n\n")
test <- round(decompose(msample, ssample, dsample, b_tilde, u_tilde),4)
cat("eEi=",test$ei[2], "\neEj=", test$ej[2], "\nDE=", test$D[2], "\nDqE=", test$Dq[2],
    "\n\neCi=", test$ei[3], "\neCj=", test$ej[3], "\nDC=", test$D[3], "\nDqC=", test$Dq[3], 
    "\n\ne(E#C)i=", test$ei[4], "\ne(E#C)j=", test$ej[4], "\nD(E#C)=", test$D[4], "\nDq(E#C)=", test$Dq[4], 
    "\n\ne[EC]i=", test$ei[5], "\ne[EC]j=", test$ej[5], "\nD[EC]=", test$D[5], "\nDq[EC]=", test$Dq[5], "\n\n")

cat("sum(D)=", sum(test$D[1:6]), "sum(Dq)=", sum(test$Dq[1:6]))





#test se change with M
source("./decomp1_plotting.R")

#M=1000000
source("./makenoise.R")
test1<- dePlot1(0,1.0)
source("./makenoise.R")
test2 <- dePlot1(0,1.0)
source("./makenoise.R")
test3 <- dePlot1(0,1.0)
source("./makenoise.R")
test4 <- dePlot1(0,1.0)
source("./makenoise.R")
test5 <- dePlot1(0,1.0)
test1_5 <- c(test1[[8]]$D[7],test2[[8]]$D[7],test3[[8]]$D[7],test4[[8]]$D[7],test5[[8]]$D[7])
test1_5r <- c(test1[[8]]$D[6],test2[[8]]$D[6],test3[[8]]$D[6],test4[[8]]$D[6],test5[[8]]$D[6])
test1_5b <- c(test1[[8]]$D[5],test2[[8]]$D[5],test3[[8]]$D[5],test4[[8]]$D[5],test5[[8]]$D[5])

#M=5000000
source("./makenoise.R")
test6<- dePlot1(0,1.0)
source("./makenoise.R")
test7 <- dePlot1(0,1.0)
source("./makenoise.R")
test8 <- dePlot1(0,1.0)
source("./makenoise.R")
test9 <- dePlot1(0,1.0)
source("./makenoise.R")
test10 <- dePlot1(0,1.0)
test6_10 <- c(test6[[8]]$D[7],test7[[8]]$D[7],test8[[8]]$D[7],test9[[8]]$D[7],test10[[8]]$D[7])
test6_10r <- c(test6[[8]]$D[6],test7[[8]]$D[6],test8[[8]]$D[6],test9[[8]]$D[6],test10[[8]]$D[6])
test6_10b <- c(test6[[8]]$D[5],test7[[8]]$D[5],test8[[8]]$D[5],test9[[8]]$D[5],test10[[8]]$D[5])

#M=100000
source("./makenoise.R")
test11<- dePlot1(0,1.0)
source("./makenoise.R")
test12 <- dePlot1(0,1.0)
source("./makenoise.R")
test13 <- dePlot1(0,1.0)
source("./makenoise.R")
test14 <- dePlot1(0,1.0)
source("./makenoise.R")
test15 <- dePlot1(0,1.0)
test11_15 <- c(test11[[8]]$D[7],test12[[8]]$D[7],test13[[8]]$D[7],test14[[8]]$D[7],test15[[8]]$D[7])
test11_15r <- c(test11[[8]]$D[6],test12[[8]]$D[6],test13[[8]]$D[6],test14[[8]]$D[6],test15[[8]]$D[6])
test11_15b <- c(test11[[8]]$D[5],test12[[8]]$D[5],test13[[8]]$D[5],test14[[8]]$D[5],test15[[8]]$D[5])

mean(abs(test1_5))
mean(abs(test6_10))
mean(abs(test11_15))

mean(abs(test1_5r))
mean(abs(test6_10r))
mean(abs(test11_15r))

mean(abs(test1_5b))
mean(abs(test6_10b))
mean(abs(test11_15b))

#with M=1000000, is SE below a certain value?
#from Figure3.R
load("./params.RData")
#M=1000000
source("./makenoise.R")
res <- vector(mode='list',length=1)
mvec <- NA
dvec <- NA
n <- 1
for (m in 1:length(mudif)){
  for (d in 1:length(delta)){
    res <- append(res,dePlot1(mudif[m], delta[d], xaxt="n"))
    n <- n+1
    mrep <- rep(mudif[m], 8)
    drep <- rep(delta[d],8)
    mvec <- append(mvec, mrep)
    dvec <- append(dvec, drep)
  }
}
res <- res[-1]; mvec<- mvec[-1]; dvec <- dvec[-1]




se <- lapply(res, function(X){X$D_se[-8]})
Delta <- lapply(res, function(X){X$D[-8]})
semat <- matrix(unlist(se), ncol=7, nrow=128, byrow=TRUE)
Dmat <- matrix(unlist(Delta), ncol=7, nrow=128, byrow=TRUE)
max(semat)
resvec <- unlist(se)
whichmax <- which(resvec==max(resvec))
whichmax/7
md <- cbind(mvec,dvec)
md[31,]; md[63,]; md[95,]; md[127,]
# max always sits at 6, [E||C] AND delta=1
seATA<-semat[,6]
se_r <- semat[,7]
D_ATA<-Dmat[,6]
D_r <- Dmat[,7]

resMat <- cbind(md, seATA, D_ATA, seATA/D_ATA, se_r, D_r, se_r/D_r)
resMat[resMat[,3]==max(resMat[,3]),]
max(seofD)

dePlot1(-0.3,0.5)

resMat0 <- resMat[resMat[,2]!=1.0,] #take out delta=1 from resMat
resMat0[resMat0[,3]==max(resMat0[,3]),] #what is the max se of ATA contribution now

#dont include delta=1 in loop

#M=100000
source("./makenoise.R")
res <- vector(mode='list',length=1)
n <- 1
for (m in 1:length(mudif)){
  for (d in 1:length(delta)){
    res <- append(res,dePlot1(mudif[m], delta[d], xaxt="n"))
    #axis(1, labels=ifelse(n>12, yes=TRUE, no=FALSE), tick=TRUE)
    n <- n+1
  }
}
res <- res[-1]
res1 <- lapply(res, function(X){X$D_se[-8]})
resmat2 <- matrix(unlist(res1), ncol=7, nrow=128, byrow=TRUE)
max(resmat2)
