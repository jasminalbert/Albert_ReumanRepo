source("./makenoise.R") 
sigma <- 2
mu <- c(0.5,0.8)
b_l1 <- sigma*b_tilde$l[,1] + mu[1]
b_l2 <- sigma*b_tilde$l[,2] + mu[2]
Bl <- cbind(exp(b_l1), exp(b_l2))

b_r1 <- sigma*b_tilde$r[,1] + mu[1]
b_r2 <- sigma*b_tilde$r[,2] + mu[2]
Br <- cbind(exp(b_r1), exp(b_r2))

b_s1 <- sigma*b_tilde$s[,1] + mu[1]
b_s2 <- sigma*b_tilde$s[,2] + mu[2]
Bs <- cbind(exp(b_s1), exp(b_s2))

source("./makenoise.R") 
b_l2sharp <- sigma*b_tilde$l[,2] + mu[2]
Bl2sharp <- exp(b_l2sharp)

b_r2sharp <- sigma*b_tilde$r[,2] + mu[2]
Br2sharp <- exp(b_r2sharp)

b_s2sharp <- sigma*b_tilde$s[,2] + mu[2]
Bs2sharp <- exp(b_s2sharp)

delta <- 0.5

null_l <-  log(1-delta+delta*(mean(Bl[,1])/mean(Bl[,2]))); print(null_l)
null_r <-  log(1-delta+delta*(mean(Br[,1])/mean(Br[,2]))); print(null_r)
null_s <-  log(1-delta+delta*(mean(Bs[,1])/mean(Bs[,2]))); print(null_s)

epC1l <- mean(log(1-delta+delta*(mean(Bl[,1])/Bl[,2])))- null_l; print(epC1l)
epC1r <- mean(log(1-delta+delta*(mean(Br[,1])/Br[,2])))- null_r; print(epC1r)
epC1s <- mean(log(1-delta+delta*(mean(Bs[,1])/Bs[,2])))- null_s; print(epC1s)

epC2l <- mean(log(1-delta+delta*(mean(Bl[,2])/Bl[,2]))); print(epC2l)
epC2r <- mean(log(1-delta+delta*(mean(Br[,2])/Br[,2]))); print(epC2r)
epC2s <- mean(log(1-delta+delta*(mean(Bs[,2])/Bs[,2]))); print(epC2s)

epE1l <- mean(log(1-delta+delta*(Bl[,1]/mean(Bl[,2]))))- null_l; print(epE1l)
epE1r <- mean(log(1-delta+delta*(Br[,1]/mean(Br[,2]))))- null_r; print(epE1r)
epE1s <- mean(log(1-delta+delta*(Bs[,1]/mean(Bs[,2]))))- null_s; print(epE1s)

epE2l <- mean(log(1-delta+delta*(Bl[,2]/mean(Bl[,2])))); print(epE2l)
epE2r <- mean(log(1-delta+delta*(Br[,2]/mean(Br[,2])))); print(epE2r)
epE2s <- mean(log(1-delta+delta*(Bs[,2]/mean(Bs[,2])))); print(epE2s)

epEC1l <- mean(log(1-delta+delta*(Bl[,1]/Bl[,2])))- (null_l+ epC1l+ epE1l); print(epEC1l)
epEC1r <- mean(log(1-delta+delta*(Br[,1]/Br[,2])))- (null_r+ epC1r+ epE1r); print(epEC1r)
epEC1s <- mean(log(1-delta+delta*(Bs[,1]/Bs[,2])))- (null_s+ epC1s+ epE1s); print(epEC1s)

epEC2l <- mean(log(1-delta+delta*(Bl[,2]/Bl[,2])))- (epC2l+ epE2l); print(epEC2l)
epEC2r <- mean(log(1-delta+delta*(Br[,2]/Br[,2])))- (epC2r+ epE2r); print(epEC2r)
epEC2s <- mean(log(1-delta+delta*(Bs[,2]/Bs[,2])))- (epC2s+ epE2s); print(epEC2s)

epECsharp1l <- mean(log(1-delta+delta*(Bl[,1]/Bl2sharp)))- (null_l+ epC1l+ epE1l); print(epECsharp1l)
epECsharp1r <- mean(log(1-delta+delta*(Br[,1]/Br2sharp)))- (null_r+ epC1r+ epE1r); print(epECsharp1r)
epECsharp1s <- mean(log(1-delta+delta*(Bs[,1]/Bs2sharp)))- (null_s+ epC1s+ epE1s); print(epECsharp1s)

epECsharp2l <- mean(log(1-delta+delta*(Bl[,2]/Bl2sharp)))- (epC2l+ epE2l); print(epECsharp2l)
epECsharp2r <- mean(log(1-delta+delta*(Br[,2]/Br2sharp)))- (epC2r+ epE2r); print(epECsharp2r)
epECsharp2s <- mean(log(1-delta+delta*(Bs[,2]/Bs2sharp)))- (epC2s+ epE2s); print(epECsharp2s)

epEC_1l <- epEC1l - epECsharp1l; print(epEC_1l)
epEC_1r <- epEC1r - epECsharp1r; print(epEC_1r)
epEC_1s <- epEC1s - epECsharp1s; print(epEC_1s)

epEC_2l <- epEC2l - epECsharp2l; print(epEC_2l)
epEC_2r <- epEC2r - epECsharp2r; print(epEC_2r)
epEC_2s <- epEC2s - epECsharp2s; print(epEC_2s)

Dnull_l <- null_l
Dnull_r <- null_r
Dnull_s <- null_s

DE_l <- epE1l-epE2l
DE_r <- epE1r-epE2r
DE_s <- epE1s-epE2s

DC_l <- epC1l-epC2l
DC_r <- epC1r-epC2r
DC_s <- epC1s-epC2s

DEC_l <- epEC_1l-epEC_2l
DEC_r <- epEC_1r-epEC_2r
DEC_s <- epEC_1s-epEC_2s

DECsharpl <- epECsharp1l-epECsharp2l
DECsharpr <- epECsharp1r-epECsharp2r
DECsharpS <- epECsharp1s-epECsharp2s

Dnull <- c(Dnull_l, Dnull_r, Dnull_s)

DE <- c(DE_l, DE_r, DE_s)

DC <- c(DC_l, DC_r, DC_s)

DEC_ <- c(DEC_l, DEC_r, DEC_s)

DECsharp <- c(DECsharpl, DECsharpr, DECsharpS)

res <- data.frame(Dnull=Dnull, DE=DE, DC=DC, DEC_=DEC_, DECsharp=DECsharp)
res

#cbind(mean(log(1-delta+delta*(Bl[,1]/mean(Bl[,2])))), mean(log(1-delta+delta*(mean(Bl[,1])/Bl[,2]))))
