#decomposition function
decomp <- function(sigma, mu, delta, M){
  
  #exponentiate noise to get B
  load("./noise_etc.RData")
  rv <- transform(b_tilde, u_tilde, rho, sigma, mu, b_s=TRUE) #makes random vars defined by parameters
  Bl <- cbind(exp(rv$b_l1), exp(rv$b_l2))
  Br <- cbind(exp(rv$b_r1), exp(rv$b_r2))
  Bs <- cbind(exp(rv$b_s1), exp(rv$b_s2))
  
  #make B sharp
  bsharp_tilde <- makenoise(M)
  rho <- cor(bsharp_tilde$l)[1,2]
  rvsharp <- transform(bsharp_tilde, u_tilde, rho, sigma, mu, b_s=TRUE)
  Bl2sharp <- exp(rvsharp$b_l2)
  Br2sharp <- exp(rvsharp$b_r2)
  Bs2sharp <- exp(rvsharp$b_s2)
  
  #epsilon_0
  null_l <- log(1-delta+delta*(mean(Bl[,1])/mean(Bl[,2])))
  null_r <- log(1-delta+delta*(mean(Bl[,1])/mean(Bl[,2])))
  null_s <- log(1-delta+delta*(mean(Bl[,1])/mean(Bl[,2])))
  
  #epsilon_C
  epC1l <- mean(log(1-delta+delta*(mean(Bl[,1])/Bl[,2])))- null_l
  epC1r <- mean(log(1-delta+delta*(mean(Br[,1])/Br[,2])))- null_r
  epC1s <- mean(log(1-delta+delta*(mean(Bs[,1])/Bs[,2])))- null_s
  
  epC2l <- mean(log(1-delta+delta*(mean(Bl[,2])/Bl[,2])))
  epC2r <- mean(log(1-delta+delta*(mean(Br[,2])/Br[,2])))
  epC2s <- mean(log(1-delta+delta*(mean(Bs[,2])/Bs[,2])))
  
  #epsilon_E
  epE1l <- mean(log(1-delta+delta*(Bl[,1]/mean(Bl[,2]))))- null_l
  epE1r <- mean(log(1-delta+delta*(Br[,1]/mean(Br[,2]))))- null_r
  epE1s <- mean(log(1-delta+delta*(Bs[,1]/mean(Bs[,2]))))- null_s
  
  epE2l <- mean(log(1-delta+delta*(Bl[,2]/mean(Bl[,2]))))
  epE2r <- mean(log(1-delta+delta*(Br[,2]/mean(Br[,2]))))
  epE2s <- mean(log(1-delta+delta*(Bs[,2]/mean(Bs[,2]))))
  
  #epsilon_EC
  epEC1l <- mean(log(1-delta+delta*(Bl[,1]/Bl[,2])))- (null_l+ epC1l+ epE1l)
  epEC1r <- mean(log(1-delta+delta*(Br[,1]/Br[,2])))- (null_r+ epC1r+ epE1r)
  epEC1s <- mean(log(1-delta+delta*(Bs[,1]/Bs[,2])))- (null_s+ epC1s+ epE1s)
  
  epEC2l <- mean(log(1-delta+delta*(Bl[,2]/Bl[,2])))- (epC2l+ epE2l)
  epEC2r <- mean(log(1-delta+delta*(Br[,2]/Br[,2])))- (epC2r+ epE2r)
  epEC2s <- mean(log(1-delta+delta*(Bs[,2]/Bs[,2])))- (epC2s+ epE2s)
  
  #epsilon_(E#C)
  epECsharp1l <- mean(log(1-delta+delta*(Bl[,1]/Bl2sharp)))- (null_l+ epC1l+ epE1l)
  epECsharp1r <- mean(log(1-delta+delta*(Br[,1]/Br2sharp)))- (null_r+ epC1r+ epE1r)
  epECsharp1s <- mean(log(1-delta+delta*(Bs[,1]/Bs2sharp)))- (null_s+ epC1s+ epE1s)
  
  epECsharp2l <- mean(log(1-delta+delta*(Bl[,2]/Bl2sharp)))- (epC2l+ epE2l)
  epECsharp2r <- mean(log(1-delta+delta*(Br[,2]/Br2sharp)))- (epC2r+ epE2r)
  epECsharp2s <- mean(log(1-delta+delta*(Bs[,2]/Bs2sharp)))- (epC2s+ epE2s)
  
  #epsilon_(EC)
  epEC_1l <- epEC1l - epECsharp1l
  epEC_1r <- epEC1r - epECsharp1r
  epEC_1s <- epEC1s - epECsharp1s
  
  epEC_2l <- epEC2l - epECsharp2l
  epEC_2r <- epEC2r - epECsharp2r
  epEC_2s <- epEC2s - epECsharp2s
  
  #Delta_0
  Dnull_l <- null_l
  Dnull_r <- null_r
  Dnull_s <- null_s
  
  #Delta_E
  DE_l <- epE1l-epE2l
  DE_r <- epE1r-epE2r
  DE_s <- epE1s-epE2s
  
  #Delta_C
  DC_l <- epC1l-epC2l
  DC_r <- epC1r-epC2r
  DC_s <- epC1s-epC2s
  
  #Delta_(EC)
  DEC_l <- epEC_1l-epEC_2l
  DEC_r <- epEC_1r-epEC_2r
  DEC_s <- epEC_1s-epEC_2s
  
  #Delta_(E#C)
  DECsharpl <- epECsharp1l-epECsharp2l
  DECsharpr <- epECsharp1r-epECsharp2r
  DECsharpS <- epECsharp1s-epECsharp2s
  
  #format
  Dnull <- c(Dnull_l, Dnull_r, Dnull_s)
  
  DE <- c(DE_l, DE_r, DE_s)
  
  DC <- c(DC_l, DC_r, DC_s)
  
  DEC_ <- c(DEC_l, DEC_r, DEC_s)
  
  DECsharp <- c(DECsharpl, DECsharpr, DECsharpS)
  
  res <- data.frame(Dnull=Dnull, DE=DE, DC=DC, DEC_=DEC_, DECsharp=DECsharp,
                    row.names=c("left","right","sym"))
  return(res)
  
}

#decomp(1.6, c(0.3,0.4), 0.4, M)

