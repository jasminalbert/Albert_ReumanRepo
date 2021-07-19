source("./simsPlot_fxn.R")
par(mfrow=c(2,1), mar=c(3,3,2,1))
#impeding
simsPlot(mudif=-0.8, delta=0.8, sigma=5)
simsPlot(mudif=-0.6, delta=0.8, sigma=4)

#facilitating?
simsPlot(mudif=0, delta=1, sigma=7)
simsPlot(mudif=0, delta=1, sigma=6)
simsPlot(mudif=0, delta=1, sigma=5)
simsPlot(mudif=0, delta=1, sigma=3)
simsPlot(mudif=0, delta=1, sigma=2)
simsPlot(mudif=0, delta=1, sigma=1)


#max r
simsPlot(mudif=0, delta=0.5, sigma=7)

#min r (negative)
simsPlot(mudif=-0.8, delta=1, sigma=1)





