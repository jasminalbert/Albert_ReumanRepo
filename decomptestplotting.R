list <- vector(mode='list', length=10)

for (i in 1:10){
  source("./decomp_test.R")
  list[[i]] <- res
  
}
list

#DECsharp
plot(0, pch='', ylim = c(0, 0.01), xlim=c(0,3.5), main="DECsharp")
for (i in 1:10){
  points(1:3, list[[i]]$DECsharp, col=rainbow(10)[i])
}


#DEC_
plot(0, pch='', ylim = c(0.13, 0.17), xlim=c(0,3.5), main="DEC_")
for (i in 1:10){
  points(1:3, list[[i]]$DEC_, col=rainbow(10)[i])
}

#DE
plot(0, pch='', ylim = c(0.05, 0.1), xlim=c(0,3.5), main="DE")
for (i in 1:10){
  points(1:3, list[[i]]$DE, col=rainbow(10)[i])
}

#DC
plot(0, pch='', ylim = c(-0.1, -0.07), xlim=c(0,3.5), main="DC")
for (i in 1:10){
  points(1:3, list[[i]]$DC, col=rainbow(10)[i])
}

#Dnull
plot(0, pch='', ylim = c(-0.17, -0.10), xlim=c(0,3.5), main="Dnull")
for (i in 1:10){
  points(1:3, list[[i]]$Dnull, col=rainbow(10)[i])
}

