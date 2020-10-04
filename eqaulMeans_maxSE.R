source("./param_sim.R")

#noticed trend that when mn1=mn2, max SE appears to be at delta = 0.5
#testing that this is true for all means and all sdevs

#extract all simimulations where mn1=mn2 and store in list of all sdevs and deltas
#length will be length of sdev * length of delta

#initialize list
eq_meanList <- vector(mode = 'list', length = length(mn1)*length(sdev))

#loop to make list
#each element in list is one mean value
index <- matrix(c(rep(sort(mn1),5), sort(rep(sdev, 9))), ncol = 2)

for (i in 1:dim(index)[1]) {
	eq_meanList[[i]] <- mean_df[mean_df$mn1 - mean_df$mn2 ==0 & mean_df$mn1 == index[i,1] & mean_df$sdev == index[i,2],-c(8,9,10)]
}

#new list of simulations of the max SE for that mean value
listA <- lapply(eq_meanList, function(x){print(x[x$sym == max(x$sym),])})

#elt diff
listB <- lapply(eq_meanList, function(x){print(x[abs(x$elt) == max(abs(x$elt)),])})

#ert diff
listC <- lapply(eq_meanList, function(x){print(x[abs(x$ert) == max(abs(x$ert)),])})

#see what the delta value is at max SE
deltaA <- lapply(listA, function(x){print(x$delta)})
deltaB <- lapply(listB, function(x){print(x$delta)})
deltaC <- lapply(listC, function(x){print(x$delta)})
deltaABC <- cbind(index, unlist(deltaA), unlist(deltaB), unlist(deltaC))
colnames(deltaABC) <- c("mn","sdev", "sym", "elt","ert")

#jitter plot for at what delta is SEdif max at diff sdev

#see figure 7 (line 145) in /FIGURES.R


