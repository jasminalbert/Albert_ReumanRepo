new.dflist<-read.csv('new.dflist.csv')

new.df <- new.dflist[-1]


cols <- seq(1,160,16)
df <- vector(mode='list', length=10)

names <- NA

for (i in 1:10){
	df[[i]] <- new.df[cols[i]:(cols[i]+15)]
	names(df[[i]]) <- names(df[[1]])
	names[i] <- paste("df",i, sep="")
}

names(df)<-names
str(df)

list2env(df, envir=.GlobalEnv)

mean.df <- (df1+df2+df3+df4+df5+df6+df7+df8+df9+df10)/10

mean.df[,1:4]<-df1[,1:4]


