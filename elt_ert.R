#adding means of dflist$sim_dfX$Y - mean_df$sym to mean_df
# X is number of simulation, Y is noise type

plot_list2 <- lapply(dflist, function(x){print(x[,c("ELT","sym", "ERT")])})

plot_list3 <- lapply(plot_list2, function(x) {print(apply(x, 2, function(y){y-mean_df$sym}))})

	ELTd <- matrix(NA, ncol = length(plot_list3), nrow = nrow(plot_list3[[1]]))
	ERTd <- matrix(NA, ncol = length(plot_list3), nrow = nrow(plot_list3[[1]]))
	symd <- matrix(NA, ncol = length(plot_list3), nrow = nrow(plot_list3[[1]]))
	
	for (i in 1:length(plot_list3)){
	
	ELTd[,i] <- plot_list3[[i]][,"ELT"]
	symd[,i] <- plot_list3[[i]][,"sym"]	
	ERTd[,i] <- plot_list3[[i]][,"ERT"]	

}
	
mean_df[,"elt"] <- apply(ELTd, MARGIN =1, mean)
mean_df[,"ert"] <- apply(ERTd, MARGIN =1, mean)
mean_df[,"sym_sym"] <- apply(symd, MARGIN =1, mean)
	
head(mean_df)