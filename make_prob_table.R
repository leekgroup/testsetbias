# Input:
# data - dataset that you made the pairs from
# outcome - the outcomes for the dataset
# tsp - the pairs
#
# Output:
# a table with P(Ci=ci|Pj=1) for each class and pair

make_prob_table <- function(data, outcome, tsp){
	outcome <- as.factor(outcome)
	pair_list <- sapply(tsp,function(x){as.numeric(strsplit(x,"<")[[1]])},simplify=F)
	
	mat <- matrix(NA, length(pair_list), length(levels(outcome)))
	for(i in 1:length(pair_list)){
		tmp <- table((data[pair_list[[i]][1],] - data[pair_list[[i]][2],] < 0), outcome)
		tmp <- tmp[2,]/rowSums(tmp)[2]
		mat[i,] <- tmp
#		for(j in 1:length(levels(outcome))){
#			mat[i,j] <- tmp[j]
#		}
	}

	rownames(mat) <- tsp
	colnames(mat) <- levels(outcome)
	mat
}

