# First we get the npair pairs from subwrap(...)
# Let us assume the rownames correspond in all
# datasets

# expand.grid(rep(list(0:1),5))

build_list_tree <- function(len){
	if(len == 0){
		return("")
	} else {
		list("1"=build_list_tree(len-1), "0"=build_list_tree(len-1))
	}
}

# Pass this the pairmatrix resulting from calculateTspairs(),
# the outcome vector, and the tsp vector
create_model_tree <- function(pairmat, outcome, tsp){
	grid <- expand.grid(rep(list(0:1), length(tsp)))
	grid <- grid[-1,] # Remove the all-zeroes row
	grid <- grid[-which(rowSums(grid)==1),] # Get rid of 1-pair rows bc they suck
	tree <- build_list_tree(length(tsp))	

	cols <- apply(grid, 1, function(x){which(x == 1)})

	for(i in 1:nrow(grid)){
		tree[[as.character(grid[i,])]] <- rpart(outcome~., data=pairmat[,c(cols[[i]])], method="class")
	}

	tree
}

# Pass this the new dataset, the gene row list, the tree from
# create_model_tree, and the tsp vector
predict_model_tree <- function(data, genes, tree, tsp){

	mat <- make_mat(data,genes) #need make_mat() from predict_prob_table.R
	pm <- calculateTspairs(mat, tsp)

	pm_idx <- (!is.na(pm$pairMat[1,]))
	model_idx <- as.character(pm_idx+0)

	pairmat <- pm$pairMat[,c(pm_idx)]

	if(sum(pm_idx) <= 1){
		list("predictions"=NULL, "pairs"=model_idx)	
	} else {
		predictions <- predict(tree[[model_idx]], newdata=pairmat)

		list("predictions"=colnames(predictions)[apply(predictions, 1, which.max)],
	     "pairs"=model_idx)
	}
}

tspair2gene <- function(tsp_idx, genelist){
        tmp_idx <- sapply(tsp_idx,strsplit,"<")
        unlist(lapply(tmp_idx,function(x){paste(genelist[as.numeric(x)],collapse="<")}))
}

geneid2symbol <- function(dat, gid){
	tmp_gid <- sapply(gid, strsplit, "<")
	unlist(lapply(tmp_gid, function(x){paste(fData(dat)[x,"SYMBOL"], collapse="<")}))
}
