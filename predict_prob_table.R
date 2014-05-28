# Assume data has the rownames that genes is made of!

make_mat <- function(data,genes){
	mat <- matrix(NA, length(genes), ncol(data))
	rownames(mat) <- genes
	colnames(mat) <- colnames(data)

	for(i in 1:length(genes)){
		if(genes[i] %in% rownames(data)){
			mat[i,] <- data[genes[i],]
		}
	}

	mat
}

prob_mult <- function(pairs,probs){
	running <- 1
	for(i in 1:length(pairs)){
		if(pairs[i] == 1){
			running <- running*probs[i]
		} else if(pairs[i] == 0){
			running <- running*(1-probs[i])
		}
	}
	running
}

class_probs <- function(data, genes, prob_mat, tsp){
	mat <- make_mat(data, genes) # Each missing gene is NA
	m <- nrow(mat)
	pm <- pairMatC(mat) # This should have NAs in it
	
	rownames(pm) <- paste(rep(1:(m-1),(m-1):1), create_seq(2,m),sep="<")

	pm <- pm[tsp,]

	output <- matrix(NA, ncol(pm), ncol(prob_mat))

	for(i in 1:ncol(pm)){
		cur <- pm[,i]
		for(j in 1:ncol(output)){
			output[i,j] <- prob_mult(cur, prob_mat[,j])
		}
	}
	output
}

