# Need this to create index sequence

create_seq <- function(start,stop){
	if(start == stop){
		return(start)
	} else {
		return(c(start:stop, create_seq(start+1,stop)))
	}
}



# Input:
# data - pair matrix, rows = features, columns = samples
# outcome - outcomes for each sample, length = ncol(data)
# npair - number of pairs desired for classifier
subset_fit <- function(data, outcome, npair=5){
	outcome <- as.factor(outcome)
	lev <- levels(outcome)
	grid <- t(combn(lev,2))
	
	if(is.null(rownames(data))){
		rownames(data) <- paste0("V", 1:nrow(data))
	}

	if(length(lev) > 2){
	
		subset <- apply(grid, 1,function(x){data[,which(outcome==x[1] | outcome == x[2])]})
		subout <- apply(grid, 1,function(x){outcome[which(outcome==x[1] | outcome == x[2])]})

		idx <- c()
		for(i in 1:length(subout)){
			tmp <- as.data.frame(t(subset[[i]]))
			fit <- rpart(as.formula(paste("subout[[i]]~", paste(colnames(tmp),collapse="+"))),data=tmp,method="class")
			# Take the top npair features, or however many we have up to npair

			idx <- c(idx, fit$var)

			tmp2 <- names(fit$var)[1:npair]
			if(length(which(is.na(tmp2))) >0){
				tmp2 <- tmp2[-which(is.na(tmp2))]
			}
			idx <- c(idx,which(colnames(tmp) %in% tmp2))
		}


		idx <- unique(idx)
		idx

	} else {
		tmp <- as.data.frame(t(data))
		fit <- rpart(as.formula(paste("outcome~", paste(colnames(tmp),collapse="+"))),data=tmp,method="class")
		# Take the top npair features, or however many we have up to npair
		tmp2 <- names(fit$var)[1:npair]
		if(length(which(is.na(tmp2))) > 0){
			tmp2 <- tmp2[-which(is.na(tmp2))]
		}

		which(colnames(tmp) %in% tmp2)
	}

	
#	tmp <- as.data.frame(t(data))
#	fit <- rpart(as.formula(paste("outcome~", paste(colnames(tmp),collapse="+"))),data=tmp,method="class")

#	prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

}

subwrap <- function(data, outcome, npair=5){
	m <- nrow(data)
	index <- paste(rep(1:(m-1),(m-1):1), create_seq(2,m),sep="<")

	pm <- pairMatC(data)
	index[subset_fit(pm, outcome, npair)]
}
