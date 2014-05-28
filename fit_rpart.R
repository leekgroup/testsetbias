pred_loss <- function(preds, outcome, loss){
	out_lev <- levels(outcome)
	sum(mapply(function(x,y){ idx <- which(outcome == x); sum(preds[idx] != outcome[idx])/length(preds[idx])*y}, out_lev, loss))
}


# Expect data to be features x samples (r x c)
# outcome is coerced to factor

fit_rpart <- function(data, outcome, index, npair=5, loss=NULL){

	outcome <- as.factor(outcome)
	features <- vector("integer", npair)
	max_ac <- 0

	data <- as.data.frame(t(data)) # now we are samples x features (better for regression)

	#idx <- sample(1:nrow(data), round(nrow(data)/4))

	# Split data into training and testing sets
	#train <- data[-idx,]
	#test <- data[idx,]
	#train_outcome <- outcome[-idx]
	#test_outcome <- outcome[idx]

	cn <- colnames(data)
	#fla <- "train_outcome ~"
	fla <- "outcome ~ 1"
	j = 1
	while(j <= npair){
		if(is.null(data) | is.null(dim(data))){
			break
		}
		if(is.null(ncol(data)) | ncol(data) == 0){
			break
		}
		varimp <- vector("numeric", ncol(data))
		for(i in 1:ncol(data)){
			fit <- rpart(as.formula(paste0(fla, "+", cn[i])), data=data, method="class")
                        pred <- predict(fit,type="class")
			varimp[i] <- sum(as.character(pred) == as.character(outcome[as.numeric(names(pred))]))/length(outcome[as.numeric(names(pred))]) - max_ac # The indexing is a crude way to deal with missing predictions
			#varimp[i] <- pred_loss(pred, outcome[as.numeric(names(pred))], loss)
		}
		idx_tmp <- which.max(varimp) # This is the PAIR ITSELF, not its index
		#idx_tmp <- which.min(varimp) # We've changed to varimp == weighted number of errors, so we want the fewest
		features[j] <- index[idx_tmp]
		max_ac <- varimp[idx_tmp]
		fla <- paste0(fla, "+", cn[idx_tmp])
		#cat(paste("max_ac is:", max_ac, "\n"))
		if(max_ac <= 0 | all(varimp == varimp[1]) | sum(duplicated(features[1:j])) > 0){
			#return(features[1:(j-1)])
			cur <- features[1:j]
			# These are the genes we need to get rid of from the data
			gset <- unique(as.numeric(unlist(sapply(cur, strsplit, "<"))))
			# These are the records we need to toss from the data and the index
			remove <- rowSums(sapply(gset, function(x){grepl(paste0("^",x,"<"),index) + grepl(paste0("<",x,"$"),index)}))
			# If remove > 0, then a removal gene has appeared there at least once
			remove <- ifelse(remove > 0, TRUE, FALSE)
			data <- data[,-which(remove)]
			index <- index[-which(remove)]
			cn <- cn[-which(remove)]
			# Restart the model
			fla <- "outcome ~ 1"
			# NOTE we don't increment j here because we want features[j] to be set to non-garbage
		} else {
			features[j] <- index[idx_tmp]
			j = j + 1
		}
	}
	features[features != "0"]
	#features
}

rpart_cond <- function(data, outcome, npair=5, loss=rep(1, length(levels(outcome)))){
	m <- nrow(data)
	index <- paste(rep(1:(m-1),(m-1):1), create_seq(2,m),sep="<")

	pm <- pairMatC(data)
	#out <- fit_rpart(pm, outcome, npair)

	#if(length(out) < npair){
	#	warning(paste("Only found", length(out), "pairs!\n"))
	#}
	#index[out]
	out <- fit_rpart(pm, outcome, index, npair, loss)
	out
}
