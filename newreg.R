library(Rcpp)
#library(RcppArmadillo)
library(genefilter)
sourceCpp("~/code/pairMat.cpp")
#sourceCpp("~/code/fstats.cpp")

# Need this for indexing
create_seq <- function(start,stop){
	if(start == stop){
		return(start)
	} else {
		return(c(start:stop, create_seq(start+1,stop)))
	}
}

newreg <- function(data, outcome, npair=5){
	m <- dim(data)[1]
#	ind <- 1:m
	oc <- as.numeric(outcome) # Expect factor, treat as numeric
	genes <- vector("integer", npair)

#	ind1 <- rep(1:m,m); ind2 <- rep(1:m,each=m)
#	index <- paste(rep(ind,m),rep(ind,each=m),sep="<")
#	index <- index[ind1 < ind2] # These are the rownames for pairMat
	index <- paste(rep(1:(m-1),(m-1):1), create_seq(2,m),sep="<")

	pairMat <- pairMatC(data) # Matrix of pairwise comparisons
	#pmrow <- 1:nrow(pairMat)

	row_check <- apply(pairMat, 1, sum)# Toss all 0's and all 1's
	row_check <- which(row_check == 0 | row_check == ncol(pairMat))
	if(length(row_check) > 0){
		pairMat <- pairMat[-row_check,]
	}

	fs1 <- genefilter::rowFtests(pairMat, as.factor(outcome))$statistic
	genes[1] <- which.max(fs1) # First gene, as found by row F tests

	mod0 <- cbind(matrix(1, ncol(data), 1),pairMat[genes[1],]) # Null
	mod <- cbind(mod0, rep(0, dim(mod0)[1])) # Alternative w/placeholder

	for(i in 2:npair){
		tmpstats <- fstatsC(pairMat, mod, mod0, oc)
		idx <- which.max(tmpstats)
		genes[i] <- idx
		#genes[i] <- pmrow[idx]
		mod[,dim(mod)[2]] <- pairMat[idx,]
		mod0 <- mod
		mod <- cbind(mod0, rep(0,dim(mod0)[1]))
		#pairMat <- pairMat[-idx,]
		#pmrow <- pmrow[-idx]
	}
	
	index[genes]
}

#data <- matrix(rnorm(500*20), 500, 20)
#outcome <- as.factor(sample(c("A","B","C"),20,replace=T))
#outcome <- rnorm(20,mean=10,sd=2)
#tmp <- newreg(data, outcome,npair=10)

#pairMat <- pairMatC(dat)


## Code excerpt for testing just the fstatsC function
#oc <- as.numeric(outcome) # Expect factor, treat as numeric
#genes <- vector("integer", npair)

#ind1 <- rep(1:m,m); ind2 <- rep(1:m,each=m)
#index <- paste(rep(ind,m),rep(ind,each=m),sep="<")
#index <- index[ind1 < ind2] # These are the rownames for pairMat

#pairMat <- pairMatC(data) # Matrix of pairwise comparisons

#fs1 <- genefilter::rowFtests(pairMat, outcome)$statistic
#genes[1] <- which.max(fs1) # First gene, as found by row F tests
#mod0 <- cbind(matrix(1, ncol(data), 1),pairMat[genes[1],]) # Null
#mod <- cbind(mod0, rep(0, dim(mod0)[2])) # Alternative w/placeholder
#tmpstats <- fstatsC(pairMat, mod, mod0, oc)
##

