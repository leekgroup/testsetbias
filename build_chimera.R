#' Create a "chimeric" dataset
#'
#' \code{build_chimera} takes a list of ExpressionSets and produces a dataset that combines selected
#' records from each dataset in the list. A specified outcome of interest governs how records are chosen
#' (we want the outcome to be non-NA overall).
#'
#' @param data_list List of ExpressionSets
#' @param outcome Outcome of interest
#' @param num Number of records required from each dataset. If this value is larger than the number of records
#' available with non-missing outcome in a dataset, use as many as possible.
#' @return A list containing the the new matrix of expression values ($data), the outcome vector ($vector), and
#' all of the phenotype data for each record, including the platform used ($pData).
#' @export
#' @examples
#' # Suppose we have a list eset.all that contains all of our expression sets
#' chim <- build_chimera(eset.all[c(1,2,5,6,21,22)], "subtype", 100)

build_chimera <- function(data_list, outcome, num = 40){
	chimera <- c()
	ch_out <- c()
	pdata <- c()

	rows <- Reduce(intersect, lapply(data_list, rownames))

	for(i in 1:length(data_list)){
		mat <- exprs(data_list[[i]])[rows,]
		out <- pData(data_list[[i]])[[outcome]]
		out_idx <- which(!is.na(out)) # These are the non-missing records
		platform <- annotation(data_list[[i]])

		if(length(out_idx) == 0){
			warning(paste0("Dataset ", i, " has no non-missing records. Skipping dataset ", i, "."))
			next
		} else if(length(out_idx) < num){
			warning(paste0("Fewer non-missing records than desired in dataset ", i,". Using as many as possible."))
			idx <- out_idx
			
		} else {
			idx <- sample(out_idx, num)
		}
		chimera <- cbind(chimera, mat[,idx])
		if(is.factor(out)){
			ch_out <- c(ch_out, as.character(out[idx]))
		} else {
			ch_out <- c(ch_out, out[idx])
		}
		pdata <- rbind(pdata, cbind(pData(data_list[[i]])[idx,], rep(platform, length(idx))))
	}
	
	colnames(pdata)[ncol(pdata)] <- "Platform"
	list("data"=chimera, "outcome"=ch_out, "pData"=pdata)
}
