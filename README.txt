Replicating the analysis from Patil, et. Al. (2014)
Runtime: ~50 minutes
Run environment: R

0. Install git and github so that you can pull these files.
   Alternatively, if you are averse to doing so, a .zip archive
   containing all these materials can be found here: 
   http://biostat.jhsph.edu/~prpatil/testsetbias.zip

1. Please ensure that the necessary packages are installed on your
system.

	The following can be installed from CRAN:

	rpart, rattle, colorspace, ggplot2, plyr, survival, knitr

	The following can be installed from Bioconductor using the code
	source("http://bioconductor.org/biocLite.R")
	biocLite(PAKCAGE_NAME)

	genefu, affyPLM, pamr, survcomp

	The following can be installed from github using the devtools package:
	e.g. install_github("MetaGx", username="bhaibeka", ref="master")	

	tspreg, MetaGx

	(See installation instructions at)
	https://github.com/jtleek/tspreg
	https://github.com/bhaibeka/MetaGx

2. Once all dependencies are installed, open R and make the testsetbias directory
your working directory. Then run the following commands

	library(knitr)
	knit2html("test_set_analysis.Rmd")

The whole processs should take about 50 minutes to run. The resulting file
"test_set_analysis.html" can be opened in a web browser to view the analysis
and figures from the paper. The downloaded data and code chunks will be cached
so that the analysis can be run again and/or modified quickly.

There are two sections that use data from simulations which are currently not 
evaluated (the data are simply provided). You may choose to run those by changing 
the eval flag to TRUE - this will add substantial computation time.

Thank you!