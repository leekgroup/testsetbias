Thanks for replicating the analysis from Patil, et. Al. (2014).

1. Please ensure that the necessary packages are installed on your
system.

	The following can be installed from CRAN:

	colorspace, ggplot2, plyr, knitr, tables

	The following can be installed from Bioconductor using the code
	source("http://bioconductor.org/biocLite.R")
	biocLite(PAKCAGE_NAME)

	genefu, affyPLM, pamr, survcomp

	The following can be installed from github using the devtools package:
	e.g. install_github("MetaGx", username="bhaibeka", ref="master")	

	MetaGx

	(See installation instructions at)
	https://github.com/jtleek/tspreg
	https://github.com/bhaibeka/MetaGx

2. Once all dependencies are installed, open R and make the test_set_analysis directory
your working directory. Then run the following commands

	library(knitr)
	knit2html("testsetbias.Rmd")

The whole processs should take about 35 minutes to run. The resulting file
"testsetbias.html" can be opened in a web browser to view the analysis
and figures from the paper.

Thank you!
