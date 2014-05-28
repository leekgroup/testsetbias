library(devtools)
#install_github("survcomp", username="bhaibeka", ref="master")
#install_github("MetaGx", username="bhaibeka", ref="master")


datasets <- read.csv(system.file(file.path("extdata", "datasets.csv"), package="MetaGx"), stringsAsFactors=FALSE)
rownames(datasets) <- as.character(datasets[ , "Dataset.ID"])
system.time(datall2 <- MetaGx::getBrCaData(datasets=datasets[, , drop=FALSE], resdir="cache", remove.duplicates=FALSE, topvar.genes=1000, duplicates.cor=0.98, sbt.model="pam50", merging.method="union", merging.std="none", nthread=8, verbose=TRUE))
eset.all <- datall2$each

annotations <- c("agilentRosetta","affy hgu95", "Stanford spotted cDNA microarray", "NCI spotted cDNA microarray", "custom spotted cDNA microarray", "affy hgu133plus2", "affy hgu133a", "affy hgu133a", "affy hgu133a", "affy hgu133plus2", "Agilent G4110A", "Swegene UniGene188", "affy hgu133plus2", "affy hgu133a", "affy hgu95", "Swegene H_v2.1.1_27k", "DKFZ Operon 4.0_35k", "affy hgu133a", "affy hgu133a", "affy hgu133plus2", "Illumina human-6 v2.0 beadchip", "Sentrix human-6 v2.0 beadchip", "affy hgu133plus2", "affy hgu133plus2", "Agilent G4110A/B/Custom", "affy hgu133plus2", "affy hgu133plus2", "Illumina HT-12V3.0")

for(i in 1:length(eset.all)){
	pData(eset.all[[i]])$subtype <- MetaGx::getSubtype(eset=eset.all[[i]], method="class")
	annotation(eset.all[[i]]) <- annotations[i]
}
