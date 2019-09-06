if(exists("MLAge")) MLAgeOld <- MLAge
MLAge <- 18
scenario_name <- "Tax2"
taxDir <- taxDir2
noecig_transformations <- c("MLAtransform.R", "TAX2transform.R")
##########################################################################################
source(file.path(codePiecesDir, "_noecig_core.R"), local=TRUE)
test_graph
if(exists("MLAgeOld")) MLAge <- MLAgeOld