## Input parameters
scenario_name <- "MLAEP"
MLAge <- 21
fMLA <- fMLA21
fPres <- 0.5
noecig_transformations <- c("MLAtransform.R")
ecig_transformations <- c("EPtransform.R")
##########################################################################################
source(file.path(codePiecesDir, "_ecig_core.R"), local=TRUE)