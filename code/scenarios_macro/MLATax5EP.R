## Input parameters
scenario_name <- "MLATax5EP"
MLAge <- 21
fMLA <- fMLA21
taxDir <- taxDir5
fPres <- 0.5
noecig_transformations <- c("MLAtransform.R", "TAX5transform.R")
ecig_transformations <- c("EPtransform.R")
##########################################################################################
source(file.path(codePiecesDir, "_ecig_core.R"), local=TRUE)
test_graph