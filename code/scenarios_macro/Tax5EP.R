## Input parameters
scenario_name <- "Tax5EP"
MLAge <- 18
taxDir <- taxDir5
fPres <- 0.5
noecig_transformations <- c("MLAtransform.R", "TAX5transform.R")
ecig_transformations <- c("EPtransform.R")
##########################################################################################
source(file.path(codePiecesDir, "_ecig_core.R"), local=TRUE)