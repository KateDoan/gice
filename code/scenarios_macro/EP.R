## Input parameters
scenario_name <- "EP"
noecig_transformations <- NULL
ecig_transformations <- c("EPtransform.R")
##########################################################################################
source(file.path(codePiecesDir, "_ecig_core.R"), local=TRUE)
test_graph