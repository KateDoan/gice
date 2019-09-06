# Testing parameters
ELFAge <- 21
scenario_name <- "E21"
noecig_transformations <- NULL
ecig_transformations <- c("ELFtransform.R")
##########################################################################################
source(file.path(codePiecesDir, "_ecig_core.R"), local=TRUE)
test_graph
