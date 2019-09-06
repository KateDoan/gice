# Testing parameters
ELFAge <- 18
scenario_name <- "ELF"
noecig_transformations <- NULL
ecig_transformations <- c("ELFtransform.R")
##########################################################################################
source(file.path(codePiecesDir, "_ecig_core.R"), local=TRUE)
test_graph
