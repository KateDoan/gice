# Testing parameters
ELFAge <- 25
scenario_name <- "E25"
# ExcessRisk_D <- "geom_mean"
noecig_transformations <- NULL
ecig_transformations <- c("ELFtransform.R")
##########################################################################################
source(file.path(codePiecesDir, "_ecig_core.R"), local=TRUE)
test_graph
