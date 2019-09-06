##########################################################################################
library(dplyr)
library(tidyr)
##########################################################################################
source(file.path(codePiecesDir, "ecig_helper_functions.R"), local=TRUE)
source(file.path(codePiecesDir, "set_age_and_year_limit.R"), local=TRUE)
source(file.path(codePiecesDir, "set_population.R"), local=TRUE)
source(file.path(codePiecesDir, "set_RRC_and_RRQ.R"), local=TRUE)
##########################################################################################
# SCENARIO
##########################################################################################
source(file.path(codePiecesDir, "read_SQ_NCQ_mat.R"), local=TRUE)
for(n in noecig_transformations){
  source(file.path(codePiecesDir, n), local=TRUE)
}
source(file.path(codePiecesDir, "calculate_results_with_noecig.R"), local=TRUE)
source(file.path(codePiecesDir, "noecig_summary_and_save_files.R"), local=TRUE)
##########################################################################################
# Test plot
source(file.path(codePiecesDir, "test_plot.R"), local=TRUE)