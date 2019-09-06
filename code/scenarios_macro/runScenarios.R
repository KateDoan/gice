library(tidyverse)
library(ggplot2)
####################################################################################################
# Helper function
# This functions will output the output the macro-calculation results  
# for the Smoking prevalence and Life Years Lost to the output folder
# The input of the functions are:
# small: the country serving as the data source for the transition probabilities between the 3 states N, C, Q
# big: the country serving as the data source for the transition probabilities to and from ecig use
# Note: Currently, small can only be "SG", big can be either "US", "UK", or "JP"
runScenarios <- function(small, big) {
  # Parameters
  # small = "SG"
  # big = "US"
  ageInitCig = 17
  SFGBirth = 2000
  scenarioDir <- file.path("code", "scenarios_macro")
  fMLA <- 2/3
  fMLA21 <- 2/3
  fMLA25 <- 1/2
  
  ## In directories
  datDir <- file.path("data", "singapore")
  smallMatDir <- file.path("code", "transitions", "sin_jags", "singapore_mcmc_free9", "output")
  fpopDir <- file.path(scenarioDir, "util", "util_output")
  bigMatDirUS <- file.path("code", "transitions", "path1_pool", "transition_output")
  bigMatDirUK <- file.path("code", "transitions", "uk_jags", "ukv", "output")
  bigMatDirJP <- file.path("code", "transitions", "japan_jags", "output")
  bigMatDirOW <- file.path("data", "OliverWyman")
  taxDir5 <- file.path("code", "transitions", "sin_tax_coef", "outputTax5")
  taxDir2 <- file.path("code", "transitions", "sin_tax_coef", "outputTax2")
  
  ## Out directories
  outDirNoEcig <- file.path(scenarioDir, "output", small)
  outDirEcig <- file.path(scenarioDir, "output", paste0(small, big))
  
  # codePiecesDir
  codePiecesDir <- file.path(scenarioDir, "util", "scenario_components")
  
  # 
  # # Basic scenarios
  source(file.path(scenarioDir,"SQ.R"), local=TRUE)
  source(file.path(scenarioDir,"MLA21.R"), local=TRUE)
  source(file.path(scenarioDir,"MLA25.R"), local=TRUE)
  source(file.path(scenarioDir,"SFG.R"), local=TRUE)
  source(file.path(scenarioDir,"Tax2.R"), local=TRUE)
  source(file.path(scenarioDir,"Tax5.R"), local=TRUE)
  source(file.path(scenarioDir,"MLATax5.R"), local=TRUE)
  
  # # Ecig scenarios
  ExcessRisk_E = 0.10
  ExcessRisk_D = "geom_mean"
  fInitEcig = 1
  fInitEcigPres = 0
  fPres = 0.5
  #
  source(file.path(scenarioDir,"ELF.R"), local=TRUE)
  source(file.path(scenarioDir,"ELF21.R"), local=TRUE)
  source(file.path(scenarioDir,"ELF25.R"), local=TRUE)
  source(file.path(scenarioDir,"EP.R"), local=TRUE)
  source(file.path(scenarioDir,"MLAEP.R"), local=TRUE)
  source(file.path(scenarioDir,"MLATax5EP.R"), local=TRUE)
  source(file.path(scenarioDir,"Tax5EP.R"), local=TRUE)
  source(file.path(scenarioDir,"SFGELF.R"), local=TRUE)
  # 
  # In and Out Directories
  datDirNoEcig <- outDirNoEcig
  datDirEcig <- outDirEcig
  outDirCombined <- file.path(scenarioDir, "output","combined")
  source(file.path(codePiecesDir, "combine_output.R"), local=TRUE)
}
####################################################################################################
# Get the results for the 3 variants SGUS, SGUK, SGJP
runScenarios("SG", "US") 
runScenarios("SG", "UK") 
runScenarios("SG", "JP")

