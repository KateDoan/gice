rootDir <- file.path("code", "scenarios_micro", "micro_v0.2")
source(file.path(rootDir, "microscenario_plots", "plotLYL.R"))
source(file.path(rootDir, "microscenario_plots", "plotPrevalence.R"))
####
df_prop <- readRDS(file.path(inDir, paste0("df_prop_SG", big, ".rds")))
# Prevalence
ylab="Prevalence (%)"
pN <- plotPrev("N", ylab, ylim = c(0, 100), text = "NEVER SMOKERS")
pNleg <- plotPrev("N", ylab, ylim = c(0, 100), text = "NEVER SMOKERS", leg=TRUE)
pC <- plotPrev("C", ylab, ylim = c(0, 30), text = "CIGARETTE ONLY USERS")
pQ <- plotPrev("Q", ylab, ylim = c(0, 30), text = "EX-SMOKERS")
pD <- plotPrev("D", ylab, ylim = c(0, 5), text = "DUAL USERS")
pE <- plotPrev("E", ylab, ylim = c(0, 5), text = "E-CIGARETTE ONLY USERS")
pNandQ <- plotPrev("NandQ", ylab="Prevalence of Never Smokers and Ex-Smokers (%)", ylim = c(0, 100), text = " ")
pCandD <- plotPrev("CandD", ylab="Prevalence of Cigarette and Dual Users (%)", ylim = c(0, 15.5), text = " ")
pCDE <- plotPrev("CDE", ylab="Prevalence of e/Cigarette Users (%)", ylim = c(0, 16), text = " ")
####
df_LYL <- readRDS(file.path(inDir, paste0("df_QALY_SG", big, ".rds")))
# Life Years Lost
pAnnualLYL <- plotLYL('QALY', 1/1000,  
                      ylab = "Annual QALYs Gained (000s)", text = " ",
                      leg=FALSE, ylim = c(-10, 65), y_interval = 0.1e2)
####
plotsSet <- list(pN=pN, pNleg=pNleg, pC=pC, pQ=pQ, pD=pD, pE=pE,
                 pNandQ=pNandQ, pCandD=pCandD, pCDE=pCDE, 
                 pAnnualLYL=pAnnualLYL)