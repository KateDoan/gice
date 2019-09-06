getPlots <- function(chosen, df_prop, df_LYL){
  # Plot settings
  panelCol <- "white"
  backgroundCol <- "white"
  foregroundCol <- "black"
  #
  ALL_SCENARIOS <- c("SQ", "MLA21", "MLA25", "SFG", "TAX5", "TAX2", "ELF", "E25",
                 "EP", "MLATax5", "MLAEP", "Tax5EP", "MLATax5EP", "SFGELF")
  selected_idx <- match(chosen, ALL_SCENARIOS)
  SCENARIOS <- ALL_SCENARIOS[selected_idx]
  COLZ <- c(SQ=foregroundCol, MLA21='brown', MLA25='brown', SFG='violetred', TAX5='red', TAX2='red', 
            ELF='darkblue', E25='seagreen1', EP='deepskyblue',
            MLATax5='darkviolet', MLAEP='orchid3',  Tax5EP='deepskyblue', MLATax5EP='darkorange',
            SFGELF='darkblue')[selected_idx]
  FILZ <- c(SQ=foregroundCol, MLA21='white', MLA25='brown', SFG='violetred', TAX5='white', TAX2='red', 
            ELF='darkblue', E25='seagreen1', EP='white',
            MLATax5='darkviolet', MLAEP='white',  Tax5EP='white', MLATax5EP='darkorange',
            SFGELF='white')[selected_idx]
  # Linetypes: 1:solid, 2:dashed
  LINETYPES <- c(SQ=1, MLA21=1, MLA25=1, SFG=1, TAX5=1, TAX2=1, ELF=1, E25=2, EP=1, 
                 MLATax5=2, MLAEP=2, Tax5EP=2, MLATax5EP=2, SFGELF=2)[selected_idx]
  # Shapes: 4:cross, 21:circle, 22:square, 23:diamond, 24:triangle
  SHAPES <- c(SQ=4, MLA21=21, MLA25=21, SFG=21,  TAX5=22, TAX2=22, ELF=24, E25=24, EP=24,
              MLATax5=21, MLAEP=24,  Tax5EP=22, MLATax5EP=22, SFGELF=23)[selected_idx]
  LABELS <- c("Status Quo (SQ)", "Minimum Legal Age (MLA)", "Minimum Legal Age 25 (MLA25)",
              "Smoke Free Generation (SFG)",  "TAX5", "TAX2", 
              "E-cigarette Laissez-Faire (ELF)", "E-cigarette 25 (E25)",
              "E-cigarette Prescription (EP)",
              "MLA + TAX5", "MLA + EP", "TAX5 + EP", "MLA + TAX5 + EP", "SFG + ELF")[selected_idx]
  CHOSEN_TIMES <- seq(2027, 2067, by=10)
  #
  axisTextSize <- 8
  legendTextSize <- 8
  annoteTextSize <- 8
  
  source(file.path(codeDir, "plotLYL.R"), local=TRUE)
  source(file.path(codeDir, "plotPrevalence.R"), local=TRUE)
  
  # Prevalence
  ylab="Prevalence (%)"
  pN <- plotPrev(chosen, df_prop, "N", ylab, ylim = c(0, 100), text = "NEVER SMOKERS")
  pC <- plotPrev(chosen, df_prop, "C", ylab, ylim = c(0, 30), text = "CIGARETTE ONLY USERS")
  pQ <- plotPrev(chosen, df_prop, "Q", ylab, ylim = c(0, 30), text = "EX-SMOKERS")
  pD <- plotPrev(chosen, df_prop, "D", ylab, ylim = c(0, 5), text = "DUAL USERS")
  pE <- plotPrev(chosen, df_prop, "E", ylab, ylim = c(0, 5), text = "E-CIGARETTE ONLY USERS")
  pNandQ <- plotPrev(chosen, df_prop, "NandQ", ylab="Prevalence of Never Smokers and Ex-Smokers (%)", ylim = c(0, 100), text = " ")
  pCandD <- plotPrev(chosen, df_prop, "CandD", ylab="Prevalence of Cigarette and Dual Users (%)", ylim = c(0, 15.5), text = " ")
  pCDE <- plotPrev(chosen, df_prop, "CDE", ylab="Prevalence of e/Cigarette Users (%)", ylim = c(0, 16.5), text = " ")
  pCandDleg <- plotPrev(chosen, df_prop, "CandD", ylab="Prevalence of Cigarette and Dual Users (%)", ylim = c(0, 15.5), text = " ", leg=TRUE)
  pCDEleg <- plotPrev(chosen, df_prop, "CDE", ylab="Prevalence of e/Cigarette Users (%)", ylim = c(0, 16.5), text = " ", leg=TRUE)
 
  ####
  # Life Years Lost
  pAnnualLYL <- plotLYL(chosen, df_LYL, 'QALY', 1/1000,  
                        ylab = "Annual QALYs Gained (000s)", text = " ",
                        leg=FALSE, ylim = c(-10, 52), y_interval = 0.1e2)
  
  pAnnualLYLdis <- plotLYL(chosen, df_LYL, 'QALY', 1/1000,  
                        ylab = "Annual QALYs Gained (000s)", text = " ",
                        leg=FALSE, ylim = c(-5, 15), y_interval = 0.1e2)
  ####
  plotsSet <- list(pN=pN, pC=pC, pQ=pQ, pD=pD, pE=pE,
                   pNandQ=pNandQ, pCandD=pCandD, pCDE=pCDE, 
                   pCandDleg=pCandDleg, pCDEleg=pCDEleg,
                   pAnnualLYL=pAnnualLYL,
                   pAnnualLYLdis=pAnnualLYLdis)
  
  plotsSet
}