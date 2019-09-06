path_plot_lyl <- "code/figures/archive/edited_sensitivity_plots/microscenario_plots/plotLYL.R"
path_plot_prev <- "code/figures/archive/edited_sensitivity_plots/microscenario_plots/plotPrevalence.R"
    
getPlotsEdited <- function(chosen, df_prop, df_LYL){
  SINGLES <- intersect(c("SQ", "MLA21", "SFG", "TAX5", "TAX2", "ELF", "EP", "E25"), chosen)
  COMBS <- intersect(c("MLATax5", "MLAEP", "Tax5EP", "MLATax5EP", "SFGELF"), chosen) 
  # Plot settings
  panelCol <- "white"
  backgroundCol <- "white"
  foregroundCol <- "black"
  #
  ALL_SCENARIOS <- c("SQ", "MLA21", "SFG", "TAX5", "TAX2", "ELF", "E25",
                     "EP", "MLATax5", "MLAEP", "Tax5EP", "MLATax5EP", "SFGELF")
  selected_idx <- match(chosen, ALL_SCENARIOS)
  SCENARIOS <- ALL_SCENARIOS[selected_idx]
  COLZ <- c(SQ=foregroundCol, MLA21='brown', SFG='violetred', TAX5='red', TAX2='red', 
            ELF='darkblue', E25='seagreen1', EP='deepskyblue',
            MLATax5='darkviolet', MLAEP='orchid3',  Tax5EP='deepskyblue', MLATax5EP='darkorange',
            SFGELF='darkblue')[selected_idx]
  FILZ <- c(SQ=foregroundCol, MLA21='white', SFG='violetred', TAX5='white', TAX2='red', 
            ELF='darkblue', E25='seagreen1', EP='white',
            MLATax5='darkviolet', MLAEP='white',  Tax5EP='white', MLATax5EP='darkorange',
            SFGELF='white')[selected_idx]
  # Linetypes: 1:solid, 2:dashed
  LINETYPES <- c(SQ=1, MLA21=1, SFG=1, TAX5=1, TAX2=1, ELF=1, E25=2, EP=1, 
                 MLATax5=2, MLAEP=2, Tax5EP=2, MLATax5EP=2, SFGELF=2)[selected_idx]
  # Shapes: 4:cross, 21:circle, 22:square, 23:diamond, 24:triangle
  SHAPES <- c(SQ=4, MLA21=21, SFG=21,  TAX5=22, TAX2=22, ELF=24, E25=24, EP=24,
              MLATax5=21, MLAEP=24,  Tax5EP=22, MLATax5EP=22, SFGELF=23)[selected_idx]
  LABELS <- c(SQ="Status Quo (SQ)", MLA21="Minimum Legal Age (MLA)",
              SFG="Smoke Free Generation (SFG)",  TAX5="TAX5", TAX2="TAX2", 
              ELF="E-cigarette Laissez-Faire (ELF)", E25="E-cigarette 25 (E25)",
              EP="E-cigarette Prescription (EP)",
              MLATax5="MLA + TAX5", MLAEP="MLA + EP", Tax5EP="TAX5 + EP", 
              MLATax5EP="MLA + TAX5 + EP", SFGELF="SFG + ELF")[selected_idx]
  ALPHAS1 <- c(SQ=0.1, MLA21=0.1, SFG=0.1,  TAX5=0.1, TAX2=0.1, ELF=0.1, E25=0.1, EP=0.1,
              MLATax5=1, MLAEP=1,  Tax5EP=1, MLATax5EP=1, SFGELF=1)[selected_idx]
  ALPHAS2 <- c(SQ=1, MLA21=1,  SFG=1,  TAX5=1, TAX2=1, ELF=1, E25=1, EP=1,
              MLATax5=1, MLAEP=1,  Tax5EP=1, MLATax5EP=1, SFGELF=1)[selected_idx]
  CHOSEN_TIMES <- seq(2027, 2067, by=10)
  #
  axisTextSize <- 8
  legendTextSize <- 8
  annoteTextSize <- 8
  
  source(path_plot_lyl, local=TRUE)
  source(path_plot_prev, local=TRUE)
  
  # Prevalence
  ylab = "Prevalence (%)"
  #pN <- plotPrevEdited(chosen, COMBS, ALPHAS1, df_prop, "N", ylab, ylim = c(0, 100), text = "NEVER SMOKERS")
  #pC <- plotPrevEdited(chosen, COMBS, ALPHAS1, df_prop, "C", ylab, ylim = c(0, 30), text = "CIGARETTE ONLY USERS")
  #pQ <- plotPrevEdited(chosen, COMBS, ALPHAS1, df_prop, "Q", ylab, ylim = c(0, 30), text = "EX-SMOKERS")
  #pNandQ <- plotPrevEdited(chosen, COMBS, ALPHAS1, df_prop, "NandQ", ylab="Prevalence of Never Smokers and Ex-Smokers (%)", ylim = c(0, 100), text = " ")
  #pCandD <- plotPrevEdited(chosen, COMBS, ALPHAS1, df_prop, "CandD", ylab="Prevalence of Cigarette and Dual Users (%)", ylim = c(0, 15.5), text = " ")
  #pCDE <- plotPrevEdited(chosen, COMBS, ALPHAS1, df_prop, "CDE", ylab="Prevalence of e/Cigarette Users (%)", ylim = c(0, 16.5), text = " ")
  
  pCandDleg_base <- plotPrevEdited(SINGLES, SINGLES, ALPHAS2, df_prop, "CandD", ylab="Prevalence of Cigarette and Dual Users (%)", ylim = c(0, 15.5), text = " ", leg=TRUE)
  pCandDleg_com <- plotPrevEdited(chosen, COMBS, ALPHAS1, df_prop, "CandD", ylab="Prevalence of Cigarette and Dual Users (%)", ylim = c(0, 15.5), text = " ", leg=TRUE)
  pCandDleg1 <- plotPrevEdited(chosen, SCENARIOS[1:5], ALPHAS2, df_prop, "CandD", ylab="Prevalence of Cigarette and Dual Users (%)", ylim = c(0, 15.5), text = " ", leg=TRUE)
  pCandDleg2 <- plotPrevEdited(chosen, SCENARIOS[6:9], ALPHAS2, df_prop, "CandD", ylab="Prevalence of Cigarette and Dual Users (%)", ylim = c(0, 15.5), text = " ", leg=TRUE)
  pCandDleg3 <- plotPrevEdited(chosen, SCENARIOS[10:13], ALPHAS2, df_prop, "CandD", ylab="Prevalence of Cigarette and Dual Users (%)", ylim = c(0, 15.5), text = " ", leg=TRUE)
  pCandD_base <- plotPrevEdited(SINGLES, SINGLES, ALPHAS2, df_prop, "CandD", ylab="Prevalence of Cigarette and Dual Users (%)", ylim = c(0, 15.5), text = " ")
  pCDE_base <- plotPrevEdited(SINGLES, SINGLES, ALPHAS2, df_prop, "CDE", ylab="Prevalence of e/Cigarette Users (%)", ylim = c(0, 16.5), text = " ")
  pCandD_com <- plotPrevEdited(chosen, COMBS, ALPHAS1, df_prop, "CandD", ylab="Prevalence of Cigarette and Dual Users (%)", ylim = c(0, 15.5), text = " ")
  pCDE_com <- plotPrevEdited(chosen, COMBS, ALPHAS1, df_prop, "CDE", ylab="Prevalence of e/Cigarette Users (%)", ylim = c(0, 16.5), text = " ")
  
  pD1leg <- plotPrevEdited(chosen, SINGLES, ALPHAS2, df_prop, "D", ylab, ylim = c(0, 5), text = "DUAL USERS", leg=TRUE)
  pD2leg <- plotPrevEdited(chosen, COMBS, ALPHAS2, df_prop, "D", ylab, ylim = c(0, 5), text = "DUAL USERS", leg=TRUE)
  pD <- plotPrevEdited(chosen, chosen, ALPHAS2, df_prop, "D", ylab, ylim = c(0, 5), text = "DUAL USERS")
  pE <- plotPrevEdited(chosen, chosen, ALPHAS2, df_prop, "E", ylab, ylim = c(0, 5), text = "E-CIGARETTE ONLY USERS")
  
  ####
  # Life Years Lost
  pAnnualQALY <- plotLYL(chosen, df_LYL, 'QALY', 1/1000,  
                        ylab = "Annual QALYs Gained (000s)", text = " ",
                        leg=FALSE, ylim = c(-10, 52), y_interval = 0.1e2)
  pAnnualQALYdis <- plotLYL(chosen, disQALY(df_LYL), 'QALY', 1/1000,  
                        ylab = "Annual QALYs Gained (000s)", text = " ",
                        leg=FALSE, ylim = c(-5, 15), y_interval = 0.1e2)
  ####
  plotsSet <- list(pCandDleg_base=pCandDleg_base, 
                   pCandDleg_com=pCandDleg_com,
                   pCandDleg1=pCandDleg1,
                   pCandDleg2=pCandDleg2,
                   pCandDleg3=pCandDleg3,
                   pCandD_base=pCandD_base,
                   pCDE_base=pCDE_base,
                   pCandD_com=pCandD_com,
                   pCDE_com=pCDE_com,
                   pD1leg=pD1leg,
                   pD2leg=pD2leg,
                   pD=pD,
                   pE=pE,
                   pAnnualQALY=pAnnualQALY,
                   pAnnualQALYdis=pAnnualQALYdis)
  
  plotsSet
}