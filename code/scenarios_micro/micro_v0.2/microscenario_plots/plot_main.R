library(grid)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
######################################################################################################
## Input data directory
rootDir <- file.path("code", "scenarios_micro", "micro_v0.2")
inDir <- file.path(rootDir, "combine", "combined_data")
getPlotPath <- file.path(rootDir, "microscenario_plots", "getPlotSet.R")
## Output plot directory
outDirPlot <- file.path(rootDir, "microscenario_plots", "microPlots")
######################################################################################################
# Plot settings
panelCol <- "white"
backgroundCol <- "white"
foregroundCol <- "black"
#
COLZ <- c(SQ=foregroundCol, MLA21='brown', MLA25='brown', SFG='violetred', TAX5='red', TAX2='red', 
          ELF='darkblue', E25='seagreen1', EP='deepskyblue',
          MLATax5='darkviolet', MLAEP='lightpink',  Tax5EP='lightpink', MLATax5EP='darkorange',
          SFGELF='darkgoldenrod1')
# Linetypes: 1:solid, 2:dashed
LINETYPES <- c(SQ=1, MLA21=1, MLA25=2, SFG=1, ELF=1, E25=2, EP=1, TAX5=1, TAX2=1,
               MLATax5=1, MLAEP=1, Tax5EP=2, MLATax5EP=1, SFGELF=2)
# Shapes: 4:cross, 21:circle, 24:triangle
SHAPES <- c(SQ=4, MLA21=4, MLA25=21, SFG=4, ELF=24, E25=24, EP=24, TAX5=21, TAX2=4, 
            MLATax5=21, MLAEP=24,  Tax5EP=21, MLATax5EP=24, SFGELF=4)
SCENARIOS <- c("SQ", "MLA21", "MLA25", "SFG", "TAX5", "TAX2", "ELF", "E25",
               "EP", "MLATax5", "MLAEP", "Tax5EP", "MLATax5EP", "SFGELF")
LABELS <- c("Status Quo (SQ)", "Minimum Legal Age 21 (MLA21)", "Minimum Legal Age 25 (MLA25)",
            "Smoke Free Generation (SFG)",  "TAX5", "TAX2", 
            "E-cigarette Laissez-Faire (ELF)", "E-cigarette 25 (E25)",
            "E-cigarette Prescription (EP)",
            "MLA21 + TAX5", "MLA21 + EP", "TAX5 + EP", "MLA21 + TAX5 + EP", "Smoke Free + E-cigarette Liberalization")
CHOSEN_TIMES <- seq(2027, 2067, by=10)
#
axisTextSize <- 8
legendTextSize <- 8
annoteTextSize <- 8
######################################################################################################
# Plotting
L <- length(SCENARIOS)
for(i in L:L) {
  cat("Plotting set ", i, "/", L, "\n")
  # chosen = SCENARIOS[1:i]
  chosen = SCENARIOS
  ####
  # Get Plot sets
  small = "SG"
  big = "US"
  source(getPlotPath)
  pUS <- plotsSet
  
  big = "UK"
  source(getPlotPath)
  pUK <- plotsSet
  ####
  # Combine plots
  # Ecig Prevalence
  l <- get_legend(pUS$pN + 
                    theme(legend.position = c(unit(0.5, "npc"), unit(0.5, "npc")),
                          legend.text = element_text(colour=foregroundCol),
                          legend.background = element_rect(fill=backgroundCol, colour=backgroundCol)))
  png(file.path(outDirPlot, "prevalenceEcig", paste0(i,'_','PrevalenceEcig.png')),
       bg=backgroundCol, height=14,width=21,units='cm',res=400,pointsize=10)
  grid.arrange(arrangeGrob(pUS$pD, pUS$pE, top=textGrob("            SGUS Variant",
                                                        gp=gpar(fontface="bold", col=foregroundCol))),
               arrangeGrob(pUK$pD, pUK$pE, top=textGrob("            SGUK Variant",
                                                        gp=gpar(fontface="bold", col=foregroundCol))),
               as_ggplot(l),
               nrow = 1,
               vp = viewport(width = 0.97, height = 0.97))
  dev.off()
  
  # Cigarette Prevalence
  png(file.path(outDirPlot, "prevalenceCig", paste0(i,'_','PrevalenceCig.png')),
       bg=backgroundCol, height=14,width=21,units='cm',res=400,pointsize=10)
  grid.arrange(arrangeGrob(pUS$pCandD, pUS$pCDE, top=textGrob("            SGUS Variant",
                                                              gp=gpar(fontface="bold", col=foregroundCol))),
               arrangeGrob(pUK$pCandD, pUK$pCDE, top=textGrob("            SGUK Variant",
                                                              gp=gpar(fontface="bold", col=foregroundCol))),
               as_ggplot(l),
               nrow = 1,
               vp = viewport(width = 0.97, height = 0.97))
  dev.off()
  
  # Life Years Lost
  l <- get_legend(pUS$pNleg + theme(legend.position = c(unit(0.1, "npc"), unit(0.1, "npc"))))
  png(file.path(outDirPlot, "LYL", paste0(i,'_','LYL.png')),
       bg=backgroundCol, height=7, width=21, units='cm', res=400, pointsize=10)
  g <- grid.arrange(arrangeGrob(pUS$pAnnualLYL, top=textGrob("            SGUS Variant",
                                                             gp=gpar(fontface="bold", col=foregroundCol))),
                    arrangeGrob(pUK$pAnnualLYL, top=textGrob("            SGUK Variant",
                                                             gp=gpar(fontface="bold", col=foregroundCol))),
                    as_ggplot(l),
                    nrow = 1, 
                    vp = viewport(width = 0.97, height = 0.97))
  dev.off()
}