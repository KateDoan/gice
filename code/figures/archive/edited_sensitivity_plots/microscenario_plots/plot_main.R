library(grid)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
######################################################################################################
# Helper functions
startYear <- 2017
d.e <- 0.03
disQALY <- function(df_QALY){
  df_QALY %>%
    mutate(f_discount = 1 / (1 + d.e) ^ (year-startYear)) %>%
    mutate(QALY = f_discount * QALY)
}
######################################################################################################
# In and Out Dir
inDir <- file.path("code", "tables", "combined_microtables", "output", "for_graph")
outDirPlot <- file.path("code/figures/archive/edited_sensitivity_plots/microscenario_plots/outPlot")
######################################################################################################
path_get_plots <-  "code/figures/archive/edited_sensitivity_plots/microscenario_plots/getPlotSet.R"
source(path_get_plots, local=TRUE)
######################################################################################################
ALL_SCENARIOS <- c("SQ", "MLA21", "SFG", "TAX5", "TAX2", "ELF", "E25",
                   "EP", "MLATax5", "MLAEP", "Tax5EP", "MLATax5EP", "SFGELF")
chosen <- ALL_SCENARIOS 
df_prop_US <- readRDS(file.path(inDir, paste0("df_prop_SG", "US", ".rds")))
df_LYL_US <- readRDS(file.path(inDir, paste0("df_QALY_SG", "US", ".rds")))

df_prop_UK <- readRDS(file.path(inDir, paste0("df_prop_SG", "UK", ".rds")))
df_LYL_UK <- readRDS(file.path(inDir, paste0("df_QALY_SG", "UK", ".rds")))

df_prop_JP <- readRDS(file.path(inDir, paste0("df_prop_SG", "JP", ".rds")))
df_LYL_JP <- readRDS(file.path(inDir, paste0("df_QALY_SG", "JP", ".rds")))

pUS <- getPlotsEdited(chosen, df_prop_US, df_LYL_US)
pUK <- getPlotsEdited(chosen, df_prop_UK, df_LYL_UK)
pJP <- getPlotsEdited(chosen, df_prop_JP, df_LYL_JP)
pUSdis <- getPlotsEdited(chosen, df_prop_US, disQALY(df_LYL_US))
pUKdis <- getPlotsEdited(chosen, df_prop_UK, disQALY(df_LYL_UK))
pJPdis <- getPlotsEdited(chosen, df_prop_JP, disQALY(df_LYL_JP))
######################################################################################################
# Main plots
# Plot settings
panelCol <- "white"
backgroundCol <- "white"
foregroundCol <- "black"

l <- get_legend(pUS$pN + 
                  theme(legend.position = c(unit(0.5, "npc"), unit(0.5, "npc")),
                        legend.text = element_text(colour=foregroundCol),
                        legend.background = element_rect(fill=backgroundCol, colour=backgroundCol)))

# Plot of CD and CDE Basic
# Cigarette Prevalence
# png(file.path(outDirPlot, paste0('PrevalenceCigBasic.png')),
#     bg=backgroundCol, height=14,width=21,units='cm',res=400,pointsize=10)
jpeg(file.path(outDirPlot, "jpg", paste0('PrevalenceCigBasic.jpeg')),
    bg=backgroundCol, height=14,width=21,units='cm',res=400,pointsize=10)
grid.arrange(arrangeGrob(pUSbasic$pCandDleg, pUSbasic$pCDE, top=textGrob("            SGUS Variant",
                                                         gp=gpar(fontface="bold", col=foregroundCol))),
             arrangeGrob(pUKbasic$pCandD, pUKbasic$pCDE, top=textGrob("            SGUK Variant",
                                                         gp=gpar(fontface="bold", col=foregroundCol))),
             arrangeGrob(pJPbasic$pCandD, pJPbasic$pCDE, top=textGrob("            SGJP Variant",
                                                         gp=gpar(fontface="bold", col=foregroundCol))),
             nrow = 1,
             vp = viewport(width = 0.97, height = 0.97))
dev.off()

# Plot of CD and CDE Add
# Cigarette Prevalence
# png(file.path(outDirPlot, paste0('PrevalenceCigAdd.png')),
#     bg=backgroundCol, height=14,width=21,units='cm',res=400,pointsize=10)
jpeg(file.path(outDirPlot, "jpg", paste0('PrevalenceCigAdd.jpeg')),
    bg=backgroundCol, height=14,width=21,units='cm',res=400,pointsize=10)
grid.arrange(arrangeGrob(pUS$pCandDleg, pUS$pCDE, top=textGrob("            SGUS Variant",
                                                                         gp=gpar(fontface="bold", col=foregroundCol))),
             arrangeGrob(pUK$pCandD, pUK$pCDE, top=textGrob("            SGUK Variant",
                                                                      gp=gpar(fontface="bold", col=foregroundCol))),
             arrangeGrob(pJP$pCandD, pJP$pCDE, top=textGrob("            SGJP Variant",
                                                            gp=gpar(fontface="bold", col=foregroundCol))),
             nrow = 1,
             vp = viewport(width = 0.97, height = 0.97))
dev.off()

# Plot of E and D
# E-cigarette Prevalence
# png(file.path(outDirPlot, paste0('PrevalenceEcig.png')),
#     bg=backgroundCol, height=14, width=21,units='cm',res=400,pointsize=10)
jpeg(file.path(outDirPlot, "jpg", paste0('PrevalenceEcig.jpeg')),
    bg=backgroundCol, height=14, width=21,units='cm',res=400,pointsize=10)
grid.arrange(arrangeGrob(pUS$pD1 + theme(legend.position = c(0.0, 0.35)), pUS$pE, 
                         top=textGrob("            SGUS Variant",
                                                               gp=gpar(fontface="bold", col=foregroundCol))),
             arrangeGrob(pUK$pD2 + theme(legend.position = c(0.0, 0.55)), pUK$pE, 
                         top=textGrob("            SGUK Variant",
                                                            gp=gpar(fontface="bold", col=foregroundCol))),
             arrangeGrob(pJP$pD, pJP$pE, 
                         top=textGrob("            SGJP Variant",
                                                            gp=gpar(fontface="bold", col=foregroundCol))),
             nrow = 1,
             vp = viewport(width = 0.97, height = 0.97))
dev.off()

# Plot of QALY Add
# Life Years Lost
QALYleg1 <- get_legend(pUS$pCandDleg1 + theme(legend.position = c(unit(0.2, "npc"), unit(0, "npc"))))
QALYleg2 <- get_legend(pUS$pCandDleg2 + theme(legend.position = c(unit(0.2, "npc"), unit(0, "npc"))))
QALYleg3 <- get_legend(pUS$pCandDleg3 + theme(legend.position = c(unit(0.35, "npc"), unit(0, "npc"))))
# png(file.path(outDirPlot, paste0('QALY.png')),
#     bg=backgroundCol, height=9.5, width=21, units='cm', res=400, pointsize=10)
jpeg(file.path(outDirPlot, "jpg", paste0('QALY.jpeg')),
    bg=backgroundCol, height=9.5, width=21, units='cm', res=400, pointsize=10)
g <- grid.arrange(arrangeGrob(pUS$pAnnualLYL, top=textGrob("            SGUS Variant",
                                                           gp=gpar(fontface="bold", col=foregroundCol))),
                  arrangeGrob(pUK$pAnnualLYL, top=textGrob("            SGUK Variant",
                                                           gp=gpar(fontface="bold", col=foregroundCol))),
                  arrangeGrob(pJP$pAnnualLYL, top=textGrob("            SGJP Variant",
                                                           gp=gpar(fontface="bold", col=foregroundCol))),
                  as_ggplot(QALYleg1),
                  as_ggplot(QALYleg2),
                  as_ggplot(QALYleg3),
                  heights=c(0.77, 0.23),
                  nrow = 2,
                  vp = viewport(width = 0.97, height = 0.97))
dev.off()

jpeg(file.path(outDirPlot, "jpg", paste0('QALYdis.jpeg')),
     bg=backgroundCol, height=9.5, width=21, units='cm', res=400, pointsize=10)
g <- grid.arrange(arrangeGrob(pUSdis$pAnnualLYLdis, top=textGrob("            SGUS Variant",
                                                           gp=gpar(fontface="bold", col=foregroundCol))),
                  arrangeGrob(pUKdis$pAnnualLYLdis, top=textGrob("            SGUK Variant",
                                                           gp=gpar(fontface="bold", col=foregroundCol))),
                  arrangeGrob(pJPdis$pAnnualLYLdis, top=textGrob("            SGJP Variant",
                                                           gp=gpar(fontface="bold", col=foregroundCol))),
                  as_ggplot(QALYleg1),
                  as_ggplot(QALYleg2),
                  as_ggplot(QALYleg3),
                  heights=c(0.77, 0.23),
                  nrow = 2,
                  vp = viewport(width = 0.97, height = 0.97))
dev.off()
######################################################################################################
