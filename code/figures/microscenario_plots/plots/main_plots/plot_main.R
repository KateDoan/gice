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
codeDir <- file.path("code/figures/microscenario_plots/plots/main_plots")
outDirPlot <- file.path(codeDir, "outPlot")
######################################################################################################
path_get_plots <-  file.path(codeDir, "getPlotSet.R")
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
#pUSdis <- getPlotsEdited(chosen, df_prop_US, disQALY(df_LYL_US))
#pUKdis <- getPlotsEdited(chosen, df_prop_UK, disQALY(df_LYL_UK))
#pJPdis <- getPlotsEdited(chosen, df_prop_JP, disQALY(df_LYL_JP))
######################################################################################################
# Main plots
# Plot settings
panelCol <- "white"
backgroundCol <- "white"
foregroundCol <- "black"

# l <- get_legend(pUS$pD + 
#                   theme(legend.position = c(unit(0.5, "npc"), unit(0.5, "npc")),
#                         legend.text = element_text(colour=foregroundCol),
#                         legend.background = element_rect(fill=backgroundCol, colour=backgroundCol)))

# Plot of CD and CDE Basic
# Cigarette Prevalence
# png(file.path(outDirPlot, paste0('PrevalenceCigBasic.png')),
#     bg=backgroundCol, height=14,width=21,units='cm',res=400,pointsize=10)
jpeg(file.path(outDirPlot, "jpg", paste0('PrevalenceCigBasic.jpeg')),
    bg=backgroundCol, height=14,width=21,units='cm',res=400,pointsize=10)
grid.arrange(arrangeGrob(pUS$pCandDleg_base, pUS$pCDE_base, top=textGrob("            SGUS Variant",
                                                         gp=gpar(fontface="bold", col=foregroundCol))),
             arrangeGrob(pUK$pCandD_base, pUK$pCDE_base, top=textGrob("            SGUK Variant",
                                                         gp=gpar(fontface="bold", col=foregroundCol))),
             arrangeGrob(pJP$pCandD_base, pJP$pCDE_base, top=textGrob("            SGJP Variant",
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
grid.arrange(arrangeGrob(pUS$pCandDleg_com, pUS$pCDE_com, top=textGrob("            SGUS Variant",
                                                                         gp=gpar(fontface="bold", col=foregroundCol))),
             arrangeGrob(pUK$pCandD_com, pUK$pCDE_com, top=textGrob("            SGUK Variant",
                                                                      gp=gpar(fontface="bold", col=foregroundCol))),
             arrangeGrob(pJP$pCandD_com, pJP$pCDE_com, top=textGrob("            SGJP Variant",
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
grid.arrange(arrangeGrob(pUS$pD1leg + theme(legend.position = c(0.0, 0.35)), pUS$pE, 
                         top=textGrob("            SGUS Variant",
                                                               gp=gpar(fontface="bold", col=foregroundCol))),
             arrangeGrob(pUK$pD2leg + theme(legend.position = c(0.0, 0.55)), pUK$pE, 
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
g <- grid.arrange(arrangeGrob(pUS$pAnnualQALY, top=textGrob("            SGUS Variant",
                                                           gp=gpar(fontface="bold", col=foregroundCol))),
                  arrangeGrob(pUK$pAnnualQALY, top=textGrob("            SGUK Variant",
                                                           gp=gpar(fontface="bold", col=foregroundCol))),
                  arrangeGrob(pJP$pAnnualQALY, top=textGrob("            SGJP Variant",
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
g <- grid.arrange(arrangeGrob(pUS$pAnnualQALYdis, top=textGrob("            SGUS Variant",
                                                           gp=gpar(fontface="bold", col=foregroundCol))),
                  arrangeGrob(pUK$pAnnualQALYdis, top=textGrob("            SGUK Variant",
                                                           gp=gpar(fontface="bold", col=foregroundCol))),
                  arrangeGrob(pJP$pAnnualQALYdis, top=textGrob("            SGJP Variant",
                                                           gp=gpar(fontface="bold", col=foregroundCol))),
                  as_ggplot(QALYleg1),
                  as_ggplot(QALYleg2),
                  as_ggplot(QALYleg3),
                  heights=c(0.77, 0.23),
                  nrow = 2,
                  vp = viewport(width = 0.97, height = 0.97))
dev.off()
######################################################################################################
