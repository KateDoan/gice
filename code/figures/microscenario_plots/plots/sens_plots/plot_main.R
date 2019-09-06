library(grid)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
######################################################################################################
# In and Out Dir
codeDir <- file.path("code", "figures", "microscenario_plots", "plots", "sens_plots")
inDir <- file.path("code", "tables", "combined_microtables", "output", "for_graph")
inDirAllEN <- file.path("code/scenarios_micro/micro_v0.2/combine/combined_data")
inDirSens <- file.path("code/figures/microscenario_plots/combine/output/sensitivity_df")
outDirPlot <- file.path(codeDir, "outPlot")
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
source(file.path(codeDir, "getPlotSet.R"), local=TRUE)
SCENARIOS <- c("SQ", "MLA21", "SFG", "TAX5", "TAX2", "ELF", "EP", "E25",
               "MLATax5", "MLAEP", "Tax5EP", "MLATax5EP", "SFGELF")
SCENARIOS_BASIC <- c("SQ", "MLA21", "SFG", "TAX5", "TAX2", "ELF", "EP", "E25")
chosen <- SCENARIOS
chosen_basic <- SCENARIOS_BASIC
df_prop_US <- readRDS(file.path(inDir, paste0("df_prop_SG", "US", ".rds")))
df_LYL_US <- readRDS(file.path(inDir, paste0("df_QALY_SG", "US", ".rds")))
pUS <- getPlots(chosen, df_prop_US, df_LYL_US)

df_prop_UK <- readRDS(file.path(inDir, paste0("df_prop_SG", "UK", ".rds")))
df_LYL_UK <- readRDS(file.path(inDir, paste0("df_QALY_SG", "UK", ".rds")))
pUK <- getPlots(chosen, df_prop_UK, df_LYL_UK)

df_prop_JP <- readRDS(file.path(inDir, paste0("df_prop_SG", "JP", ".rds")))
df_LYL_JP <- readRDS(file.path(inDir, paste0("df_QALY_SG", "JP", ".rds")))
pJP <- getPlots(chosen, df_prop_JP, df_LYL_JP)
######################################################################################################
pUSbasic <- getPlots(chosen_basic, df_prop_US, df_LYL_US)
pUKbasic <- getPlots(chosen_basic, df_prop_UK, df_LYL_UK)
pJPbasic <- getPlots(chosen_basic, df_prop_JP, df_LYL_JP)
######################################################################################################
## Sensitivity plots
# Sens: more NE for the UK
df_prop_UKNE2x <- readRDS(file.path(inDirSens, "df_prop_moreNE2x.rds"))
df_LYL_UKNE2x <- readRDS(file.path(inDirSens, "df_QALY_moreNE2x.rds"))
pUKNE2x <- getPlots(chosen, df_prop_UKNE2x, df_LYL_UKNE2x)

df_prop_UKNE3x <- readRDS(file.path(inDirSens, "df_prop_moreNE3x.rds"))
df_LYL_UKNE3x <- readRDS(file.path(inDirSens, "df_QALY_moreNE3x.rds"))
pUKNE3x <- getPlots(chosen, df_prop_UKNE3x, df_LYL_UKNE3x)

# Sens: more CECD for the US
df_prop_USCE2x <- readRDS(file.path(inDirSens, "df_prop_moreCECD2x.rds"))
df_LYL_USCE2x <- readRDS(file.path(inDirSens, "df_QALY_moreCECD2x.rds"))
pUSCE2x <- getPlots(chosen, df_prop_USCE2x, df_LYL_USCE2x)

df_prop_USCE3x <- readRDS(file.path(inDirSens, "df_prop_moreCECD3x.rds"))
df_LYL_USCE3x <- readRDS(file.path(inDirSens, "df_QALY_moreCECD3x.rds"))
pUSCE3x <- getPlots(chosen, df_prop_USCE3x, df_LYL_USCE3x)

# Sens: different Risk for Ecig
df_prop_USE5 <- readRDS(file.path(inDirSens, "df_prop_lessERisk.rds"))
df_LYL_USE5 <- readRDS(file.path(inDirSens, "df_QALY_lessERisk.rds"))
pUSE5 <- getPlots(chosen, df_prop_USE5, df_LYL_USE5)
pUSE5dis <- getPlots(chosen, df_prop_USE5, disQALY(df_LYL_USE5))

df_prop_USE20 <- readRDS(file.path(inDirSens, "df_prop_moreERisk.rds"))
df_LYL_USE20 <- readRDS(file.path(inDirSens, "df_QALY_moreERisk.rds"))
pUSE20 <- getPlots(chosen, df_prop_USE20, df_LYL_USE20)
pUSE20dis <- getPlots(chosen, df_prop_USE20, disQALY(df_LYL_USE20))

# Sens: different Scaling Combination
df_prop_USscaling <- readRDS(file.path(inDirSens, "df_prop_scaling.rds"))
df_LYL_USscaling <- readRDS(file.path(inDirSens, "df_QALY_scaling.rds"))
pUSscaling <- getPlots(chosen, df_prop_USscaling, df_LYL_USscaling)

# All EN SGUS
df_prop_USAllEN <- readRDS(file.path(inDirAllEN, paste0("df_prop_SG", "US", ".rds")))
df_LYL_USAllEN <- readRDS(file.path(inDirAllEN, paste0("df_QALY_SG", "US", ".rds")))
pUSAllEN <- getPlots(chosen, df_prop_USAllEN, df_LYL_USAllEN)
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

######################################################################################################
# Sensitivity plots
# More NE UK
png(file.path(outDirPlot, "sensitivity", paste0('MoreNEUK.png')),
    bg=backgroundCol, height=14,width=21,units='cm',res=400,pointsize=10)
grid.arrange(arrangeGrob(pUK$pCandD, pUK$pCDE, top=textGrob("            SGUK",
                                                            gp=gpar(fontface="bold", col=foregroundCol, cex=0.85))),
             arrangeGrob(pUKNE3x$pCandD, pUKNE3x$pCDE, top=textGrob("           SGUK with more e-cigarette initiation",
                                                                    gp=gpar(fontface="bold", col=foregroundCol, cex=0.85))),
             as_ggplot(l),
             nrow = 1,
             vp = viewport(width = 0.97, height = 0.97))
dev.off()

# More CE US
png(file.path(outDirPlot, "sensitivity", paste0('MoreCEUS.png')),
    bg=backgroundCol, height=14,width=21,units='cm',res=400,pointsize=10)
grid.arrange(arrangeGrob(pUS$pCandD, pUS$pCDE, top=textGrob("            SGUS",
                                                            gp=gpar(fontface="bold", col=foregroundCol, cex=0.8))),
             arrangeGrob(pUSCE3x$pCandD, pUSCE3x$pCDE, top=textGrob("           SGUS with more smokers using e-cigarettes",
                                                                    gp=gpar(fontface="bold", col=foregroundCol, cex=0.8))),
             as_ggplot(l),
             nrow = 1,
             vp = viewport(width = 0.97, height = 0.97))
dev.off()

# Risk Ecig
QALYleg <- get_legend(pUS$pCandDleg + theme(legend.position = c(unit(0.1, "npc"), unit(0.1, "npc"))))
png(file.path(outDirPlot, "sensitivity", paste0('ecigRisk.png')),
    bg=backgroundCol, height=7, width=21, units='cm', res=400, pointsize=10)
g <- grid.arrange(arrangeGrob(pUSE5$pAnnualLYL, top=textGrob("             E-cigarette is 5% as harmful as cigarettes",
                                                             gp=gpar(fontface="bold", col=foregroundCol, cex=0.7))),
                  arrangeGrob(pUSE20$pAnnualLYL, top=textGrob("            E-cigarette is 20% as harmful as cigarettes",
                                                              gp=gpar(fontface="bold", col=foregroundCol, cex=0.7))),
                  as_ggplot(QALYleg),
                  nrow = 1,
                  vp = viewport(width = 0.97, height = 0.97))
dev.off()

# Risk Ecig
QALYleg <- get_legend(pUS$pCandDleg + theme(legend.position = c(unit(0.1, "npc"), unit(0.1, "npc"))))
png(file.path(outDirPlot, "sensitivity", paste0('ecigRiskDis.png')),
    bg=backgroundCol, height=7, width=21, units='cm', res=400, pointsize=10)
g <- grid.arrange(arrangeGrob(pUSE5dis$pAnnualLYLdis, top=textGrob("             E-cigarette is 5% as harmful as cigarettes",
                                                             gp=gpar(fontface="bold", col=foregroundCol, cex=0.7))),
                  arrangeGrob(pUSE20dis$pAnnualLYLdis, top=textGrob("            E-cigarette is 20% as harmful as cigarettes",
                                                              gp=gpar(fontface="bold", col=foregroundCol, cex=0.7))),
                  as_ggplot(QALYleg),
                  nrow = 1,
                  vp = viewport(width = 0.97, height = 0.97))
dev.off()

# Risk Ecig
QALYleg <- get_legend(pUS$pCandDleg + theme(legend.position = c(unit(0.1, "npc"), unit(0.1, "npc"))))
png(file.path(outDirPlot, "sensitivity", paste0('ecigRisk.png')),
    bg=backgroundCol, height=7, width=21, units='cm', res=400, pointsize=10)
g <- grid.arrange(arrangeGrob(pUSE5$pAnnualLYL, top=textGrob("             E-cigarette is 5% as harmful as cigarettes",
                                                             gp=gpar(fontface="bold", col=foregroundCol, cex=0.7))),
                  arrangeGrob(pUSE20$pAnnualLYL, top=textGrob("            E-cigarette is 20% as harmful as cigarettes",
                                                              gp=gpar(fontface="bold", col=foregroundCol, cex=0.7))),
                  as_ggplot(QALYleg),
                  nrow = 1,
                  vp = viewport(width = 0.97, height = 0.97))
dev.off()

# Different scaling
png(file.path(outDirPlot, "sensitivity", paste0('USscaling.png')),
    bg=backgroundCol, height=14,width=21,units='cm',res=400,pointsize=10)
grid.arrange(arrangeGrob(pUS$pCandD, pUS$pCDE, top=textGrob("            SGUS scaling (a)",
                                                            gp=gpar(fontface="bold", col=foregroundCol, cex=0.8))),
             arrangeGrob(pUSscaling$pCandD, pUSscaling$pCDE, top=textGrob("           SGUS scaling (b)",
                                                                          gp=gpar(fontface="bold", col=foregroundCol, cex=0.8))),
             as_ggplot(l),
             nrow = 1,
             vp = viewport(width = 0.97, height = 0.97))
dev.off()

# All EN
# More CE US
png(file.path(outDirPlot, "sensitivity", paste0('SGUS_AllEN.png')),
    bg=backgroundCol, height=14,width=21,units='cm',res=400,pointsize=10)
grid.arrange(arrangeGrob(pUS$pCandD, pUS$pCDE, top=textGrob("            SGUS transition model (a)",
                                                            gp=gpar(fontface="bold", col=foregroundCol, cex=0.8))),
             arrangeGrob(pUSAllEN$pCandD, pUSAllEN$pCDE, top=textGrob("           SGUS transition model (b)",
                                                                      gp=gpar(fontface="bold", col=foregroundCol, cex=0.8))),
             as_ggplot(l),
             nrow = 1,
             vp = viewport(width = 0.97, height = 0.97))
dev.off()