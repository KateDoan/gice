library(tidyverse)
library(gridExtra)
############################################################################################
## In and Out directories
inDir <- "code/transitions/sin_jags/singapore_mcmc_free9/output"
outDir <- "code/figures/other_figures/plots"
############################################################################################
## Parameters
COLZ = list(NC="darkorange", CQ="deepskyblue", QC="grey10")
############################################################################################
## Read mod_csim file
mod_csim <- readRDS(file.path(inDir,"mod_csim.rds"))
mean_trans <- colMeans(mod_csim)*100
lower_trans <- apply(mod_csim, 2, quantile, probs=0.025)*100
upper_trans <- apply(mod_csim, 2, quantile, probs=0.975)*100
## Indices for transitions
CQ_idx <- 1:68
NC_idx <- 69:136
QC_idx <- 137:204
## Define dataframe for plotting
dat <- data.frame(age = 12:79,
                  NC = mean_trans[NC_idx],
                  CQ = mean_trans[CQ_idx],
                  QC = mean_trans[QC_idx],
                  NC_low = lower_trans[NC_idx],
                  NC_high = upper_trans[NC_idx],
                  CQ_low = lower_trans[CQ_idx],
                  CQ_high = upper_trans[CQ_idx],
                  QC_low = lower_trans[QC_idx],
                  QC_high = upper_trans[QC_idx])
## Plot the transition graphs
pNC <- ggplot(data=dat, aes(x=age, y=NC))+
    geom_ribbon(aes(ymin=NC_low, ymax=NC_high), col=COLZ$NC, fill=COLZ$NC, alpha=0.3) +
    geom_line(col=COLZ$NC) +
    geom_point(col=COLZ$NC) +
    theme_classic() +
    theme(panel.border = element_rect(colour="black", fill=NA)) +
    theme(axis.text.x  = element_text(color = "black", size = 8),
          axis.text.y = element_text(color = "black", size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8)) +
    labs(x="Age (years)", y="Annual probablity of smoking initiation (%)") +
    ylim(c(0,10)) +
    theme(legend.position = "None")

pCQ <- ggplot(data=dat, aes(x=age, y=CQ))+
    geom_ribbon(aes(ymin=CQ_low, ymax=CQ_high), col=COLZ$CQ, fill=COLZ$CQ, alpha=0.3) +
    geom_line(col=COLZ$CQ) +
    geom_point(col=COLZ$CQ) +
    theme_classic() +
    theme(panel.border = element_rect(colour="black", fill=NA)) +
    theme(axis.text.x  = element_text(color = "black", size = 8),
          axis.text.y = element_text(color = "black", size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8)) +
    labs(x="Age (years)", y="Annual probablity of smoking quitting (%)") +
    ylim(c(0,10)) +
    theme(legend.position = "None")

pQC <- ggplot(data=dat, aes(x=age, y=QC))+
    geom_ribbon(aes(ymin=QC_low, ymax=QC_high), col=COLZ$QC, fill=COLZ$QC, alpha=0.3) +
    geom_line(col=COLZ$QC) +
    geom_point(col=COLZ$QC) +
    theme_classic() +
    theme(panel.border = element_rect(colour="black", fill=NA)) +
    theme(axis.text.x  = element_text(color = "black", size = 8),
          axis.text.y = element_text(color = "black", size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8)) +
    labs(x="Age (years)", y="Annual probability of smoking relapse (%)") +
    ylim(c(0,10)) +
    theme(legend.position = "None")
###########################################################################################
png(file.path(outDir, "sin_trans_plot.png"), height=8, width=24, units='cm', res=400, pointsize=10)
    grid.arrange(pNC, pCQ, pQC, nrow=1)
dev.off()
###########################################################################################
