library(tidyverse)
library(grid)
library(gridExtra)
library(ggpubr)
#########################################################################################
## In and Out directories
inDir <- "tables/transition_tables/tables_output"
outDir <- "figures/createCompareTranSupplot/output_plots"
#########################################################################################
## Helper functions
preprocessDat <- function(dat){
    age_start <- dat$age_start
    age_start2_pre <- c(rep(age_start, each=2))


    age_end <- dat$age_end
    age_end2 <- rep(age_end, each=2)
    age_start2 <- c(age_start2_pre[-1], tail(age_end, n=1))

    tran <- dat$value
    tran2 <- rep(tran, each=2)

    sd <- dat$sd
    tran_up <- pmin(tran+2*sd, 100)
    tran_low <- pmax(tran-2*sd, 0)
    tran_up2 <- rep(tran_up, each=2)
    tran_low2 <- rep(tran_low, each=2)

    dat2 <- data.frame(age_start2=age_start2, age_end2=age_end2,
                   tran2=tran2,
                   tran_up2=tran_up2, tran_low2=tran_low2)
    dat2
}
###########################################################################################
## Read the transition tables
USDat <- readRDS(file.path(inDir, "USTmSupplot.rds"))
UKDat <- readRDS(file.path(inDir, "UKTmSupplot.rds")) %>% rename(value=mean)
JPDat <- readRDS(file.path(inDir, "JPTmSupplot.rds")) %>% rename(value=mean)
OWDat <- readRDS(file.path(inDir, "OWTmSupplot.rds")) %>% rename(value=mean)

plotTransCompare <- function(tran, ylim, ylab){
    ## Preprocess
    USDat2 <- preprocessDat(USDat %>% filter(transition==tran))
    UKDat2 <- preprocessDat(UKDat %>% filter(transition==tran))
    JPDat2 <- preprocessDat(JPDat %>% filter(transition==tran))
    OWDat2 <- preprocessDat(OWDat %>% filter(transition==tran))
    ## Plot the comparison plots
    p <- ggplot() +
        geom_ribbon(data = JPDat2, aes(x=age_start2,ymax=tran_up2, ymin=tran_low2), alpha=0.6, fill="grey") +
        geom_segment(data = JPDat2, aes(x=age_start2, y=tran2, xend=age_end2, yend=tran2), col="grey") +
        geom_ribbon(data = UKDat2, aes(x=age_start2,ymax=tran_up2, ymin=tran_low2), alpha=0.4, fill="deepskyblue") +
        geom_segment(data = UKDat2, aes(x=age_start2, y=tran2, xend=age_end2, yend=tran2), col="deepskyblue") +
        geom_ribbon(data = USDat2, aes(x=age_start2, ymax=tran_up2, ymin=tran_low2), alpha=0.5, fill="darkorange") +
        geom_segment(data = USDat2, aes(x=age_start2, y=tran2, xend=age_end2, yend=tran2), col="darkorange") + 
        geom_ribbon(data = OWDat2, aes(x=age_start2, ymax=tran_up2, ymin=tran_low2), alpha=0.5, fill="red") +
        geom_segment(data = OWDat2, aes(x=age_start2, y=tran2, xend=age_end2, yend=tran2), col="red") + 
        theme_classic() +
        theme(panel.border=element_rect(color="black", fill=NA)) +
        labs(x=" ", y=ylab) +
        ylim(ylim)
    p
}
pNN <- plotTransCompare("NN", c(0, 100), 
                 ylab=expression("N"%->%"N (%)"))
pNC <- plotTransCompare("NC", c(0, 100), 
                  ylab=expression("N"%->%"C (%)"))
pND <- plotTransCompare("ND", c(0, 100), 
                 ylab=expression("N"%->%"D (%)"))
pNE <- plotTransCompare("NE", c(0, 100), 
                 ylab=expression("N"%->%"E (%)"))

pCC <- plotTransCompare("CC", c(0, 100), 
                 ylab=expression("C"%->%"C (%)"))
pCQ <- plotTransCompare("CQ", c(0, 100), 
                 ylab=expression("C"%->%"Q (%)"))
pCD <- plotTransCompare("CD", c(0, 100), 
                 ylab=expression("C"%->%"D (%)"))
pCE <- plotTransCompare("CE", c(0, 100), 
                 ylab=expression("C"%->%"E (%)"))

pQC <- plotTransCompare("QC", c(0, 100), 
                 ylab=expression("Q"%->%"C (%)"))
pQQ <- plotTransCompare("QQ", c(0, 100), 
                 ylab=expression("Q"%->%"Q (%)"))
pQD <- plotTransCompare("QD", c(0, 100), 
                 ylab=expression("Q"%->%"D (%)"))
pQE <- plotTransCompare("QE", c(0, 100), 
                 ylab=expression("Q"%->%"E (%)"))

pDC <- plotTransCompare("DC", c(0, 100), 
                 ylab=expression("D"%->%"C (%)"))
pDQ <- plotTransCompare("DQ", c(0, 100), 
                 ylab=expression("D"%->%"Q (%)"))
pDD <- plotTransCompare("DD", c(0, 100), 
                 ylab=expression("D"%->%"D (%)"))
pDE <- plotTransCompare("DE", c(0, 100), 
                 ylab=expression("D"%->%"E (%)"))

pEC <- plotTransCompare("EC", c(0, 100), 
                 ylab=expression("E"%->%"C (%)")) 
pEQ <- plotTransCompare("EQ", c(0, 100), 
                 ylab=expression("E"%->%"Q (%)"))
pED <- plotTransCompare("ED", c(0, 100), 
                 ylab=expression("E"%->%"D (%)"))
pEE <- plotTransCompare("EE", c(0, 100), 
                 ylab=expression("E"%->%"E (%)"))
pEC <- pEC + labs(x="Age (years)")
pEQ <- pEQ + labs(x="Age (years)")
pED <- pED + labs(x="Age (years)")
pEE <- pEE + labs(x="Age (years)")
###########################################################################################
## Create a pseudo plot to extract the legend
df_pseudo <- data.frame(
    x = rep(c(1:2),4),
    y = rep(c(2:3),4),
    sd = rep(c(0.2,0.4),4),
    group = rep(c("US", "UK", "JP", "OW"), each=2)
) %>% mutate(yup = y+sd, ydown = y-sd)
pseudo_plot <-
    ggplot(df_pseudo, aes(x=x, y=y, col=group))+
    geom_line() + 
    geom_ribbon(aes(ymin=ydown, ymax=yup, fill=group), alpha=0.5) +
    scale_color_manual(values=c("grey", "deepskyblue", "darkorange", "red"),
                       labels=c("Japan (JP)   ", "United Kingdom (UK)   ", "United States (US)   ", "Oliver Wyman")) +
    scale_fill_manual(values=c("grey", "deepskyblue", "darkorange", "red"),
                      labels=c("Japan (JP)   ", "United Kingdom (UK)   ", "United States (US)   ", "Oliver Wyman")) +
    theme(legend.position = "top") +
    labs(fill = "          ", col = "          ")
leg <- get_legend(pseudo_plot)
     
###########################################################################################
png(file.path(outDir, paste0("compareTranPlotOW.png")), height=18, width=15, units='cm', res=400, pointsize=10)
grid.arrange(arrangeGrob(leg),
             arrangeGrob(
             pNN,pNC,pND,pNE,
             pCC,pCQ,pCD,pCE,
             pQC,pQQ,pQD,pQE,
             pDC,pDQ,pDD,pDE,
             pEC,pEQ,pED,pEE,
             nrow = 5), 
             nrow = 2, heights=c(0.05, 0.95))
dev.off()
###########################################################################################