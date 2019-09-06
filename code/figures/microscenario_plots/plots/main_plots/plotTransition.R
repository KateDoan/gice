library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

outDirPlot <- file.path("code/figures/microscenario_plots/plots/main_plots/outPlot")

foregroundCol <- "black"
backgroundCol <- "white"
axisTextSize <- 8

df_RR <- data.frame(x=seq(0, 50, 1)) %>% mutate(y=0.93^x)
df_QALY <- data.frame(x=seq(0, 50, 1)) %>% mutate(y=1-0.93^x)

pRR <- ggplot(data=df_RR, aes(x=x, y=y)) +
        geom_line() +
        geom_hline(yintercept = c(0), linetype="dashed") +
        theme_classic() +
        scale_y_continuous(breaks=c(0,1), labels=c(expression("RR"["low"]),expression("RR"["high"]))) +
        theme(axis.text.x  = element_text(color=foregroundCol, size=axisTextSize),
              axis.text.y = element_text(color=foregroundCol, size=axisTextSize),
              axis.title.x  = element_text(color=foregroundCol, size=axisTextSize),
              axis.title.y = element_text(color=foregroundCol, size=axisTextSize)) +
        labs(x="Time since the transition (years)", y="Relative Risk (RR)")

pQALY <- ggplot(data=df_QALY, aes(x=x, y=y)) +
    geom_line() +
    geom_hline(yintercept = c(1), linetype="dashed") +
    theme_classic() +
    scale_y_continuous(breaks=c(0,1), labels=c(expression("QALY"["low"]),expression("QALY"["high"]))) +
    theme(axis.text.x  = element_text(color=foregroundCol, size=axisTextSize),
          axis.text.y = element_text(color=foregroundCol, size=axisTextSize),
          axis.title.x  = element_text(color=foregroundCol, size=axisTextSize),
          axis.title.y = element_text(color=foregroundCol, size=axisTextSize)) +
    labs(x="Time since the transition (years)", y="Quality-adjusted life year (QALY)")

png(file.path(outDirPlot, paste0('RR_transition.png')),
    bg=backgroundCol, height=7,width=15,units='cm',res=400,pointsize=10)
grid.arrange(pRR, pQALY, nrow=1)
dev.off()