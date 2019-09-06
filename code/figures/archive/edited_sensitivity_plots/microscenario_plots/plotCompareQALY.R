detach(package:plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
#########################################################################################################
# In and Out Dir
inDir <- file.path("code", "tables", "combined_microtables", "output", "for_graph")
# outDirPlot <- file.path("code/figures/microscenario_plots/plots/outPlot")
outDirPlot <- file.path("code/figures/archive/edited_sensitivity_plots/microscenario_plots/outPlot")
#########################################################################################################
startYear <- 2017
#########################################################################################################
genSumQALY <- function(df_QALY, big){
    df_QALY %>%
        mutate(f_discount = 1 / (1 + d.e) ^ (year-startYear)) %>%
        mutate(dis_QALY = f_discount * QALY) %>% 
        group_by(scenario) %>%
        summarise(QALYs=sum(dis_QALY)) %>%
        mutate(variant=paste0("SG", big)) %>%
        rename("scen" = "scenario")
}
#########################################################################################################
df_LYL_US <- readRDS(file.path(inDir, paste0("df_QALY_SG", "US", ".rds")))
df_LYL_UK <- readRDS(file.path(inDir, paste0("df_QALY_SG", "UK", ".rds")))
df_LYL_JP <- readRDS(file.path(inDir, paste0("df_QALY_SG", "JP", ".rds")))

for(dis in c(T, F)){
# dis <- T
    if(dis){
        d.e <- 0.03
        post <- "Dis"
    } else {
        d.e <- 0.00
        post <- ""
    }
    
    US_QALY <- genSumQALY(df_LYL_US, "US")
    UK_QALY <- genSumQALY(df_LYL_UK, "UK")
    JP_QALY <- genSumQALY(df_LYL_JP, "JP")
    
    dfSumQALY <- bind_rows(US_QALY, UK_QALY, JP_QALY) %>%
        mutate(scen = factor(scen, levels=rev(c("SQ", "MLA21", "MLA25", "SFG", "TAX2", "TAX5",
                                                "ELF", "E25", "EP", "MLATax5", "Tax5EP", "MLAEP", "MLATax5EP", "SFGELF"))))
    # %>%
    #   mutate(scen = plyr::revalue(scen, c("MLATax5"="MLATAX", "Tax5EP"="TAXEP", 
    #                                       "MLATax5EP"="MLATAXEP", "MLA21"="MLA")))
    
    # Plotting
    foregroundCol = "black"
    backgroundCol = "white"
    panelCol = "white"
    SHAPES <- c("SGJP"=4, "SGUK"=21, "SGUS"=24)
    COLORS <- c("SGJP"="grey19", "SGUK"="deepskyblue", "SGUS"="darkorange")
    VARIANTS <- c("SGJP", "SGUK", "SGUS")
    axisTextSize <- 8
    legendTextSize <- 7
    
    # png(file.path(outDirPlot, paste0("compareQALY.png")), height=10, width=15, units="cm", res=300, pointsize = 8)
    jpeg(file.path(outDirPlot, "jpg", paste0("compareQALY", post, ".jpeg")), height=10, width=15, units="cm", res=300, pointsize = 8)
    print(
    ggplot(dfSumQALY %>% filter(scen!="MLA25"),
           aes(y=scen, x=QALYs/1000, group=variant, shape=variant, fill=variant, color=variant)) +
        theme_bw() +
        theme(panel.border=element_rect(color = foregroundCol, fill=NA),
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.background = element_rect(fill = panelCol),
              plot.background = element_rect(fill = backgroundCol)) +
        theme(axis.text.x  = element_text(color=foregroundCol, size=axisTextSize+1),
              axis.text.y = element_text(color=foregroundCol, size=axisTextSize),
              axis.title.x = element_text(color=foregroundCol, size=axisTextSize+1, face="bold"),
              axis.title.y = element_text(color=foregroundCol, size = axisTextSize, face="bold"),
              axis.ticks = element_line(color=foregroundCol),
              axis.line = element_line(colour=foregroundCol)) +
        geom_point() +
        # geom_vline(xintercept = seq(0, 500, 100)) + 
        scale_shape_manual(values = SHAPES,
                           name = " ",
                           limits=VARIANTS) +
        scale_fill_manual(values = COLORS,
                          name = " ",
                          limits=VARIANTS) +
        scale_color_manual(values = COLORS,
                           name = " ",
                           limits=VARIANTS) +
        scale_y_discrete(labels=c(SQ="Status Quo (SQ)", MLA21="Minimum Legal Age (MLA)",
                                  SFG="Smoke Free Generation (SFG)",  TAX5="TAX5", TAX2="TAX2", 
                                  ELF="E-cigarette Laissez-Faire (ELF)", E25="E-cigarette 25 (E25)",
                                  EP="E-cigarette Prescription (EP)",
                                  MLATax5="MLA + TAX5", MLAEP="MLA + EP", Tax5EP="TAX5 + EP", 
                                  MLATax5EP="MLA + TAX5 + EP", SFGELF="SFG + ELF")) +
        scale_x_continuous(position = "top") + 
        labs(y="", x="QALYs gained in 2017-2067 compared with Status Quo (000s)") +
        theme(legend.position = c(x=unit(0.93, "npc"), y=unit(0.92, "npc")),
              legend.background = element_rect(fill=NA),
              legend.text = element_text(size=legendTextSize, colour=foregroundCol),
              legend.title = element_blank()) 
    )
    dev.off()
}





