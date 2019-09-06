rm(list=ls())
###########################################################################################################
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
###########################################################################################################
smallDatDir <- file.path("code", "scenarios_micro", "micro_noecig", "outData")
bigDatDir <- file.path("code", "scenarios_micro", "micro_v0.2", "outData")
outDir <- file.path("code", "scenarios_micro", "micro_v0.2", "combine", "combined_data")
###########################################################################################################
d.e <- 0.03
n.t <- 50
v.dwe <- 1 / (1 + d.e) ^ (0:n.t)
###########################################################################################################
calQALY <- function(scen, type="small", big="US"){
  if(type=="small"){
    fname <- paste0(scen, "_colSumME.rds")
    datDir = smallDatDir
  } else {
    fname <- paste0(scen, "_SG", big, "_colSumME.rds")
    datDir = bigDatDir
  }
  
  discountedQALY <- (readRDS(file.path(datDir, fname)) - SQQALY) * v.dwe

  sum(discountedQALY)
}
###########################################################################################################
SQQALY <- readRDS(file.path(smallDatDir, "SQ_colSumME.rds"))
NOECIGS <- c("SQ", "MLA", "MLA25", "SFG", "TAX2", "TAX5", "MLATAX")
ECIGS <- c("ELF", "ELF25", "EP", "MLAEP", "MLATAXEP", "TAXEP", "SFGELF")

genQALY <- function(big="US"){
  lQALYs <- list()
  for(scen in NOECIGS){
    lQALYs[[scen]] <- calQALY(scen)
    print(scen)
  }
  for(scen in ECIGS){
    lQALYs[[scen]] <- calQALY(scen, "big", big)
    print(scen)
  }
  lQALYs
}

lQALYs_US <- genQALY("US")
lQALYs_UK <- genQALY("UK")
lQALYs_JP <- genQALY("JP")

lQALYs_US
lQALYs_UK
lQALYs_JP

###########################################################################################################################
df <- data.frame(scen = names(lQALYs_US), 
                 QALYs = unname(unlist(lQALYs_US)),
                 variant = "SGUS") %>%
  bind_rows(
    data.frame(scen = names(lQALYs_UK), 
               QALYs = unname(unlist(lQALYs_UK)),
               variant = "SGUK")
  ) %>%
  bind_rows(
    data.frame(scen = names(lQALYs_JP), 
               QALYs = unname(unlist(lQALYs_JP)),
               variant = "SGJP")
  )

df <- df %>%
  mutate(scen = factor(scen, levels=c("SQ", "MLA", "MLA25", "SFG", "TAX2", "TAX5", "MLATAX",
                                         "ELF", "ELF25", "EP", "TAXEP", "MLAEP", "MLATAXEP", "SFGELF"))) %>% 
  mutate(scen = revalue(scen, c("ELF25" = "E25")))

View(df)

# Plotting
foregroundCol = "black"
backgroundCol = "white"
panelCol = "white"
SHAPES <- c("SGJP"=4, "SGUK"=21, "SGUS"=24)
COLORS <- c("SGJP"="grey19", "SGUK"="deepskyblue", "SGUS"="darkorange")
VARIANTS <- c("SGJP", "SGUK", "SGUS")
axisTextSize <- 6
legendTextSize <- 8

png(file.path(outDir, "compareQALY.png"), height=10, width=15, units="cm", res=300, pointsize = 8)
ggplot(df%>%filter(scen!="MLA25"),
       aes(x=scen, y=QALYs/1000, group=variant, shape=variant, fill=variant, color=variant)) +
  theme_bw() +
  theme(panel.border=element_rect(color = foregroundCol, fill=NA),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = panelCol),
        plot.background = element_rect(fill = backgroundCol)) +
  theme(axis.text.x  = element_text(color=foregroundCol, size=axisTextSize),
        axis.text.y = element_text(color=foregroundCol, size=axisTextSize),
        axis.title.x = element_text(color=foregroundCol, size=axisTextSize+2, face="bold"),
        axis.title.y = element_text(color=foregroundCol, size = axisTextSize+2, face="bold"),
        axis.ticks = element_line(color=foregroundCol),
        axis.line = element_line(colour=foregroundCol)) +
  geom_point() +
  scale_shape_manual(values = SHAPES,
                     name = " ",
                     limits=VARIANTS) +
  scale_fill_manual(values = COLORS,
                     name = " ",
                     limits=VARIANTS) +
  scale_color_manual(values = COLORS,
                    name = " ",
                    limits=VARIANTS) +
  labs(x="Policies", y="QALYs gained in 2017-2067 compared with Status Quo (000s)") +
  theme(legend.position = c(x=unit(0.075, "npc"), y=unit(0.95, "npc")),
        legend.background = element_rect(fill=NA),
        legend.text = element_text(size=legendTextSize, colour=foregroundCol))
dev.off()
