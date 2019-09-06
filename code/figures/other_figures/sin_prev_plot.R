library(tidyverse)
library(readxl)

############################################################################################
## In directories
smokeDatPath <- 'data/singapore/smoking_data.xlsx'
## Out directories
outPlotDir <- 'code/figures/other_figures/plots'
############################################################################################
# Read the actual prevalence in Singapore
actual_dat <- read_excel(smokeDatPath, sheet=2)

actual_dat <- actual_dat %>%
    filter(!is.na(smoking_status)) %>%
    group_by(year, smoking_status) %>%
    summarise(num = sum(sample_wt)) %>%
    mutate(prevalence = num/sum(num)) %>%
    select(-num) %>%
    spread(smoking_status, prevalence) %>%
    rename('DailySmoker' = 'Daily Smoker',
           'OccasionalSmoker' = 'Ocassional Smoker',
           'Ex-smoker' = "Ex-smoker",
           'Never smoker' = "Non-smoker") %>%
    mutate("Current smoker" = sum(DailySmoker, OccasionalSmoker, na.rm=T)) %>%
    select(-c(DailySmoker, OccasionalSmoker)) %>%
    gather(key=smoking_status, value=prevalence, -year) %>%
    mutate(prevalence = prevalence*100)

# Plot the prevalence over the years
png(file.path(outPlotDir,"sin_smoke_prev.png"), height=8.5, width=8, units="cm", res=300, pointsize=8)
ggplot(data=actual_dat, 
       aes(x=year, y=prevalence, 
           group=smoking_status, color=smoking_status, fill=smoking_status)) +
    geom_point() +
    geom_smooth(alpha=0.2) +
    labs(x="Year", y="Prevalence (%)", fill="smoking status", color="smoking status") +
    scale_x_continuous(breaks=seq(1992,2016,by=6)) +
    scale_color_manual(values=c("deepskyblue", "darkorange", "grey"),
                       limits=c("Never smoker", "Current smoker", "Ex-smoker")) +
    scale_fill_manual(values=c("deepskyblue", "darkorange", "grey"),
                       limits=c("Never smoker", "Current smoker", "Ex-smoker")) +
    ylim(0, 100) +
    theme_classic() +
    theme(panel.border=element_rect(color="black", fill=NA)) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size=7)) +
    theme(axis.text.x  = element_text(colour = "black", size = 8),
          axis.text.y = element_text(colour = "black", size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8)) 
dev.off()