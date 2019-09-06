rm(list=ls())
###################################################################################################
library(tidyverse)
###################################################################################################
i <- 1000
pre <- ifelse(i==1, "", paste0(i, "_"))
###################################################################################################
inDir <- file.path("code", "tables", "combined_microtables", "input", paste0("input", i))
inDir100 <- file.path("code", "tables", "combined_microtables", "input", paste0("input", 100))
outDir <- file.path("code", "tables", "combined_microtables", "output", "for_graph")
utilDir <- file.path("code", "tables", "combined_microtables", "util")
source(file.path(utilDir, "cal_ELFE25_diff.R"))
###################################################################################################
startYear <- 2017
endYear <- 2067
chosen_years <- seq(2017, 2067, by=10)

NONECIG_SCENARIOS <- c("SQ", "MLA", "SFG", "TAX2", "TAX5", "MLATAX")
ECIG_SCENARIOS <- c("ELF", "EP", "ELF25", "MLAEP", "TAXEP", "MLATAXEP", "SFGELF")
# ECIG_SCENARIOS <- c("ELF", "EP", "ELF25", "MLAEP", "SFGELF")

NONECIG_LABEL <- c("SQ", "MLA21", "SFG", "TAX2", "TAX5", "MLATax5")
ECIG_LABEL <- c("ELF", "EP", "E25", "MLAEP", "Tax5EP", "MLATax5EP", "SFGELF")
# ECIG_LABEL <- c("ELF", "EP", "E25", "MLAEP", "SFGELF")
###################################################################################################
# Combine prevalence tables
readProp <- function(type="big", scen, big=NA) {
  if(type == "small"){
    if(scen %in% c("MLA", "SFG")){
      df <- readRDS(file.path(inDir100, paste0("100_", scen, "_summaryTR.rds")))
    } else {
      df <- readRDS(file.path(inDir, paste0(pre, scen, "_summaryTR.rds")))
    }
    df <- df %>%
      select(year, group, mean) %>%
      spread("group", "mean")
    df <- df %>% mutate("D"=0, "E"=0) %>% 
      mutate(scenario=NONECIG_LABEL[match(scen, NONECIG_SCENARIOS)])%>%
      mutate(NandQ = N + Q, CandD = C+D, CDE = C+D+E)
  } else {
    if(scen == "ELF25"){
      df <- readRDS(file.path(inDir, paste0(pre, "ELF", "_SG", big, "_summaryTR.rds"))) %>%
        select(year, group, mean) 
      df$mean <- df$mean + cal_ELFE25_diff100(big)$prop_mean_diff
      df <- df %>% spread("group", "mean")
    } else {
      df <- readRDS(file.path(inDir, paste0(pre, scen, "_SG", big, "_summaryTR.rds"))) %>%
        select(year, group, mean) %>%
        spread("group", "mean")
    }
    
    df <- df %>% 
      mutate(scenario=ECIG_LABEL[match(scen, ECIG_SCENARIOS)]) %>%
      mutate(NandQ = N + Q, CandD = C+D, CDE = C+D+E)
  }
  
  df
}

combine_prop <- function(big){
  df_combine_prop <- readProp("small", "SQ")
  for(scen in NONECIG_SCENARIOS){
    temp <- readProp("small", scen)
    df_combine_prop <- bind_rows(df_combine_prop, temp)
  }
  for(scen in ECIG_SCENARIOS){
    temp <- readProp("big", scen, big)
    df_combine_prop <- bind_rows(df_combine_prop, temp)
  }
  df_combine_prop
}

for(big in c("US", "UK", "JP")){
  saveRDS(combine_prop(big), file.path(outDir, paste0("df_prop_SG", big, ".rds")))
}
########################################################################################################
# Combine QALY tables
readQALY <- function(type="big", scen, big=NA){
  if(scen %in% c("MLA", "SFG")){
    SQ_QALY <- readRDS(file.path(inDir100, paste0("100_", "SQ", "_dfQALY.rds")))%>% gather(i, QALY, -year)
  } else {
    SQ_QALY <- readRDS(file.path(inDir, paste0(pre, "SQ", "_dfQALY.rds")))%>% gather(i, QALY, -year)
  }
  if(type == "small"){
    if(scen %in% c("MLA", "SFG")){
      temp <- readRDS(file.path(inDir100, paste0("100_", scen, "_dfQALY.rds")))
    } else if(scen == "TAX5"){
      temp <- bind_cols(readRDS(file.path(inDir, "TAX5", paste0("160_", scen, "_dfQALY.rds"))),
                        readRDS(file.path(inDir, "TAX5", paste0("1000_", scen, "_dfQALY.rds"))) %>% select(-year))
    } else {
      temp <- readRDS(file.path(inDir, paste0(pre, scen, "_dfQALY.rds")))
    }
  } else {
    if(scen == "ELF25"){
      temp <- readRDS(file.path(inDir, paste0(pre, "ELF", "_SG", big, "_dfQALY.rds")))
    } else {
      temp <- readRDS(file.path(inDir, paste0(pre, scen, "_SG", big, "_dfQALY.rds")))
    }
  }
  temp <- temp %>% gather(i, QALY, -year)
  temp$QALY <- temp$QALY - SQ_QALY$QALY
  temp <- temp %>%
    group_by(year) %>%
    summarise(mean=mean(QALY))
  
  if(scen == "ELF25"){
    temp$mean <- temp$mean + cal_ELFE25_diff100(big)$QALY_mean_diff
  }
  
  temp <- temp %>% rename(QALY=mean) 
  
  if(type == "small"){
    temp <- temp %>%
      mutate(scenario=NONECIG_LABEL[match(scen, NONECIG_SCENARIOS)])
  } else {
    temp <- temp %>%
      mutate(scenario=ECIG_LABEL[match(scen, ECIG_SCENARIOS)])
  }
  temp
}

combine_QALY <- function(big){
  df_combine_QALY <- readQALY("small", "SQ")
  for(scen in NONECIG_SCENARIOS){
    temp <- readQALY("small", scen)
    df_combine_QALY <- bind_rows(df_combine_QALY, temp)
  }
  for(scen in ECIG_SCENARIOS){
    temp <- readQALY("big", scen, big)
    df_combine_QALY <- bind_rows(df_combine_QALY, temp)
  }
  df_combine_QALY
}

for(big in c("US", "UK", "JP")){
  saveRDS(combine_QALY(big), file.path(outDir, paste0("df_QALY_SG", big, ".rds")))
}
