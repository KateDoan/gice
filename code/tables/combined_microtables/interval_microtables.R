rm(list=ls())
###################################################################################################
library(tidyverse)
###################################################################################################
i <- 1000
i_sd <- 100
f <- 2
pre <- ifelse(i==1, "", paste0(i, "_"))
###################################################################################################
inDir <- file.path("code", "tables", "combined_microtables", "input", paste0("input", i))
inDir100 <- file.path("code", "tables", "combined_microtables", "input", paste0("input", 100))
outDir <- file.path("code", "tables", "combined_microtables", "output", "interval")
outDirCSV <- file.path("code", "tables", "combined_microtables", "output", "interval", "outputCSV")
utilDir <- file.path("code", "tables", "combined_microtables", "util")
source(file.path(utilDir, "cal_ELFE25_diff.R"))
###################################################################################################
startYear <- 2017
endYear <- 2067
chosen_years <- seq(2017, 2067, by=10)

NONECIG_SCENARIOS <- c("SQ", "MLA", "SFG", "TAX2", "TAX5", "MLATAX")
ECIG_SCENARIOS <- c("ELF", "EP",  "ELF25", "MLAEP", "TAXEP", "MLATAXEP", "SFGELF")
# ECIG_SCENARIOS <- c("ELF", "EP",  "ELF25", "MLAEP", "SFGELF")
###################################################################################################
# Util functions
fm <- function(num){format(round(num, 1), nsmall=1)}
fm0 <- function(num){format(round(num, 0), nsmall=0)}

createCdf <- function(df, scen){
  df <- df %>% filter(group=="C") 
  df[[scen]] <- with(df, paste0(fm(mean), " (", fm(mean-f*sd/sqrt(i_sd)), ", ", fm(mean+f*sd/sqrt(i_sd)), ")"))
  df[,c("year", scen)]
}

createCDdf <- function(df, scen, opt="CD"){
  if(opt=="CD"){
    df <- df %>% filter(group %in% c("C", "D")) 
  } else if(opt=="CDE"){
    df <- df %>% filter(group %in% c("C", "D", "E")) 
  }
  
  df <- df %>%
    group_by(year) %>%
    summarise(mean = sum(mean), sd = sqrt(sum(sd^2)), CIlow = sum(CIlow), CIhigh = sum(CIhigh))
  df[[scen]] <- with(df, paste0(fm(mean), " (", fm(mean-f*sd/sqrt(i_sd)), ", ", fm(mean+f*sd/sqrt(i_sd)), ")"))
  df[,c("year", scen)]
}

createDfEcig <- function(opt="CD", big="US"){
  df_combine <- data.frame(year = (startYear+1):endYear)
  for(scen in ECIG_SCENARIOS){
    if(scen == "ELF25"){
      df <- readRDS(file.path(inDir, paste0(pre, "ELF", "_SG", big, "_summaryTR.rds")))
      df$mean <- df$mean + cal_ELFE25_diff100(big)$prop_mean_diff
      df$sd <- df$sd * cal_ELFE25_diff100(big)$prop_sd_ratio
    } else {
      df <- readRDS(file.path(inDir, paste0(pre, scen, "_SG", big, "_summaryTR.rds")))
    }
    
    if(opt=="C"){
      df <- createCdf(df, scen)
    } else {
      df <- createCDdf(df, scen, opt=opt)
    }
    
    df_combine <- df_combine %>%
      left_join(df, by = c("year"))
  }
  df_combine <- df_combine %>% filter(year %in% chosen_years)
  saveRDS(df_combine, file.path(outDir, paste0(pre, opt, "_Ecig_SG", big, ".rds")))
  write.csv(df_combine, file.path(outDirCSV, paste0(pre, opt, "_Ecig_SG", big, ".csv")), row.names = FALSE)
  df_combine
}

###################################################################################################
# Combine prevalence tables
# For non-ecig scenarios
df_C_NoEcig <- data.frame(year = (startYear+1):endYear)
for(scen in NONECIG_SCENARIOS){
  if(scen %in% c("MLA", "SFG")){
    df <- readRDS(file.path(inDir100, paste0("100_", scen, "_summaryTR.rds")))
  } else {
    df <- readRDS(file.path(inDir, paste0(pre, scen, "_summaryTR.rds")))
  }
  df <- createCdf(df, scen)
  df_C_NoEcig <- df_C_NoEcig %>%
    left_join(df, by = c("year"))
}
df_C_NoEcig <- df_C_NoEcig %>% filter(year %in% chosen_years)
saveRDS(df_C_NoEcig, file.path(outDir, paste0(pre, "C_NoEcig_SG.rds")))
write.csv(df_C_NoEcig, file.path(outDirCSV, paste0(pre, "C_NoEcig_SG.csv")), row.names = FALSE)

USdf_C_Ecig <- createDfEcig(opt="C", big="US")
USdf_CD_Ecig <- createDfEcig(opt="CD", big="US")
USdf_CDE_Ecig <- createDfEcig(opt="CDE", big="US")

UKdf_C_Ecig <- createDfEcig(opt="C", big="UK")
UKdf_CD_Ecig <- createDfEcig(opt="CD", big="UK")
UKdf_CDE_Ecig <- createDfEcig(opt="CDE", big="UK")

JPdf_C_Ecig <- createDfEcig(opt="C", big="JP")
JPdf_CD_Ecig <- createDfEcig(opt="CD", big="JP")
JPdf_CDE_Ecig <- createDfEcig(opt="CDE", big="JP")

###################################################################################################
# Combine QALY tables
df_QALY_NoEcig <- data.frame(year = (startYear):endYear)
for(scen in NONECIG_SCENARIOS){
  if(scen %in% c("MLA", "SFG")){
    temp <- readRDS(file.path(inDir100, paste0("100_", scen, "_dfQALY.rds")))%>% gather(i, QALY, -year)
    SQ_QALY <- readRDS(file.path(inDir100, paste0("100_", "SQ", "_dfQALY.rds"))) %>% gather(i, QALY, -year)
  } else if(scen == "TAX5"){
    temp <- bind_cols(readRDS(file.path(inDir, "TAX5", paste0("160_", scen, "_dfQALY.rds"))),
                      readRDS(file.path(inDir, "TAX5", paste0("1000_", scen, "_dfQALY.rds"))) %>% 
                        select(-year)) %>% gather(i, QALY, -year)
    SQ_QALY <- readRDS(file.path(inDir, paste0(pre, "SQ", "_dfQALY.rds"))) %>% gather(i, QALY, -year)
  } else {
    temp <- readRDS(file.path(inDir, paste0(pre, scen, "_dfQALY.rds")))%>% gather(i, QALY, -year)
    SQ_QALY <- readRDS(file.path(inDir, paste0(pre, "SQ", "_dfQALY.rds"))) %>% gather(i, QALY, -year)
  }
  temp$QALY <- temp$QALY - SQ_QALY$QALY
  temp <- temp %>%
    group_by(year) %>%
    summarise(mean=mean(QALY), sd=sd(QALY), CIlow=quantile(QALY, 0.025), CIhigh=quantile(QALY, 0.975))
  df_QALY_NoEcig[[scen]] <- with(temp, paste0(fm0(mean), " (", fm0(mean-f*sd/sqrt(i_sd)), ", ", fm0(mean+f*sd/sqrt(i_sd)), ")"))
}
df_QALY_NoEcig <- df_QALY_NoEcig %>% filter(year %in% chosen_years)
saveRDS(df_QALY_NoEcig, file.path(outDir, paste0(pre, "QALY","_NoEcig", ".rds")))
write.csv(df_QALY_NoEcig, file.path(outDirCSV, paste0(pre, "QALY","_NoEcig", ".csv")), row.names=FALSE)

createQALYDfEcig <- function(big="US"){
  SQ_QALY <- readRDS(file.path(inDir, paste0(pre, "SQ", "_dfQALY.rds"))) %>% gather(i, QALY, -year)
  df_combine <- data.frame(year = (startYear):endYear)
  for(scen in ECIG_SCENARIOS){
    if(scen == "ELF25"){
      temp <- readRDS(file.path(inDir, paste0(pre, "ELF", "_SG", big, "_dfQALY.rds")))%>% gather(i, QALY, -year)
    } else {
      temp <- readRDS(file.path(inDir, paste0(pre, scen, "_SG", big, "_dfQALY.rds")))%>% gather(i, QALY, -year)
    }
    temp$QALY <- temp$QALY - SQ_QALY$QALY
    temp <- temp %>%
      group_by(year) %>%
      summarise(mean=mean(QALY), sd=sd(QALY), CIlow=quantile(QALY, 0.025), CIhigh=quantile(QALY, 0.975))
    if(scen == "ELF25"){
      temp$mean <- temp$mean + cal_ELFE25_diff100(big)$QALY_mean_diff
      temp$sd <- temp$sd * cal_ELFE25_diff100(big)$QALY_sd_ratio
    }
    df_combine[[scen]] <- with(temp, paste0(fm0(mean), " (", fm0(mean-f*sd/sqrt(i_sd)), ", ", fm0(mean+f*sd/sqrt(i_sd)), ")"))
  }
  df_combine <- df_combine %>% filter(year %in% chosen_years)
  saveRDS(df_combine, file.path(outDir, paste0(pre, "QALY","_SG", big, ".rds")))
  write.csv(df_combine, file.path(outDirCSV, paste0(pre, "QALY","_SG", big, ".csv")), row.names = FALSE)
  df_combine
}

df_SGUS_QALY <- createQALYDfEcig("US")
df_SGUK_QALY <- createQALYDfEcig("UK")
df_SGJP_QALY <- createQALYDfEcig("JP")
###################################################################################################