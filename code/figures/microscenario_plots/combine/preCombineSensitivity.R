library(dplyr)
###########################################################################################################
# In and Out Dir
sgDir <- file.path("code", "scenarios_micro", "micro_noecig", "outData")
usDir <- file.path("code", "scenarios_micro", "micro_v0.3", "outData")
ukDir <- file.path("code", "scenarios_micro", "micro_v0.3", "outData")
jpDir <- file.path("code", "scenarios_micro", "micro_v0.3", "outData")
moreNEDir <- file.path("code", "scenarios_micro", "micro_v0.3_sensitivity", "MoreNE_SGUK", "outData")
moreCECDDir <- file.path("code", "scenarios_micro", "micro_v0.3_sensitivity", "MoreCECD_SGUS", "outData")
lessERiskDir <- file.path("code", "scenarios_micro", "micro_v0.3_sensitivity", "riskEcig", "lessRisk", "outData")
moreERiskDir <- file.path("code", "scenarios_micro", "micro_v0.3_sensitivity", "riskEcig", "moreRisk", "outData")
scalingDir <- file.path("code", "scenarios_micro", "micro_v0.3_sensitivity", "scaling", "outData") 
outDir <- file.path("code", "figures", "microscenario_plots", "combine", "output", "precombined")
###########################################################################################################
startYear <- 2017
endYear <- 2067
###########################################################################################################
# Helper functions
processProp <- function(df, type="big") {
  nCol <- ncol(df)
  
  nCol <- nCol
  df <- df[,-nCol] /(1-df[,nCol])
  
  df <- as.data.frame(df*100)
  
  if(type == "big"){
    colnames(df) <- c("N", "C", "Q", "D", "E")
  } else {
    colnames(df) <- c("N", "C", "Q")
  }
  
  df <- df %>%
    mutate(year = (startYear+1): endYear) %>%
    select(year, everything())
  
  df
}

readProp <- function(fpath, type="big", scenario_name) {
  df <- readRDS(fpath)%>%processProp(type=type)
  if(type == "small"){
    df <- df %>% mutate("D"=0, "E"=0)
  }
  df <- df %>% mutate(scenario=scenario_name)
  df
}

processQALY <- function(df) {
  df <- df %>% as.data.frame() 
  
  colnames(df) <- c("QALY")
  
  df <- df %>%
    mutate(year = startYear: endYear) %>%
    select(year, everything())
  
  df
}

readQALY <- function(fpath, scenario_name) {
  df <- (readRDS(fpath) - SQQALY) %>%processQALY()
  df <- df %>% mutate(scenario=scenario_name)
  df
}

###########################################################################################################
# More NE for the UK
combinePropMoreNE <- function(opt="2x"){
  small <- "SG"
  
  df_propSQ <- readProp(file.path(sgDir, "SQ_TR.rds"), "small", "SQ")
  df_propMLA21 <- readProp(file.path(sgDir, "MLA_TR.rds"), "small", "MLA21")
  df_propMLA25 <- readProp(file.path(sgDir, "MLA25_TR.rds"), "small", "MLA25")
  df_propSFG <- readProp(file.path(sgDir, "SFG_TR.rds"), "small", "SFG")
  df_propTax2 <- readProp(file.path(sgDir, "TAX2_TR.rds"), "small", "TAX2")
  df_propTax5 <- readProp(file.path(sgDir, "TAX5_TR.rds"), "small", "TAX5")
  df_propMLATax5 <- readProp(file.path(sgDir, "MLATAX_TR.rds"), "small", "MLATax5")
  
  big <- "UK"
  bigDir <- ukDir
  
  df_propELF <- readProp(file.path(moreNEDir, paste0(opt, "ELF_", small, big, "_TR.rds")), "big", "ELF")
  df_propE25 <- readProp(file.path(moreNEDir, paste0(opt, "ELF25_", small, big, "_TR.rds")), "big", "E25")
  df_propEP <- readProp(file.path(bigDir, paste0("EP_", small, big, "_TR.rds")), "big", "EP")
  df_propMLAEP <- readProp(file.path(bigDir, paste0("MLAEP_", small, big, "_TR.rds")), "big", "MLAEP")
  df_propMLATax5EP <- readProp(file.path(bigDir, paste0("MLATAXEP_", small, big, "_TR.rds")), "big", "MLATax5EP")
  df_propTax5EP <- readProp(file.path(bigDir, paste0("TAXEP_", small, big, "_TR.rds")), "big", "Tax5EP")
  df_propSFGELF <- readProp(file.path(moreNEDir, paste0(opt, "SFGELF_", small, big, "_TR.rds")), "big", "SFGELF")
  
  df_prop <-
    bind_rows(df_propSQ, df_propMLA21, df_propMLA25, df_propSFG, df_propELF, df_propE25,
              df_propEP, df_propTax2, df_propTax5, df_propMLAEP, df_propMLATax5, df_propMLATax5EP, 
              df_propTax5EP, df_propSFGELF) %>%
    mutate(NandQ = N + Q,
           CandD = C + D,
           CDE = C + D + E) 
  df_prop
}

combineQALYMoreNE <- function(opt="2x"){
  small <- "SG"
  
  df_QALYSQ <- readQALY(file.path(sgDir, "SQ_colSumME.rds"), "SQ")
  df_QALYMLA21 <- readQALY(file.path(sgDir, "MLA_colSumME.rds"), "MLA21")
  df_QALYMLA25 <- readQALY(file.path(sgDir, "MLA25_colSumME.rds"), "MLA25")
  df_QALYSFG <- readQALY(file.path(sgDir, "SFG_colSumME.rds"), "SFG")
  df_QALYTax2 <- readQALY(file.path(sgDir, "TAX2_colSumME.rds"),  "TAX2")
  df_QALYTax5 <- readQALY(file.path(sgDir, "TAX5_colSumME.rds"), "TAX5")
  df_QALYMLATax5 <- readQALY(file.path(sgDir, "MLATAX_colSumME.rds"), "MLATax5")
  
  big <- "UK"
  bigDir <- ukDir
  
  df_QALYELF <- readQALY(file.path(moreNEDir, paste0(opt, "ELF_", small, big, "_colSumME.rds")), "ELF")
  df_QALYE25 <- readQALY(file.path(moreNEDir, paste0(opt, "ELF25_", small, big, "_colSumME.rds")), "E25")
  df_QALYEP <- readQALY(file.path(bigDir, paste0("EP_", small, big, "_colSumME.rds")), "EP")
  df_QALYMLAEP <- readQALY(file.path(bigDir, paste0("MLAEP_", small, big, "_colSumME.rds")), "MLAEP")
  df_QALYMLATax5EP <- readQALY(file.path(bigDir, paste0("MLATAXEP_", small, big, "_colSumME.rds")), "MLATax5EP")
  df_QALYTax5EP <- readQALY(file.path(bigDir, paste0("TAXEP_", small, big, "_colSumME.rds")), "Tax5EP")
  df_QALYSFGELF <- readQALY(file.path(moreNEDir, paste0(opt, "SFGELF_", small, big, "_colSumME.rds")), "SFGELF")
  
  df_QALY <-
    bind_rows(df_QALYSQ, df_QALYMLA21, df_QALYMLA25, df_QALYSFG, df_QALYELF, df_QALYE25,
              df_QALYEP, df_QALYTax2, df_QALYTax5, df_QALYMLAEP, df_QALYMLATax5, df_QALYMLATax5EP, 
              df_QALYTax5EP, df_QALYSFGELF) 
  
  df_QALY
}

# Read and combine data
df_prop_moreNE2x <- combinePropMoreNE(opt="2x")
saveRDS(df_prop_moreNE2x, file.path(outDir, "df_prop_moreNE2x.rds"))
df_prop_moreNE3x <- combinePropMoreNE(opt="3x")
saveRDS(df_prop_moreNE3x, file.path(outDir, "df_prop_moreNE3x.rds"))

SQQALY <- readRDS(file.path(sgDir, "SQ_colSumME.rds"))
df_QALY_moreNE2x <- combineQALYMoreNE(opt="2x")
saveRDS(df_QALY_moreNE2x, file.path(outDir, "df_QALY_moreNE2x.rds"))
df_QALY_moreNE3x <- combineQALYMoreNE(opt="3x")
saveRDS(df_QALY_moreNE3x, file.path(outDir, "df_QALY_moreNE3x.rds"))
###########################################################################################################
# More CECD for the US
combinePropMoreCECD <- function(opt="2x"){
  small <- "SG"
  
  df_propSQ <- readProp(file.path(sgDir, "SQ_TR.rds"), "small", "SQ")
  df_propMLA21 <- readProp(file.path(sgDir, "MLA_TR.rds"), "small", "MLA21")
  df_propMLA25 <- readProp(file.path(sgDir, "MLA25_TR.rds"), "small", "MLA25")
  df_propSFG <- readProp(file.path(sgDir, "SFG_TR.rds"), "small", "SFG")
  df_propTax2 <- readProp(file.path(sgDir, "TAX2_TR.rds"), "small", "TAX2")
  df_propTax5 <- readProp(file.path(sgDir, "TAX5_TR.rds"), "small", "TAX5")
  df_propMLATax5 <- readProp(file.path(sgDir, "MLATAX_TR.rds"), "small", "MLATax5")
  
  big <- "US"
  bigDir <- usDir
  
  df_propELF <- readProp(file.path(moreCECDDir, paste0(opt, "ELF_", small, big, "_TR.rds")), "big", "ELF")
  df_propE25 <- readProp(file.path(moreCECDDir, paste0(opt, "ELF25_", small, big, "_TR.rds")), "big", "E25")
  df_propEP <- readProp(file.path(bigDir, paste0("EP_", small, big, "_TR.rds")), "big", "EP")
  df_propMLAEP <- readProp(file.path(bigDir, paste0("MLAEP_", small, big, "_TR.rds")), "big", "MLAEP")
  df_propMLATax5EP <- readProp(file.path(bigDir, paste0("MLATAXEP_", small, big, "_TR.rds")), "big", "MLATax5EP")
  df_propTax5EP <- readProp(file.path(bigDir, paste0("TAXEP_", small, big, "_TR.rds")), "big", "Tax5EP")
  df_propSFGELF <- readProp(file.path(moreCECDDir, paste0(opt, "SFGELF_", small, big, "_TR.rds")), "big", "SFGELF")
  
  df_prop <-
    bind_rows(df_propSQ, df_propMLA21, df_propMLA25, df_propSFG, df_propELF, df_propE25,
              df_propEP, df_propTax2, df_propTax5, df_propMLAEP, df_propMLATax5, df_propMLATax5EP, 
              df_propTax5EP, df_propSFGELF) %>%
    mutate(NandQ = N + Q,
           CandD = C + D,
           CDE = C + D + E) 
  df_prop
}

combineQALYMoreCECD <- function(opt="2x"){
  small <- "SG"
  
  df_QALYSQ <- readQALY(file.path(sgDir, "SQ_colSumME.rds"), "SQ")
  df_QALYMLA21 <- readQALY(file.path(sgDir, "MLA_colSumME.rds"), "MLA21")
  df_QALYMLA25 <- readQALY(file.path(sgDir, "MLA25_colSumME.rds"), "MLA25")
  df_QALYSFG <- readQALY(file.path(sgDir, "SFG_colSumME.rds"), "SFG")
  df_QALYTax2 <- readQALY(file.path(sgDir, "TAX2_colSumME.rds"),  "TAX2")
  df_QALYTax5 <- readQALY(file.path(sgDir, "TAX5_colSumME.rds"), "TAX5")
  df_QALYMLATax5 <- readQALY(file.path(sgDir, "MLATAX_colSumME.rds"), "MLATax5")
  
  big <- "US"
  bigDir <- usDir
  
  df_QALYELF <- readQALY(file.path(moreCECDDir, paste0(opt, "ELF_", small, big, "_colSumME.rds")), "ELF")
  df_QALYE25 <- readQALY(file.path(moreCECDDir, paste0(opt, "ELF25_", small, big, "_colSumME.rds")), "E25")
  df_QALYEP <- readQALY(file.path(bigDir, paste0("EP_", small, big, "_colSumME.rds")), "EP")
  df_QALYMLAEP <- readQALY(file.path(bigDir, paste0("MLAEP_", small, big, "_colSumME.rds")), "MLAEP")
  df_QALYMLATax5EP <- readQALY(file.path(bigDir, paste0("MLATAXEP_", small, big, "_colSumME.rds")), "MLATax5EP")
  df_QALYTax5EP <- readQALY(file.path(bigDir, paste0("TAXEP_", small, big, "_colSumME.rds")), "Tax5EP")
  df_QALYSFGELF <- readQALY(file.path(moreCECDDir, paste0(opt, "SFGELF_", small, big, "_colSumME.rds")), "SFGELF")
  
  df_QALY <-
    bind_rows(df_QALYSQ, df_QALYMLA21, df_QALYMLA25, df_QALYSFG, df_QALYELF, df_QALYE25,
              df_QALYEP, df_QALYTax2, df_QALYTax5, df_QALYMLAEP, df_QALYMLATax5, df_QALYMLATax5EP, 
              df_QALYTax5EP, df_QALYSFGELF) 
  
  df_QALY
}

# Read and combine data
df_prop_moreCECD2x <- combinePropMoreCECD(opt="2x")
saveRDS(df_prop_moreCECD2x, file.path(outDir, "df_prop_moreCECD2x.rds"))
df_prop_moreCECD3x <- combinePropMoreCECD(opt="3x")
saveRDS(df_prop_moreCECD3x, file.path(outDir, "df_prop_moreCECD3x.rds"))

SQQALY <- readRDS(file.path(sgDir, "SQ_colSumME.rds"))
df_QALY_moreCECD2x <- combineQALYMoreCECD(opt="2x")
saveRDS(df_QALY_moreCECD2x, file.path(outDir, "df_QALY_moreCECD2x.rds"))
df_QALY_moreCECD3x <- combineQALYMoreCECD(opt="3x")
saveRDS(df_QALY_moreCECD3x, file.path(outDir, "df_QALY_moreCECD3x.rds"))
###########################################################################################################
# Less or More Risk for Ecig
combinePropERisk <- function(opt="more"){
  small <- "SG"
  
  df_propSQ <- readProp(file.path(sgDir, "SQ_TR.rds"), "small", "SQ")
  df_propMLA21 <- readProp(file.path(sgDir, "MLA_TR.rds"), "small", "MLA21")
  df_propMLA25 <- readProp(file.path(sgDir, "MLA25_TR.rds"), "small", "MLA25")
  df_propSFG <- readProp(file.path(sgDir, "SFG_TR.rds"), "small", "SFG")
  df_propTax2 <- readProp(file.path(sgDir, "TAX2_TR.rds"), "small", "TAX2")
  df_propTax5 <- readProp(file.path(sgDir, "TAX5_TR.rds"), "small", "TAX5")
  df_propMLATax5 <- readProp(file.path(sgDir, "MLATAX_TR.rds"), "small", "MLATax5")
  
  big <- "US"
  bigDir <- usDir
  
  eRiskDir <- case_when(
    opt=="less" ~ lessERiskDir,
    TRUE ~ moreERiskDir
  )
  
  df_propELF <- readProp(file.path(eRiskDir, paste0("ELF_", small, big, "_TR.rds")), "big", "ELF")
  df_propE25 <- readProp(file.path(eRiskDir, paste0("ELF25_", small, big, "_TR.rds")), "big", "E25")
  df_propEP <- readProp(file.path(eRiskDir, paste0("EP_", small, big, "_TR.rds")), "big", "EP")
  df_propMLAEP <- readProp(file.path(eRiskDir, paste0("MLAEP_", small, big, "_TR.rds")), "big", "MLAEP")
  df_propMLATax5EP <- readProp(file.path(eRiskDir, paste0("MLATAXEP_", small, big, "_TR.rds")), "big", "MLATax5EP")
  df_propTax5EP <- readProp(file.path(eRiskDir, paste0("TAXEP_", small, big, "_TR.rds")), "big", "Tax5EP")
  df_propSFGELF <- readProp(file.path(eRiskDir, paste0("SFGELF_", small, big, "_TR.rds")), "big", "SFGELF")
  
  df_prop <-
    bind_rows(df_propSQ, df_propMLA21, df_propMLA25, df_propSFG, df_propELF, df_propE25,
              df_propEP, df_propTax2, df_propTax5, df_propMLAEP, df_propMLATax5, df_propMLATax5EP, 
              df_propTax5EP, df_propSFGELF) %>%
    mutate(NandQ = N + Q,
           CandD = C + D,
           CDE = C + D + E) 
  df_prop
}

combineQALYERisk <- function(opt="more"){
  small <- "SG"
  
  df_QALYSQ <- readQALY(file.path(sgDir, "SQ_colSumME.rds"), "SQ")
  df_QALYMLA21 <- readQALY(file.path(sgDir, "MLA_colSumME.rds"), "MLA21")
  df_QALYMLA25 <- readQALY(file.path(sgDir, "MLA25_colSumME.rds"), "MLA25")
  df_QALYSFG <- readQALY(file.path(sgDir, "SFG_colSumME.rds"), "SFG")
  df_QALYTax2 <- readQALY(file.path(sgDir, "TAX2_colSumME.rds"),  "TAX2")
  df_QALYTax5 <- readQALY(file.path(sgDir, "TAX5_colSumME.rds"), "TAX5")
  df_QALYMLATax5 <- readQALY(file.path(sgDir, "MLATAX_colSumME.rds"), "MLATax5")
  
  big <- "US"
  bigDir <- usDir
  
  eRiskDir <- case_when(
    opt=="less" ~ lessERiskDir,
    TRUE ~ moreERiskDir
  )
  
  df_QALYELF <- readQALY(file.path(eRiskDir, paste0("ELF_", small, big, "_colSumME.rds")), "ELF")
  df_QALYE25 <- readQALY(file.path(eRiskDir, paste0("ELF25_", small, big, "_colSumME.rds")), "E25")
  df_QALYEP <- readQALY(file.path(eRiskDir, paste0("EP_", small, big, "_colSumME.rds")), "EP")
  df_QALYMLAEP <- readQALY(file.path(eRiskDir, paste0("MLAEP_", small, big, "_colSumME.rds")), "MLAEP")
  df_QALYMLATax5EP <- readQALY(file.path(eRiskDir, paste0("MLATAXEP_", small, big, "_colSumME.rds")), "MLATax5EP")
  df_QALYTax5EP <- readQALY(file.path(eRiskDir, paste0("TAXEP_", small, big, "_colSumME.rds")), "Tax5EP")
  df_QALYSFGELF <- readQALY(file.path(eRiskDir, paste0("SFGELF_", small, big, "_colSumME.rds")), "SFGELF")
  
  df_QALY <-
    bind_rows(df_QALYSQ, df_QALYMLA21, df_QALYMLA25, df_QALYSFG, df_QALYELF, df_QALYE25,
              df_QALYEP, df_QALYTax2, df_QALYTax5, df_QALYMLAEP, df_QALYMLATax5, df_QALYMLATax5EP, 
              df_QALYTax5EP, df_QALYSFGELF) 
  
  df_QALY
}

# Read and combine data
df_prop_moreERisk <- combinePropERisk(opt="more")
saveRDS(df_prop_moreERisk, file.path(outDir, "df_prop_moreERisk.rds"))
df_prop_lessErisk <- combinePropERisk(opt="less")
saveRDS(df_prop_lessErisk, file.path(outDir, "df_prop_lessERisk.rds"))

SQQALY <- readRDS(file.path(sgDir, "SQ_colSumME.rds"))
df_QALY_moreERisk <- combineQALYERisk(opt="more")
saveRDS(df_QALY_moreERisk, file.path(outDir, "df_QALY_moreERisk.rds"))
df_QALY_lessERisk <- combineQALYERisk(opt="less")
saveRDS(df_QALY_lessERisk, file.path(outDir, "df_QALY_lessERisk.rds"))

###########################################################################################################
# Different scaling for Ecig
combinePropScaling <- function(){
  small <- "SG"
  
  df_propSQ <- readProp(file.path(sgDir, "SQ_TR.rds"), "small", "SQ")
  df_propMLA21 <- readProp(file.path(sgDir, "MLA_TR.rds"), "small", "MLA21")
  df_propMLA25 <- readProp(file.path(sgDir, "MLA25_TR.rds"), "small", "MLA25")
  df_propSFG <- readProp(file.path(sgDir, "SFG_TR.rds"), "small", "SFG")
  df_propTax2 <- readProp(file.path(sgDir, "TAX2_TR.rds"), "small", "TAX2")
  df_propTax5 <- readProp(file.path(sgDir, "TAX5_TR.rds"), "small", "TAX5")
  df_propMLATax5 <- readProp(file.path(sgDir, "MLATAX_TR.rds"), "small", "MLATax5")
  
  big <- "US"
  bigDir <- usDir
  
  df_propELF <- readProp(file.path(scalingDir, paste0("ELF_", small, big, "_TR.rds")), "big", "ELF")
  df_propE25 <- readProp(file.path(scalingDir, paste0("ELF25_", small, big, "_TR.rds")), "big", "E25")
  df_propEP <- readProp(file.path(scalingDir, paste0("EP_", small, big, "_TR.rds")), "big", "EP")
  df_propMLAEP <- readProp(file.path(scalingDir, paste0("MLAEP_", small, big, "_TR.rds")), "big", "MLAEP")
  df_propMLATax5EP <- readProp(file.path(scalingDir, paste0("MLATAXEP_", small, big, "_TR.rds")), "big", "MLATax5EP")
  df_propTax5EP <- readProp(file.path(scalingDir, paste0("TAXEP_", small, big, "_TR.rds")), "big", "Tax5EP")
  df_propSFGELF <- readProp(file.path(scalingDir, paste0("SFGELF_", small, big, "_TR.rds")), "big", "SFGELF")
  
  df_prop <-
    bind_rows(df_propSQ, df_propMLA21, df_propMLA25, df_propSFG, df_propELF, df_propE25,
              df_propEP, df_propTax2, df_propTax5, df_propMLAEP, df_propMLATax5, df_propMLATax5EP, 
              df_propTax5EP, df_propSFGELF) %>%
    mutate(NandQ = N + Q,
           CandD = C + D,
           CDE = C + D + E) 
  df_prop
}

combineQALYScaling<- function(){
  small <- "SG"
  
  df_QALYSQ <- readQALY(file.path(sgDir, "SQ_colSumME.rds"), "SQ")
  df_QALYMLA21 <- readQALY(file.path(sgDir, "MLA_colSumME.rds"), "MLA21")
  df_QALYMLA25 <- readQALY(file.path(sgDir, "MLA25_colSumME.rds"), "MLA25")
  df_QALYSFG <- readQALY(file.path(sgDir, "SFG_colSumME.rds"), "SFG")
  df_QALYTax2 <- readQALY(file.path(sgDir, "TAX2_colSumME.rds"),  "TAX2")
  df_QALYTax5 <- readQALY(file.path(sgDir, "TAX5_colSumME.rds"), "TAX5")
  df_QALYMLATax5 <- readQALY(file.path(sgDir, "MLATAX_colSumME.rds"), "MLATax5")
  
  big <- "US"
  bigDir <- usDir
  
  df_QALYELF <- readQALY(file.path(scalingDir, paste0("ELF_", small, big, "_colSumME.rds")), "ELF")
  df_QALYE25 <- readQALY(file.path(scalingDir, paste0("ELF25_", small, big, "_colSumME.rds")), "E25")
  df_QALYEP <- readQALY(file.path(scalingDir, paste0("EP_", small, big, "_colSumME.rds")), "EP")
  df_QALYMLAEP <- readQALY(file.path(scalingDir, paste0("MLAEP_", small, big, "_colSumME.rds")), "MLAEP")
  df_QALYMLATax5EP <- readQALY(file.path(scalingDir, paste0("MLATAXEP_", small, big, "_colSumME.rds")), "MLATax5EP")
  df_QALYTax5EP <- readQALY(file.path(scalingDir, paste0("TAXEP_", small, big, "_colSumME.rds")), "Tax5EP")
  df_QALYSFGELF <- readQALY(file.path(scalingDir, paste0("SFGELF_", small, big, "_colSumME.rds")), "SFGELF")
  
  df_QALY <-
    bind_rows(df_QALYSQ, df_QALYMLA21, df_QALYMLA25, df_QALYSFG, df_QALYELF, df_QALYE25,
              df_QALYEP, df_QALYTax2, df_QALYTax5, df_QALYMLAEP, df_QALYMLATax5, df_QALYMLATax5EP, 
              df_QALYTax5EP, df_QALYSFGELF) 
  
  df_QALY
}

# Read and combine data
df_prop_scaling <- combinePropScaling()
saveRDS(df_prop_scaling, file.path(outDir, "df_prop_scaling.rds"))


SQQALY <- readRDS(file.path(sgDir, "SQ_colSumME.rds"))
df_QALY_scaling <- combineQALYScaling()
saveRDS(df_QALY_scaling, file.path(outDir, "df_QALY_scaling.rds"))