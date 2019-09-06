library(dplyr)
###########################################################################################################
smallDatDir <- file.path("code", "scenarios_micro", "micro_noecig", "outData")
bigDatDir <- file.path("code", "scenarios_micro", "micro_v0.3", "outData")
outDir <- file.path("code", "scenarios_micro", "micro_v0.3", "combine", "combined_data")
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

readProp <- function(fname, type="big", scenario_name) {
  if(type == "small"){
      datDir = smallDatDir
  } else {
      datDir = bigDatDir
  }
  df <- readRDS(file.path(datDir, fname))%>%processProp(type=type)
  if(type == "small"){
    df <- df %>% mutate("D"=0, "E"=0)
  }
  df <- df %>% mutate(scenario=scenario_name)
  df
}

combineProp <- function(big="US", small="SG"){
  df_propSQ <- readProp("SQ_TR.rds", "small", "SQ")
  df_propMLA21 <- readProp("MLA_TR.rds", "small", "MLA21")
  df_propMLA25 <- readProp("MLA25_TR.rds", "small", "MLA25")
  df_propSFG <- readProp("SFG_TR.rds", "small", "SFG")
  df_propTax2 <- readProp("TAX2_TR.rds", "small", "TAX2")
  df_propTax5 <- readProp("TAX5_TR.rds", "small", "TAX5")
  df_propELF <- readProp(paste0("ELF_", small, big, "_TR.rds"), "big", "ELF")
  df_propE25 <- readProp(paste0("ELF25_", small, big, "_TR.rds"), "big", "E25")
  df_propEP <- readProp(paste0("EP_", small, big, "_TR.rds"), "big", "EP")
  df_propMLAEP <- readProp(paste0("MLAEP_", small, big, "_TR.rds"), "big", "MLAEP")
  df_propMLATax5 <- readProp("MLATAX_TR.rds", "small", "MLATax5")
  df_propMLATax5EP <- readProp(paste0("MLATAXEP_", small, big, "_TR.rds"), "big", "MLATax5EP")
  df_propTax5EP <- readProp(paste0("TAXEP_", small, big, "_TR.rds"), "big", "Tax5EP")
  df_propSFGELF <- readProp(paste0("SFGELF_", small, big, "_TR.rds"), "big", "SFGELF")
  
  df_prop <-
    bind_rows(df_propSQ, df_propMLA21, df_propMLA25, df_propSFG, df_propELF, df_propE25,
              df_propEP, df_propTax2, df_propTax5, df_propMLAEP, df_propMLATax5, df_propMLATax5EP, 
              df_propTax5EP, df_propSFGELF) %>%
    mutate(NandQ = N + Q,
           CandD = C + D,
           CDE = C + D + E) 
  df_prop
}

processQALY <- function(df) {
  df <- df %>% as.data.frame() 
  
  colnames(df) <- c("QALY")
  
  df <- df %>%
    mutate(year = startYear: endYear) %>%
    select(year, everything())
  
  df
}

readQALY <- function(fname, type="big", scenario_name) {
  if(type == "small"){
      datDir = smallDatDir
  } else {
      datDir = bigDatDir
  }
  df <- (readRDS(file.path(datDir, fname)) - SQQALY) %>%processQALY()
  df <- df %>% mutate(scenario=scenario_name)
  df
}

combineQALY <- function(big="US", small="SG"){
  df_QALYSQ <- readQALY("SQ_colSumME.rds", "small", "SQ")
  df_QALYMLA21 <- readQALY("MLA_colSumME.rds", "small", "MLA21")
  df_QALYMLA25 <- readQALY("MLA25_colSumME.rds", "small", "MLA25")
  df_QALYSFG <- readQALY("SFG_colSumME.rds", "small", "SFG")
  df_QALYTax2 <- readQALY("TAX2_colSumME.rds", "small", "TAX2")
  df_QALYTax5 <- readQALY("TAX5_colSumME.rds", "small", "TAX5")
  df_QALYELF <- readQALY(paste0("ELF_", small, big, "_colSumME.rds"), "big", "ELF")
  df_QALYE25 <- readQALY(paste0("ELF25_", small, big, "_colSumME.rds"), "big", "E25")
  df_QALYEP <- readQALY(paste0("EP_", small, big, "_colSumME.rds"), "big", "EP")
  df_QALYMLAEP <- readQALY(paste0("MLAEP_", small, big, "_colSumME.rds"), "big", "MLAEP")
  df_QALYMLATax5 <- readQALY("MLATAX_colSumME.rds", "small", "MLATax5")
  df_QALYMLATax5EP <- readQALY(paste0("MLATAXEP_", small, big, "_colSumME.rds"), "big", "MLATax5EP")
  df_QALYTax5EP <- readQALY(paste0("TAXEP_", small, big, "_colSumME.rds"), "big", "Tax5EP")
  df_QALYSFGELF <- readQALY(paste0("SFGELF_", small, big, "_colSumME.rds"), "big", "SFGELF")
  
  df_QALY <-
    bind_rows(df_QALYSQ, df_QALYMLA21, df_QALYMLA25, df_QALYSFG, df_QALYELF, df_QALYE25,
              df_QALYEP, df_QALYTax2, df_QALYTax5, df_QALYMLAEP, df_QALYMLATax5, df_QALYMLATax5EP, 
              df_QALYTax5EP, df_QALYSFGELF) 
  df_QALY
}

###########################################################################################################
# Read and combine data
df_prop_SGUS <- combineProp("US", "SG")
df_prop_SGUK <- combineProp("UK", "SG")
df_prop_SGJP <- combineProp("JP", "SG")
saveRDS(df_prop_SGUS, file.path(outDir, "df_prop_SGUS.rds"))
saveRDS(df_prop_SGUK, file.path(outDir, "df_prop_SGUK.rds"))
saveRDS(df_prop_SGJP, file.path(outDir, "df_prop_SGJP.rds"))

SQQALY <- readRDS(file.path(smallDatDir, "SQ_colSumME.rds"))
df_QALY_SGUS <- combineQALY("US", "SG")
df_QALY_SGUK <- combineQALY("UK", "SG")
df_QALY_SGJP <- combineQALY("JP", "SG")
saveRDS(df_QALY_SGUS, file.path(outDir, "df_QALY_SGUS.rds"))
saveRDS(df_QALY_SGUK, file.path(outDir, "df_QALY_SGUK.rds"))
saveRDS(df_QALY_SGJP, file.path(outDir, "df_QALY_SGJP.rds"))
