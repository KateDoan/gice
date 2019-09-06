###################################################################################################
cal_ELFE25_diff100 <- function(big){
  require(dplyr)
  require(tidyr)
  inDir100 <- file.path("code", "tables", "combined_microtables", "input", paste0("input", "100"))
  ###################################################################################################
  E25_prop <- readRDS(file.path(inDir100, paste0("100_", "ELF25", "_SG", big, "_summaryTR.rds")))
  ELF_prop <- readRDS(file.path(inDir100, paste0("100_", "ELF", "_SG", big, "_summaryTR.rds")))
  ###################################################################################################
  SQ_QALY <- readRDS(file.path(inDir100, paste0("100_", "SQ", "_dfQALY.rds")))
  SQ_QALY <- SQ_QALY %>% gather(i, QALY, -year)
  
  E25_QALY <- readRDS(file.path(inDir100, paste0("100_", "ELF25", "_SG", big, "_dfQALY.rds")))%>% gather(i, QALY, -year)
  E25_QALY$QALY <- E25_QALY$QALY - SQ_QALY$QALY
  E25_QALY <- E25_QALY %>%
    group_by(year) %>%
    summarise(mean=mean(QALY), sd=sd(QALY))
  
  ELF_QALY <- readRDS(file.path(inDir100, paste0("100_", "ELF", "_SG", big, "_dfQALY.rds")))%>% gather(i, QALY, -year)
  ELF_QALY$QALY <- ELF_QALY$QALY - SQ_QALY$QALY
  ELF_QALY <- ELF_QALY %>%
    group_by(year) %>%
    summarise(mean=mean(QALY), sd=sd(QALY))
  ###################################################################################################
  prop_mean_diff <- E25_prop$mean - ELF_prop$mean
  prop_sd_ratio <- E25_prop$sd/ELF_prop$sd; prop_sd_ratio[is.na(prop_sd_ratio)] <- 0
  QALY_mean_diff <- E25_QALY$mean - ELF_QALY$mean
  QALY_sd_ratio <- E25_QALY$sd/ELF_QALY$sd; QALY_sd_ratio[is.na(QALY_sd_ratio)] <- 0
  
  list(prop_mean_diff=prop_mean_diff,
       prop_sd_ratio=prop_sd_ratio,
       QALY_mean_diff=QALY_mean_diff,
       QALY_sd_ratio=QALY_sd_ratio)
}
