library(tidyverse)
#########################################################################################
## In directories
bigMatDirUS <- file.path("code", "transitions", "path1_pool", "transition_output")
## Out directories
outDir <- file.path("code", "tables", "transition_tables", "tables_output")
csvOutDir <- file.path(outDir, "csv_output")
#########################################################################################
# Parameters
f <- 1.96
trans <- c("NN","NC","ND","NE",
           "CC","CQ","CD","CE",
           "QC","QQ","QD","QE",
           "DC","DQ","DD","DE",
           "EC","EQ","ED","EE")
#########################################################################################
# Helper functions
fm <- function(x){
    format(round(x,1), nsmall = 1)
} 

createTranMat <- function(inDf, inDf_sd, age_group = ""){
    tranMat <- data.frame(
        transition = factor(trans, levels=trans),
        mean = c(inDf$NN, inDf$NC, inDf$ND, inDf$NE,
                  inDf$CC, inDf$CQ, inDf$CD, inDf$CE,
                  inDf$QC, inDf$QQ, inDf$QD, inDf$QE,
                  inDf$DC, inDf$DQ, inDf$DD, inDf$DE,
                  inDf$EC, inDf$EQ, inDf$ED, inDf$EE),
        sd = c(inDf_sd$NN, inDf_sd$NC, inDf_sd$ND, inDf_sd$NE,
               inDf_sd$CC, inDf_sd$CQ, inDf_sd$CD, inDf_sd$CE,
               inDf_sd$QC, inDf_sd$QQ, inDf_sd$QD, inDf_sd$QE,
               inDf_sd$DC, inDf_sd$DQ, inDf_sd$DD, inDf_sd$DE,
               inDf_sd$EC, inDf_sd$EQ, inDf_sd$ED, inDf_sd$EE)
    ) %>% mutate(mean = mean*100, sd = sd*100
    ) %>% mutate(lower = pmax(0,mean - f*sd),
                 upper = pmin(100,mean + f*sd))
    
    tranMat <- tranMat %>% 
        mutate(age_group=age_group) %>%
        select(age_group, everything())
    
    tranMat
}

createTranMatOld <- function(inDf, inDf_sd, age_group = ""){
  tranMat <- data.frame(
    transition = factor(trans, levels=trans),
    mean = c(inDf$NN+inDf$ND+inDf$NE, inDf$NC, 0, 0,
             inDf$CC, inDf$CQ, inDf$CD, inDf$CE,
             inDf$QC, inDf$QQ, inDf$QD, inDf$QE,
             inDf$DC, inDf$DQ, inDf$DD, inDf$DE,
             inDf$EC, inDf$EQ, inDf$ED, inDf$EE),
    sd = c(inDf_sd$NN+inDf_sd$ND+inDf_sd$NE, inDf_sd$NC, 0, 0,
           inDf_sd$CC, inDf_sd$CQ, inDf_sd$CD, inDf_sd$CE,
           inDf_sd$QC, inDf_sd$QQ, inDf_sd$QD, inDf_sd$QE,
           inDf_sd$DC, inDf_sd$DQ, inDf_sd$DD, inDf_sd$DE,
           inDf_sd$EC, inDf_sd$EQ, inDf_sd$ED, inDf_sd$EE)
  ) %>% mutate(mean = mean*100, sd = sd*100
  ) %>% mutate(lower = pmax(0,mean - f*sd),
               upper = pmin(100,mean + f*sd)
  ) 
  
  tranMat <- tranMat %>% 
    mutate(age_group=age_group) %>%
    select(age_group, everything())
  
  tranMat
}
#########################################################################################
tm1217 <- readRDS(file.path(bigMatDirUS,"params_pos1217.rds"))
tm1217_sd <- readRDS(file.path(bigMatDirUS,"params_sd_pos1217.rds"))
UStm1217 <- createTranMat(tm1217, tm1217_sd, "12-17")

tm1824 <- readRDS(file.path(bigMatDirUS,"params_pos1824.rds"))
tm1824_sd <- readRDS(file.path(bigMatDirUS,"params_sd_pos1824.rds"))
UStm1824 <- createTranMat(tm1824, tm1824_sd, "18-24")

tm2534 <- readRDS(file.path(bigMatDirUS,"params_pos2534.rds"))
tm2534_sd <- readRDS(file.path(bigMatDirUS,"params_sd_pos2534.rds"))
UStm2534 <- createTranMatOld(tm2534, tm2534_sd, "25-34")

tm3544 <- readRDS(file.path(bigMatDirUS,"params_pos3544.rds"))
tm3544_sd <- readRDS(file.path(bigMatDirUS,"params_sd_pos3544.rds"))
UStm3544 <- createTranMatOld(tm3544, tm3544_sd, "35-44")

tm4554 <- readRDS(file.path(bigMatDirUS,"params_pos4554.rds"))
tm4554_sd <- readRDS(file.path(bigMatDirUS,"params_sd_pos4554.rds"))
UStm4554 <- createTranMatOld(tm4554, tm4554_sd, "45-54")

tm5564 <- readRDS(file.path(bigMatDirUS,"params_pos5564.rds"))
tm5564_sd <- readRDS(file.path(bigMatDirUS,"params_sd_pos5564.rds"))
UStm5564 <- createTranMatOld(tm5564, tm5564_sd, "55-64")

tm6574 <- readRDS(file.path(bigMatDirUS,"params_pos6574.rds"))
tm6574_sd <- readRDS(file.path(bigMatDirUS,"params_sd_pos6574.rds"))
UStm6574 <- createTranMatOld(tm6574, tm6574_sd, "65-74")

tm75plus <- readRDS(file.path(bigMatDirUS,"params_pos75plus.rds"))
tm75plus_sd <- readRDS(file.path(bigMatDirUS,"params_sd_pos75plus.rds"))
UStm75plus <- createTranMatOld(tm75plus, tm75plus_sd, "75-80")
#########################################################################################
combinedUSTransTableRaw <- 
    UStm1217 %>% 
    bind_rows(UStm1824,UStm2534,UStm3544,UStm4554,UStm5564,UStm6574,UStm75plus) 

combinedUSTransTable <- combinedUSTransTableRaw %>%
      mutate(posterior = paste0(fm(mean)," (",fm(lower),", ",fm(upper),")")) %>% 
      select(age_group, transition, posterior) %>%
      spread(age_group, posterior)
#########################################################################################
saveRDS(combinedUSTransTable, file.path(outDir, "USTm2.rds"))
write.csv(combinedUSTransTable, file.path(csvOutDir, "USTm2.csv"), row.names = FALSE)
#########################################################################################
UStm_compare <- combinedUSTransTableRaw %>%
  separate(age_group, into=c("age_start", "age_end"), sep="-") %>%
  mutate(age_start=parse_number(age_start), age_end=parse_number(age_end)) %>%
  mutate(age_end = age_end+1) %>%
  select(age_start, age_end, everything())
saveRDS(UStm_compare, file.path(outDir, "UStm_compare.rds"))
