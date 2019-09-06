library(tidyverse)
library(readxl)

############################################################################################
## In directories
smokeDatPath <- 'data/singapore/smoking_data.xlsx'
## Out directories
outDir <- 'code/tables/other_tables/out_tables'
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
           'Ex Smoker' = "Ex-smoker",
           'Never Smoker' = "Non-smoker") %>%
    mutate("Current Smoker" = sum(DailySmoker, OccasionalSmoker, na.rm=T)) %>%
    select(-c(DailySmoker, OccasionalSmoker)) %>%
    gather(key=smoking_status, value=prevalence, -year) %>%
    mutate(smoking_status=factor(smoking_status, 
                                 levels=c("Never Smoker", "Current Smoker", "Ex Smoker"))) %>%
    arrange(smoking_status) %>%
    mutate(prevalence = prevalence*100)

write.csv(actual_dat, file.path(outDir, "sin_prev_table.csv"), row.names = FALSE)
