outDir <- outDirNoEcig
# Prevalence 
years <- startYear:endYear
idxStartTrunc <- getAgeIndex(startAgeTrunc, startAge)
idxEndTrunc <- getAgeIndex(endAgeTrunc, startAge)
snapshotNList <- colSums(numNArray[idxStartTrunc:idxEndTrunc,])
snapshotCList <- colSums(numCArray[idxStartTrunc:idxEndTrunc,])
snapshotQList <- colSums(numQArray[idxStartTrunc:idxEndTrunc,])

tempMat <- cbind(N = snapshotNList, 
                 C = snapshotCList, 
                 Q = snapshotQList)
df_prop <-  tempMat %>%
  prop.table(margin = 1) %>% 
  as.data.frame()
df_prop <- df_prop * 100
df_prop <- df_prop %>% 
  mutate(years = years) %>%
  select(years, everything())
saveRDS(df_prop, file.path(outDir, paste0("df_prop", scenario_name, ".rds")))

# Number of Users
df_num <- data.frame(years = years,
                     C = snapshotCList) %>%
  gather("group", "num", -1)
saveRDS(df_num, file.path(outDir, paste0("df_num", scenario_name, ".rds")))

# Health Impact 
# Premature Deaths
PD_C <- ((deathRateC - deathRateN) * numCArray)[idxStartTrunc:idxEndTrunc,]
PD_Q <- ((deathRateQ - deathRateN) * numQArray)[idxStartTrunc:idxEndTrunc,]
PDsum <- PD_C + PD_Q
PD <- colSums(PDsum)
cumsumPD <- cumsum(PD)
df_PD <- data.frame(years = years,
                    PDsum = PD,
                    cumsumPD = cumsumPD)
saveRDS(df_PD, file.path(outDir, paste0("df_PD", scenario_name, ".rds")))

# Life Years Lost
LifeYearLost <- PDsum * expLifeYears[idxStartTrunc:idxEndTrunc,]
LYL <- colSums(LifeYearLost)
cumsumLYL <- cumsum(LYL)
df_LYL <- data.frame(years = years,
                     LYL = LYL,
                     cumsumLYL = cumsumLYL)
saveRDS(df_LYL, file.path(outDir, paste0("df_LYL", scenario_name, ".rds")))

# Life Years Lost by Age
colnames(LifeYearLost) <- years
LifeYearLost <- LifeYearLost %>% as.data.frame()
suppressWarnings(
  LifeYearLost <- LifeYearLost %>%
    mutate(age = startAgeTrunc:endAgeTrunc,
           age_group = cut(age, breaks = c(-Inf, 18, 30, 40, 50, 60, 70, Inf),
                           right = FALSE, 
                           labels = c("below18", "18-29", "30-39", "40-49", "50-59", "60-69", "above70"))) %>%
    select(age, age_group, everything()) %>%
    gather("year", "LYL", -c(1,2)) %>%
    mutate(year = as.numeric(year))
)
saveRDS(LifeYearLost, file.path(outDir, paste0("df_AgeLYL", scenario_name, ".rds")))