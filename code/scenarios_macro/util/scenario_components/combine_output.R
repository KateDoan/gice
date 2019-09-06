#
# Prevalence
df_propSQ <- readRDS(file.path(datDirNoEcig,"df_propSQ.rds"))%>%mutate("D"=0, "E"=0, scenario="SQ")
df_propMLA21 <- readRDS(file.path(datDirNoEcig,"df_propMLA21.rds"))%>%mutate("D"=0, "E"=0, scenario="MLA21")
df_propMLA25 <- readRDS(file.path(datDirNoEcig,"df_propMLA25.rds"))%>%mutate("D"=0, "E"=0, scenario="MLA25")
df_propSFG <- readRDS(file.path(datDirNoEcig,"df_propSFG.rds"))%>%mutate("D"=0, "E"=0, scenario="SFG")
df_propTax2 <- readRDS(file.path(datDirNoEcig,"df_propTax2.rds"))%>%mutate("D"=0, "E"=0, scenario="TAX2")
df_propTax5 <- readRDS(file.path(datDirNoEcig,"df_propTax5.rds"))%>%mutate("D"=0, "E"=0, scenario="TAX5")
df_propMLATax5 <- readRDS(file.path(datDirNoEcig,"df_propMLATax5.rds"))%>%mutate("D"=0, "E"=0, scenario="MLATax5")
df_propELF <- readRDS(file.path(datDirEcig,"df_propELF.rds"))%>%mutate(scenario="ELF")
df_propE25 <- readRDS(file.path(datDirEcig,"df_propE25.rds"))%>%mutate(scenario="E25")
df_propE21 <- readRDS(file.path(datDirEcig,"df_propE21.rds"))%>%mutate(scenario="E21")
df_propEP <- readRDS(file.path(datDirEcig,"df_propEP.rds"))%>%mutate(scenario="EP")
df_propMLAEP <- readRDS(file.path(datDirEcig,"df_propMLAEP.rds"))%>%mutate(scenario="MLAEP")
df_propMLATax5EP <- readRDS(file.path(datDirEcig,"df_propMLATax5EP.rds"))%>%mutate(scenario="MLATax5EP")
df_propTax5EP <- readRDS(file.path(datDirEcig,"df_propTax5EP.rds"))%>%mutate(scenario="Tax5EP")
df_propSFGELF <- readRDS(file.path(datDirEcig,"df_propSFGELF.rds"))%>%mutate(scenario="SFGELF")
df_prop <-
  bind_rows(df_propSQ, df_propMLA21, df_propMLA25, df_propSFG, df_propELF, df_propE25, df_propE21,
            df_propEP, df_propTax2, df_propTax5, df_propMLAEP, df_propMLATax5, df_propMLATax5EP, df_propTax5EP,
            df_propSFGELF) %>%
  mutate(NandQ = N + Q,
         CandD = C + D,
         CDE = C + D + E) %>%
  rename(year = years)
saveRDS(df_prop, file.path(outDirCombined, paste0("df_prop_", paste0(small, big), ".rds")))
#
# Life Years Lost
df_LYLSQ <- readRDS(file.path(datDirNoEcig,"df_LYLSQ.rds"))%>%mutate(scenario="SQ")
df_LYLMLA21 <- readRDS(file.path(datDirNoEcig,"df_LYLMLA21.rds"))%>%mutate(scenario="MLA21")
df_LYLMLA25 <- readRDS(file.path(datDirNoEcig,"df_LYLMLA25.rds"))%>%mutate(scenario="MLA25")
df_LYLSFG <- readRDS(file.path(datDirNoEcig,"df_LYLSFG.rds"))%>%mutate(scenario="SFG")
df_LYLTax2 <- readRDS(file.path(datDirNoEcig,"df_LYLTax2.rds"))%>%mutate(scenario="TAX2")
df_LYLTax5 <- readRDS(file.path(datDirNoEcig,"df_LYLTax5.rds"))%>%mutate(scenario="TAX5")
df_LYLMLATax5 <- readRDS(file.path(datDirNoEcig,"df_LYLMLATax5.rds"))%>%mutate(scenario="MLATax5")
df_LYLELF <- readRDS(file.path(datDirEcig,"df_LYLELF.rds"))%>%mutate(scenario="ELF")
df_LYLE25 <- readRDS(file.path(datDirEcig,"df_LYLE25.rds"))%>%mutate(scenario="E25")
df_LYLE21 <- readRDS(file.path(datDirEcig,"df_LYLE21.rds"))%>%mutate(scenario="E21")
df_LYLEP <- readRDS(file.path(datDirEcig,"df_LYLEP.rds"))%>%mutate(scenario="EP")
df_LYLMLAEP <- readRDS(file.path(datDirEcig,"df_LYLMLAEP.rds"))%>%mutate(scenario="MLAEP")
df_LYLMLATax5EP <- readRDS(file.path(datDirEcig,"df_LYLMLATax5EP.rds"))%>%mutate(scenario="MLATax5EP")
df_LYLTax5EP <- readRDS(file.path(datDirEcig,"df_LYLTax5EP.rds"))%>%mutate(scenario="Tax5EP")
df_LYLSFGELF <- readRDS(file.path(datDirEcig,"df_LYLSFGELF.rds"))%>%mutate(scenario="SFGELF")
df_LYL <- 
  bind_rows(df_LYLSQ, df_LYLMLA21, df_LYLMLA25, df_LYLSFG, df_LYLELF, df_LYLE25, df_LYLE21,
            df_LYLEP, df_LYLTax2, df_LYLTax5, df_LYLMLAEP, df_LYLMLATax5, df_LYLMLATax5EP, df_LYLTax5EP,
            df_LYLSFGELF) %>%
  rename(year = years)
saveRDS(df_LYL, file.path(outDirCombined, paste0("df_LYL_", small, big, ".rds")))