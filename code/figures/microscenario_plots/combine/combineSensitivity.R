library(dplyr)
####################################################################################################
# In and Out Dirs
senDatDir <- file.path("code", "figures", "microscenario_plots", "combine", "output", "precombined") 
baseDatDir <- file.path("code", "tables", "combined_microtables", "output", "for_graph")
outDir <-  file.path("code", "figures", "microscenario_plots", "combine", "output", "sensitivity_df")
####################################################################################################
df_prop_SGUS <- readRDS(file.path(baseDatDir, "df_prop_SGUS.rds"))
df_prop_SGUK <- readRDS(file.path(baseDatDir, "df_prop_SGUK.rds"))
df_prop_moreNE2x <- bind_rows(readRDS(file.path(senDatDir, "df_prop_moreNE2x.rds")) %>% 
                                filter(scenario %in% c("ELF", "E25", "SFGELF")),
                              df_prop_SGUK %>% filter(! scenario %in% c("ELF", "E25", "SFGELF")))

df_prop_moreNE3x <- bind_rows(readRDS(file.path(senDatDir, "df_prop_moreNE3x.rds")) %>% 
                                filter(scenario %in% c("ELF", "E25", "SFGELF")),
                              df_prop_SGUK %>% filter(! scenario %in% c("ELF", "E25", "SFGELF")))

df_prop_moreCECD2x <- bind_rows(readRDS(file.path(senDatDir, "df_prop_moreCECD2x.rds")) %>% 
                                filter(scenario %in% c("ELF", "E25", "SFGELF")),
                              df_prop_SGUS %>% filter(! scenario %in% c("ELF", "E25", "SFGELF")))

df_prop_moreCECD3x <- bind_rows(readRDS(file.path(senDatDir, "df_prop_moreCECD3x.rds")) %>% 
                                  filter(scenario %in% c("ELF", "E25", "SFGELF")),
                                df_prop_SGUS %>% filter(! scenario %in% c("ELF", "E25", "SFGELF")))

df_prop_lessERisk <- bind_rows(readRDS(file.path(senDatDir, "df_prop_lessERisk.rds")) %>% 
                                  filter(scenario %in% c("ELF", "E25", "SFGELF", "EP", "MLAEP", "Tax5EP", "MLATax5EP")),
                                df_prop_SGUS %>% filter(! scenario %in% c("ELF", "E25", "SFGELF", "EP", "MLAEP", "Tax5EP", "MLATax5EP")))

df_prop_moreERisk <- bind_rows(readRDS(file.path(senDatDir, "df_prop_moreERisk.rds")) %>% 
                                 filter(scenario %in% c("ELF", "E25", "SFGELF", "EP", "MLAEP", "Tax5EP", "MLATax5EP")),
                               df_prop_SGUS %>% filter(! scenario %in% c("ELF", "E25", "SFGELF", "EP", "MLAEP", "Tax5EP", "MLATax5EP")))

df_prop_scaling <- bind_rows(readRDS(file.path(senDatDir, "df_prop_scaling.rds")) %>% 
                                 filter(scenario %in% c("ELF", "E25", "SFGELF", "EP", "MLAEP", "Tax5EP", "MLATax5EP")),
                               df_prop_SGUS %>% filter(! scenario %in% c("ELF", "E25", "SFGELF", "EP", "MLAEP", "Tax5EP", "MLATax5EP")))
####################################################################################################
df_QALY_SGUS <- readRDS(file.path(baseDatDir, "df_QALY_SGUS.rds"))
df_QALY_SGUK <- readRDS(file.path(baseDatDir, "df_QALY_SGUK.rds"))
df_QALY_moreNE2x <- bind_rows(readRDS(file.path(senDatDir, "df_QALY_moreNE2x.rds")) %>% 
                                filter(scenario %in% c("ELF", "E25", "SFGELF")),
                              df_QALY_SGUK %>% filter(! scenario %in% c("ELF", "E25", "SFGELF")))

df_QALY_moreNE3x <- bind_rows(readRDS(file.path(senDatDir, "df_QALY_moreNE3x.rds")) %>% 
                                filter(scenario %in% c("ELF", "E25", "SFGELF")),
                              df_QALY_SGUK %>% filter(! scenario %in% c("ELF", "E25", "SFGELF")))

df_QALY_moreCECD2x <- bind_rows(readRDS(file.path(senDatDir, "df_QALY_moreCECD2x.rds")) %>% 
                                  filter(scenario %in% c("ELF", "E25", "SFGELF")),
                                df_QALY_SGUS %>% filter(! scenario %in% c("ELF", "E25", "SFGELF")))

df_QALY_moreCECD3x <- bind_rows(readRDS(file.path(senDatDir, "df_QALY_moreCECD3x.rds")) %>% 
                                  filter(scenario %in% c("ELF", "E25", "SFGELF")),
                                df_QALY_SGUS %>% filter(! scenario %in% c("ELF", "E25", "SFGELF")))

df_QALY_lessERisk <- bind_rows(readRDS(file.path(senDatDir, "df_QALY_lessERisk.rds")) %>% 
                                 filter(scenario %in% c("ELF", "E25", "SFGELF", "EP", "MLAEP", "Tax5EP", "MLATax5EP")),
                               df_QALY_SGUS %>% filter(! scenario %in% c("ELF", "E25", "SFGELF", "EP", "MLAEP", "Tax5EP", "MLATax5EP")))

df_QALY_moreERisk <- bind_rows(readRDS(file.path(senDatDir, "df_QALY_moreERisk.rds")) %>% 
                                 filter(scenario %in% c("ELF", "E25", "SFGELF", "EP", "MLAEP", "Tax5EP", "MLATax5EP")),
                               df_QALY_SGUS %>% filter(! scenario %in% c("ELF", "E25", "SFGELF", "EP", "MLAEP", "Tax5EP", "MLATax5EP")))

df_QALY_scaling <- bind_rows(readRDS(file.path(senDatDir, "df_QALY_scaling.rds")) %>% 
                               filter(scenario %in% c("ELF", "E25", "SFGELF", "EP", "MLAEP", "Tax5EP", "MLATax5EP")),
                             df_QALY_SGUS %>% filter(! scenario %in% c("ELF", "E25", "SFGELF", "EP", "MLAEP", "Tax5EP", "MLATax5EP")))
####################################################################################################
for(name1 in c("prop", "QALY")){
  for(name2 in c("moreNE2x","moreNE3x","moreCECD2x","moreCECD3x","lessERisk","moreERisk","scaling")){
    object_name <- paste0('df_', name1, '_', name2)
    fname <- paste0(object_name, ".rds")
    temp <- eval(parse(text=object_name))
    saveRDS(temp, file.path(outDir, fname))
  }
}