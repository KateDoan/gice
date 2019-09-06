library(tidyverse)
#########################################################################################
## In directories
# datDir <- "data/singapore"
bigMatDirJP <- file.path("code", "transitions", "japan_jags", "output")

## Out directories
outDir <- file.path("code", "tables", "transition_tables", "tables_output")
csvOutDir <- file.path(outDir, "csv_output")
#########################################################################################
# Parameter
trans <- c("NN","CN","QN","DN","EN",
           "NC","CC","QC","DC","EC",
           "NQ","CQ","QQ","DQ","EQ",
           "ND","CD","QD","DD","ED",
           "NE","CE","QE","DE","EE")
trans_levels <- c("NN","NC","NQ","ND","NE",
                  "CN","CC","CQ","CD","CE",
                  "QN","QC","QQ","QD","QE",
                  "DN","DC","DQ","DD","DE",
                  "EN","EC","EQ","ED","EE")
#########################################################################################
# Helper functions
fm <- function(x){
    format(round(x,1), nsmall = 1)
} 
#########################################################################################
JPrates <- readRDS(file.path(bigMatDirJP, "JP_modcsim.rds"))
mean_trans <- colMeans(JPrates)
sd_trans <- apply(JPrates, 2, sd)
lower <- apply(JPrates, 2, quantile, probs=0.025)
upper <- apply(JPrates, 2, quantile, probs=0.975)
#########################################################################################
# For young
JPsampl1529 <- 881 + 1462
JPsampl_total <- 8240

mean_trans_young<-mean_trans;sd_trans_young<-sd_trans;lower_young<-lower;upper_young<-upper
# NE
mean_trans_young[21]<-mean_trans[21]*JPsampl_total/JPsampl1529
sd_trans_young[21] <- sd_trans[21]*JPsampl_total/JPsampl1529
lower_young[21] <- lower[21]*JPsampl_total/JPsampl1529
upper_young[21] <- upper[21]*JPsampl_total/JPsampl1529
# NN
mean_trans_young[1]<- 1-sum(mean_trans_young[c(6,11,16,21)])
sd_trans_young[1] <- sqrt(sum(sd_trans_young[c(6,11,16,21)]^2))
lower_young[1] <- lower_young[1] - mean_trans_young[21] + mean_trans[21]
upper_young[1] <- upper_young[1] - mean_trans_young[21] + mean_trans[21]

tm_young_raw <- data.frame(
    transition = factor(trans, levels=trans_levels),
    mean = unname(mean_trans_young)*100,
    sd = unname(sd_trans_young)*100,
    lower = unname(lower_young)*100,
    upper = unname(upper_young)*100) 

tm_young <- tm_young_raw %>% 
  mutate(posterior = paste0(fm(mean)," (",fm(lower),", ",fm(upper),")")
) %>% select(transition, posterior
) %>% arrange(transition)

saveRDS(tm_young, file.path(outDir,"JPTm2Young.rds"))
write.csv(tm_young, file.path(csvOutDir, "JPTm2Young.csv"), row.names = FALSE)
#########################################################################################
# For old
mean_trans_old<-mean_trans;sd_trans_old<-sd_trans;lower_old<-lower;upper_old<-upper
# NE, ND <- 0
mean_trans_old[c(16,21)]<- 0
sd_trans_old[c(16,21)] <- 0
lower_old[c(16,21)] <- 0
upper_old[c(16,21)] <- 0
# NN
mean_trans_old[1]<- 1-sum(mean_trans_old[c(6,11)])
sd_trans_old[1] <- sqrt(sum(sd_trans_old[c(6,11)]^2))
lower_old[1] <- lower_old[1] + sum(mean_trans[c(16,21)])  
upper_old[1] <- upper_old[1] + sum(mean_trans[c(16,21)]) 

  tm_old_raw <- data.frame(
    transition = factor(trans, levels=trans_levels),
    mean = unname(mean_trans_old)*100,
    sd = unname(sd_trans_old)*100,
    lower = unname(lower_old)*100,
    upper = unname(upper_old)*100) 
  
  tm_old <- tm_old_raw %>% 
    mutate(posterior = paste0(fm(mean)," (",fm(lower),", ",fm(upper),")")
  ) %>% select(transition, posterior
  ) %>% arrange(transition)

saveRDS(tm_old, file.path(outDir,"JPTm2Old.rds"))
write.csv(tm_old, file.path(csvOutDir, "JPTm2Old.csv"), row.names = FALSE)
##################################################################################
# Create table for comparison
tm_young_compare <- tm_young_raw %>%
  mutate(age_group="15-24")%>%
  separate(age_group, into=c("age_start", "age_end"), sep="-") %>%
  mutate(age_start=parse_number(age_start), age_end=parse_number(age_end)) %>%
  mutate(age_end = age_end+1) %>%
  select(age_start, age_end, everything())

tm_old_compare <- tm_old_raw %>%
  mutate(age_group="25-69")%>%
  separate(age_group, into=c("age_start", "age_end"), sep="-") %>%
  mutate(age_start=parse_number(age_start), age_end=parse_number(age_end)) %>%
  mutate(age_end = age_end+1) %>%
  select(age_start, age_end, everything())

JPtm_compare <- bind_rows(tm_young_compare, tm_old_compare)
saveRDS(JPtm_compare, file.path(outDir, "JPtm_compare.rds"))