library(tidyverse)
#########################################################################################
## In directories
bigMatDirUK <- file.path("code", "transitions", "uk_jags", "ukv", "output")
## Out directories
outDir <- file.path("code", "tables", "transition_tables", "tables_output")
csvOutDir <- file.path(outDir, "csv_output")
#########################################################################################
# Parameters
trans <- c("NN","NC","ND","NE",
           "CC","CQ","CD","CE",
           "QC","QQ","QD","QE",
           "DC","DQ","DD","DE",
           "EN","EC","ED","EE")
#########################################################################################
# Helper functions
fm <- function(x){
    format(round(x,1), nsmall = 1)
} 
#########################################################################################
UKrates <- readRDS(file.path(bigMatDirUK, "uktrans_modcsim.rds"))
sd <- apply(UKrates, 2, sd)
sd <- unname(sd)
mean <- colMeans(UKrates)
mean <- unname(mean)
lower <- apply(UKrates, 2, quantile, probs=0.025)
lower <- unname(lower)
upper <- apply(UKrates, 2, quantile, probs=0.975)
upper <- unname(upper)

# Change ND, NE to 0 for older age groups
for(i in seq(133, by=4, length.out = 10)){ # For NN indices
  # NN <- NN + NE + ND
  mean[i] <- mean[i] + mean[i+2] + mean[i+3] 
  sd[i] <- sd[i] + sd[i+2] + sd[i+3]
  lower[i] <- lower[i] + lower[i+2] + lower[i+3]
  upper[i] <- upper[i] + upper[i+2] + upper[i+3]
  
  # NE = ND = 0
  mean[i+2]=mean[i+3]=sd[i+2]=sd[i+3]=lower[i+2]=lower[i+3]=upper[i+2]=upper[i+3]=0
}

#
transition <- c(rep(c("CC", "CQ", "CD", "CE"),10),
                rep(c("DC", "DQ", "DD", "DE"),10),
                rep(c("EN", "EC", "ED", "EE"),10),
                rep(c("NN", "NC", "ND", "NE"),10),
                rep(c("QC", "QQ", "QD", "QE"),10))
age_group <- c(rep(rep(c("15-16","17-19","20-24","25-29","30-34","35-39",
                   "40-49","50-59","60-69","70-79"),each=4),5))
UKtm_raw <- data.frame(age_group = age_group,
                   transition = factor(transition, levels=trans),
                   mean = mean*100,
                   sd = sd*100,
                   upper = upper*100,
                   lower = lower*100) 
UKtm <- UKtm_raw %>% 
        mutate(posterior = paste0(fm(mean)," (",fm(lower),", ",fm(upper),")")) %>% 
        select(age_group, transition, posterior) %>% 
        spread(age_group, posterior)
##########################################################################################
# Write the output to the Out directory
saveRDS(UKtm, file.path(outDir, "UKTm2.rds"))
write.csv(UKtm, file.path(csvOutDir, "UKTm2.csv"), row.names = FALSE)
##########################################################################################
# Prepare for the transition compare plot
UKtm_compare <- UKtm_raw %>%
  separate(age_group, into=c("age_start", "age_end"), sep="-") %>%
  mutate(age_start=parse_number(age_start), age_end=parse_number(age_end)) %>%
  mutate(age_end = age_end+1) %>%
  select(age_start, age_end, everything())
saveRDS(UKtm_compare, file.path(outDir, "UKtm_compare.rds"))