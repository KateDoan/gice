library(wpp2017)
library(tidyverse)
###########################################################################################
## In and Out directories
outDir <- file.path("code", "scenarios_micro", "util", "util_output")
###########################################################################################
## Load the data
data("popM")
data("popF")
data(popMprojMed)
data(popFprojMed)
## Examine the data
head(popM)
head(popF)
head(popMprojMed)
head(popFprojMed)
## Extract the relevant population
popM2015 <- popM %>% filter(name=="Singapore", age=="10-14") %>% select("2015")
popM2015 <- as.numeric(as.vector(popM2015))
popF2015 <- popF %>% filter(name=="Singapore", age=="10-14") %>% select("2015")
popF2015 <- as.numeric(as.vector(popF2015))
pop2015 <- popM2015 + popF2015


popM2020_2070 <- popMprojMed %>% filter(name=="Singapore", age=="10-14")
idx2020 <- which(colnames(popM2020_2070)=="2020")
idx2070 <- which(colnames(popM2020_2070)=="2070")
popM2020_2070 <- popM2020_2070[idx2020:idx2070]
popM2020_2070 <- as.numeric(as.vector(popM2020_2070))

popF2020_2070 <- popFprojMed %>% filter(name=="Singapore", age=="10-14")
idx2020 <- which(colnames(popF2020_2070)=="2020")
idx2070 <- which(colnames(popF2020_2070)=="2070")
popF2020_2070 <- popF2020_2070[idx2020:idx2070]
popF2020_2070 <- as.numeric(as.vector(popF2020_2070))

pop2020_2070 <- popM2020_2070 + popF2020_2070

## Calculation the population growth factor
fpop <- pop2020_2070/c(pop2015, pop2020_2070[-length(pop2020_2070)]) 
fpop2017_2067 <- rep(fpop, c(3,rep(5, 9),2))

## Save fpop to the outDir
saveRDS(fpop2017_2067, file.path(outDir, "fpop.rds"))
