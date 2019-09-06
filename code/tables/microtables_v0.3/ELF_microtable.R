#############################################################################################################################
rm(list=ls())
#############################################################################################################################
library(tidyverse)
#############################################################################################################################
test <- TRUE # set test to FALSE to run the real simulation
scenarioName <- "ELF"
ELFAge <- 18
big <- "US"
rootDir <- file.path("code", "tables", "microtables_v0.3")
scenarioCodeDir <- file.path(rootDir, "scenario_codes")
outDir <- file.path(rootDir, "output_tables")
#############################################################################################################################
source(file.path(rootDir, "util", "microsimulation_preparation.R"))
source(file.path(rootDir, "util", "read_transition.R"))
source(file.path(rootDir, "scenario_codes", "common_func.R"))
init <- micro_prep()
startAge <- 11
endAge <- 80
startYear <- 2017
endYear <- 2067
numAge <- endAge - startAge + 1
numYear <- endYear - startYear + 1

if(test){
    n.i <- 10
    v.M_1 <- rep(c("C", "N", "E", "Q", "Q"), n.i/5)
    v.age <- rep(c(19, 4, 35, 180, 20), n.i/5)
    n.t   <- 10        
} else {
    n.i   <- length(init$sin.ages)     # number of simulated individuals
    v.M_1 <- init$sin.states           # beginning states
    v.age <- init$sin.ages             # initialize age
    n.t   <- 50                        # time horizon
}

v.n   <- c("N", "C", "Q", "D", "E", "X")  # the model states: Never Smoker(N), Smoker(C), Quitter(Q), Dual(D), E-cig Only(E)
n.s   <- length(v.n)  # the number of states
d.e <- 0.03           # equal discounting of costs and QALYs by 3%
d.x <- 0.03           # mortality rate decreases by 3% annually
# v.Trt <- c("No Treatment", "Treatment") # store the strategy names

# Cost and utility inputs 
u.N    <- 1                   # utility when not smoking
u.bTbase <- c(rep(0.98, 19), rep(0.96, 10), rep(0.97, 5), rep(0.96, 5), rep(0.97, 15), rep(0.98, 16))
u.Cbase <- c(rep(0.91,19), rep(c(0.88,0.86,0.83,0.81,0.78,0.76,0.74), each=5), rep(0.71, 16))  # utility when smoking
u.Qbase <- 1 - (1-u.Cbase) * 0.05
if(big == "JP"){
  u.Ebase <- 1 - (1-u.Cbase) * 0.05  
} else {
  u.Ebase <- 1 - (1-u.Cbase) * 0.10
}  
u.Dbase <- sqrt(u.Cbase * u.Ebase)

v.NXbase <- init$deathRateN
v.bTbase <- c(rep(0.92, 5), rep(0.93, 5), rep(0.94, 14), rep(0.95, 21), rep(0.96, 25))
v.RRCbase <- c(rep(2.8, 49), rep(2.5, 10), rep(2.0, 11))
v.RRQbase <- 1 + (v.RRCbase - 1) * 0.05 
if(big == "JP"){
  v.RREbase <- 1 + (v.RRCbase - 1) * 0.05  
} else {
  v.RREbase <- 1 + (v.RRCbase - 1) * 0.10
} 
v.RRDbase <- sqrt(v.RRCbase * v.RREbase)

##################################### Source the scenario code ##################################
source(file.path(scenarioCodeDir, "ELF_micro.R"), local=TRUE)
##################################### Run the simulation ##################################
# START SIMULATION
nSim <- 10

p = Sys.time()

# Transition rates transformation
fELF = 1/2
fInitEcig = 1

set.seed(2019)

last_df_TR <- data.frame(year = integer(),
                         group = factor(levels=c("N","C","Q","D","E")),
                         prop = numeric())
last_df_QALY <- data.frame(year = startYear:(startYear+n.t)) 

for(i in 1:nSim){
  sgTrans <- readSGTrans()
  eTrans <- readEcigTrans(big)
  v.NC.primer <- sgTrans$NCprimer
  v.CQ.primer <- sgTrans$CQprimer
  v.QC.primer <- sgTrans$QCprimer
  
  v.NE.primer=eTrans$NE; v.ND.primer=eTrans$ND; v.CD.primer=eTrans$CD; v.CE.primer=eTrans$CE; 
  v.QD.primer=eTrans$QD; v.QE.primer=eTrans$QE; v.DC.primer=eTrans$DC; v.DQ.primer=eTrans$DQ; 
  v.DE.primer=eTrans$DE; v.EC.primer=eTrans$EC; v.ED.primer=eTrans$ED; v.EN.primer=eTrans$EN
  
  v.NE.primer <- v.NE.primer * fInitEcig
  row18 <- 18 - startAge + 1
  rowAgeELF <- ELFAge - startAge + 1
  v.NE.primer[row18:rowAgeELF] <- v.NE.primer[row18:rowAgeELF] * fELF
  v.CD.primer[row18:rowAgeELF] <- v.CD.primer[row18:rowAgeELF] * fELF
  v.CE.primer[row18:rowAgeELF] <- v.CE.primer[row18:rowAgeELF] * fELF
  v.QE.primer[row18:rowAgeELF] <- v.QE.primer[row18:rowAgeELF] * fELF
  v.ND.primer[row18:rowAgeELF] <- v.ND.primer[row18:rowAgeELF] * fELF
  v.QD.primer[row18:rowAgeELF] <- v.QD.primer[row18:rowAgeELF] * fELF
  sim_no_trt  <- MicroSim(v.M_1, v.age, n.i, n.t, v.n, X = v.x, d.c, d.e, Trt = FALSE, seed = 200) 
  # print(sim_no_trt$colSumME)
  
  ## Process TR
  TR <- sim_no_trt$TR[,-6]/(1-sim_no_trt$TR[,6])
  TR <- as_data_frame(TR*100)
  colnames(TR) <- c("N", "C", "Q", "D", "E")
  TR <- TR %>%
    mutate(year = (startYear+1):(startYear+n.t)) %>%
    gather("group", "prop", -"year") %>%
    mutate(group=factor(group, levels=c("N","C","Q","D","E")))
  
  last_df_TR <- last_df_TR %>%
    bind_rows(TR)
  
  ## Process QALY
  QALY <- sim_no_trt$colSumME %>% as.data.frame()
  colnames(QALY) <- paste0("QALY", i)
  last_df_QALY <- last_df_QALY %>%
    bind_cols(QALY)
  
  ## Keep track of the progress
  cat("\n", i, "th iteration done\n")
}

comp.time = Sys.time() - p
comp.time

summaryTR <- last_df_TR %>%
  group_by(year, group) %>%
  summarise(mean = mean(prop), 
            sd = sd(prop),
            CIlow = quantile(prop, 0.025),
            CIhigh = quantile(prop, 0.975)) 
View(summaryTR)
View(last_df_QALY)

# # SAVE DATA
# saveRDS(summaryTR, file.path(outDir, paste0(scenarioName,"_","SG",big,"_summaryTR.rds")))
# saveRDS(last_df_QALY, file.path(outDir, paste0(scenarioName,"_","SG",big,"_dfQALY.rds")))