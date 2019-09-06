#############################################################################################################################
rm(list=ls())
#############################################################################################################################
library(tidyverse)
#############################################################################################################################
test <- TRUE # set test to FALSE to run the real simulation
nSim <- 20
scenarioName <- "ELF"
ELFAge <- 18
big <- "US"
rootDir <- file.path("code", "tables", "microtables_v0.3")
scenarioCodeDir <- file.path(rootDir, "scenario_codes")
outDir <- file.path(rootDir, "output_tables", "gateway")
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
source(file.path(scenarioCodeDir, "ELF_gateway_micro.R"), local=TRUE)
##################################### Run the simulation ##################################
# START SIMULATION
p = Sys.time()

# Transition rates transformation
fELF = 1/2
fInitEcig = 1

lres <- vector("list", nSim)

rand1 <- c(180)
rand2 <- c(400)

for(i in 1:nSim){
  set.seed(2019+rand1[1]*i)
  
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

  lres[[i]] <- MicroSim(v.M_1, v.age, n.i, n.t, v.n, X = v.x, d.c, d.e, Trt = FALSE, seed = 200+rand2[1]*i)
  
  if(i%%20==0){
    NEonlys <- sapply(lres[1:i], "[[", "pNEonly")*100
    NECs <- sapply(lres[1:i], "[[", "pNEC")*100
    NEDs <- sapply(lres[1:i], "[[", "pNED")*100
    NCs <- sapply(lres[1:i], "[[", "pNC")*100
    NDs <- sapply(lres[1:i], "[[", "pND")*100
    res <- list(NEonlys=NEonlys, NECs=NECs, NEDs=NEDs, NCs=NCs, NDs=NDs)
    sum_res <- list(mean=lapply(res,mean),
                    sd=lapply(res,sd),
                    CIlow=lapply(res,quantile,0.025),
                    CIhigh=lapply(res,quantile,0.975))
    print(sum_res)
    
    # SAVE DATA
    saveRDS(sum_res, file.path(outDir, paste0(i, "_", "mcountGateWayYouth","_SG", big, ".rds")))
  }

  ## Keep track of the progress
  cat("\n", i, "th iteration done\n")
}

comp.time = Sys.time() - p
comp.time
##################################################################################################