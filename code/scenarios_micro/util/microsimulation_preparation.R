##############################################################################################
## inDir and outDir
datDir <- file.path("data", "singapore")
fpopDir <- file.path("code", "scenarios_micro", "util", "util_output")
smallMatDir <- file.path("code", "transitions", "sin_jags", "singapore_mcmc_free9", "output")
##############################################################################################
startAge <- 11
endAge <- 80
numAge <- endAge - startAge + 1
##############################################################################################
micro_prep <- function(){
    ## Read the data in
    singpop2017 <- read.csv(file.path(datDir, "singpop_2017.csv"))
    singpop2017
  ##############################################################################################
  n2017 <- round(singpop2017$never_smoker_2017)[1:numAge]
  c2017 <- round(singpop2017$curr_smoker_2017)[1:numAge]
  q2017 <- round(singpop2017$ex_smoker_2017)[1:numAge]
  ##############################################################################################
  curr.ages <- c(rep(startAge:endAge, n2017), rep(startAge:endAge, c2017), rep(startAge:endAge, q2017))
  curr.states <- c(rep("N", sum(n2017)), rep("C", sum(c2017)), rep("Q", sum(q2017))) 
  ##############################################################################################
  totalPop <- (sum(c2017)+sum(n2017)+sum(q2017))
  ##############################################################################################
  # Add the new individuals to create the open cohort
  fpop <- readRDS(file.path(fpopDir, "fpop.rds"))
  startAgePop <- (n2017[1] + c2017[1] + q2017[1])
  # startAgesVec <- startAgePop * c(1, fpop)
  ##############################################################################################
  extraPop <- round(startAgePop * fpop[length(fpop):1])
  eL <- length(extraPop)
  eAge <- (startAge-eL):(startAge-1)
  ePop.age <- rep(eAge, extraPop)
  ePop.state <- rep("N", length(ePop.age))
  sin.ages <- c(ePop.age, curr.ages)
  sin.states <- c(ePop.state, curr.states)
  ##############################################################################################
  deathRateN <- (read.csv(file.path(datDir, "death_rate_never_smoker.csv")))[,2][1:numAge]
  ##############################################################################################
  smallMatRates <- readRDS(file.path(smallMatDir, "sintrans_meanrates.rds"))
  NCprimer <- c(smallMatRates[69:136],rep(smallMatRates[136],2))
  CQprimer <- c(smallMatRates[1:68],rep(smallMatRates[68],2))
  QCprimer <- c(smallMatRates[137:204], rep(smallMatRates[204],2))
  
  list(sin.ages=sin.ages, sin.states=sin.states, 
       deathRateN=deathRateN,
       NCprimer=NCprimer, CQprimer=CQprimer, QCprimer=QCprimer)
}
