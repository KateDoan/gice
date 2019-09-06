#############################################################################################################################
rm(list=ls())
#############################################################################################################################
test <- TRUE # set test to FALSE to run the real simulation
scenarioName <- "ELF25"
ELFAge <- 25
big <- "US"
outDir <- file.path("scenarios_micro","micro_v0.3_sensitivity", "riskEcig", "lessRisk", "outData")
#############################################################################################################################
source("scenarios_micro/util/microsimulation_preparation.R")
source("scenarios_micro/util/read_big_mat_transition.R")
init <- micro_prep()
eTrans <- readEcigTrans(big)
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

# Scale factor for risk of e-cigarettes
fRiskEcig <- 0.5

# Cost and utility inputs 
u.N    <- 1                   # utility when not smoking
u.bTbase <- c(rep(0.98, 19), rep(0.96, 10), rep(0.97, 5), rep(0.96, 5), rep(0.97, 15), rep(0.98, 16))
u.Cbase <- c(rep(0.91,19), rep(c(0.88,0.86,0.83,0.81,0.78,0.76,0.74), each=5), rep(0.71, 16))  # utility when smoking1
u.Qbase <- 1 - (1-u.Cbase) * 0.05
if(big == "JP"){
  u.Ebase <- 1 - (1-u.Cbase) * 0.05  * fRiskEcig
} else {
  u.Ebase <- 1 - (1-u.Cbase) * 0.10 * fRiskEcig
}  
u.Dbase <- sqrt(u.Cbase * u.Ebase)

v.NXbase <- init$deathRateN
v.bTbase <- c(rep(0.92, 5), rep(0.93, 5), rep(0.94, 14), rep(0.95, 21), rep(0.96, 25))
v.RRCbase <- c(rep(2.8, 49), rep(2.5, 10), rep(2.0, 11))
v.RRQbase <- 1 + (v.RRCbase - 1) * 0.05 
if(big == "JP"){
  v.RREbase <- 1 + (v.RRCbase - 1) * 0.05  * fRiskEcig
} else {
  v.RREbase <- 1 + (v.RRCbase - 1) * 0.10 * fRiskEcig
} 
v.RRDbase <- sqrt(v.RRCbase * v.RREbase)

# Transition rates transformation
fELF = 1/2
fInitEcig = 1

v.NC.primer <- init$NCprimer
v.CQ.primer <- init$CQprimer
v.QC.primer <- init$QCprimer

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

##################################### Functions ###########################################
##################################### Helper functions ####################################
getNiVec <- function(v.base, v.index){
  v.ni <- v.base[v.index]
  v.ni[is.na(v.ni)] <- 0
  v.ni
}
##################################### Main functions ######################################
# THE NEW samplev() FUNCTION
# efficient implementation of the rMultinom() function of the Hmisc package #### 

samplev <- function (probs, m) {
  d <- dim(probs)
  n <- d[1]
  k <- d[2]
  lev <- dimnames(probs)[[2]]
  if (!length(lev)) 
    lev <- 1:k
  ran <- matrix(lev[1], ncol = m, nrow = n)
  U <- t(probs)
  for(i in 2:k) {
    U[i, ] <- U[i, ] + U[i - 1, ]
  }
  if (any((U[k, ] - 1) > 1e-05))
    stop("error in multinom: probabilities do not sum to 1")
  
  for (j in 1:m) {
    un <- rep(runif(n), rep(k, n))
    ran[, j] <- lev[1 + colSums(un > U)]
  }
  ran
}
# The MicroSim function for the simple microsimulation of the 'Sick-Sicker' model keeps track of what happens to each individual during each cycle. 

MicroSim <- function(v.M_1, v.age, n.i, n.t, v.n, X = NULL, d.c, d.e, TR.out = TRUE, TS.out = FALSE, Trt = FALSE, seed = 1) {
  # Arguments:  
  # v.M_1:   vector of initial states for individuals
  # n.i:     number of individuals
  # n.t:     total number of cycles to run the model
  # v.n:     vector of health state names
  # X:       vector or matrix of individual characteristics
  # d.c:     discount rate for costs
  # d.e:     discount rate for health outcome (QALYs)
  # TR.out:  should the output include a Microsimulation trace? (default is TRUE)
  # TS.out:  should the output include a matrix of transitions between states? (default is TRUE)
  # Trt:     are the n.i individuals receiving treatment? (scalar with a Boolean value, default is FALSE)
  # seed:    starting seed number for random number generator (default is 1)
  # Makes use of:
  # Probs:   function for the estimation of transition probabilities
  # Costs:   function for the estimation of cost state values
  # Effs:    function for the estimation of state specific health outcomes (QALYs)
  v.index <- v.age - startAge + 1
  v.dwe <- 1 / (1 + d.e) ^ (0:n.t)
  
  # Create the matrix capturing the state name/costs/health outcomes for all individuals at each time point 
  m.M <- m.E <- matrix(nrow = n.i, ncol = n.t + 1, 
                dimnames = list(paste("ind", 1:n.i, sep = " "), 
                                paste("cycle", 0:n.t, sep = " ")))  
  if(TR.out == TRUE) {
    TR = matrix(NA, n.s, n.t)
  }
  
  m.M[, 1] <- v.M_1               # indicate the initial health state  
  v.RR <- getInitRR(v.M_1, v.index)
  u <- getInitU(v.M_1, v.index)
  m.E[, 1] <- Effs (u, cl=1)
  
  set.seed(seed)                  # set the seed for every individual for the random number generator
  
  for (t in 1:n.t) { # t <- 3
    # print(v.index)
    if (TR.out == TRUE) {
      TR[,t] <- table(factor((m.M[,t])[v.age>=12 & v.age<=80], levels=v.n, ordered=TRUE))
    }
    
    if(t>1){
      v.RR <- getRR(v.RR, m.M[,t], v.index)
    }
    
    # print(t)
    # print(v.RR)
    
    m.p <- Probs(m.M[, t], v.index, v.RR)           # calculate the transition probabilities at cycle t 
    
    m.M[, t + 1] <- samplev(prob = m.p, m = 1)  # sample the next health state and store that state in matrix m.M 
   
    
    cat('\r', paste(round(t/n.t * 100), "% done", sep = " "))       # display the progress of the simulation
    
    v.age <- v.age + 1
    v.index <- v.index + 1
    v.NXbase <<- v.NXbase * (1-d.x)
    u <- getU(u, m.M[,t+1], v.index)
    m.E[,t + 1] <- Effs(u, cl=1)
  } # close the loop for the time points 
  
  
  if (TS.out == TRUE) {  # create a matrix of transitions across states
    TS <- paste(m.M, cbind(m.M[, -1], NA), sep = "->") # transitions from one state to the other
    TS <- matrix(TS, nrow = n.i)
    rownames(TS) <- paste("Ind",   1:n.i, sep = " ")   # name the rows 
    colnames(TS) <- paste("Cycle", 0:n.t, sep = " ")   # name the columns 
  } else {
    TS <- NULL
  }
  
  if(TR.out==TRUE){
    TR <- prop.table(t(TR), margin = 1)
  } else {
    TR <- NULL
  }
  
  te <- m.E %*% v.dwe       # total (discounted) QALYs per individual 
  
  te_hat <- mean(te)        # average (discounted) QALYs
  
  colSumME <- colSums(m.E)

  results <- list(m.M = m.M, TS = TS, TR = TR, m.E = m.E, te = te, te_hat = te_hat, colSumME = colSumME) # store the results from the simulation in a list  
  return(results)  # return the results
}  # end of the MicroSim function  


#### Probability function
getInitRR <- function(M_it, v.index){
  v.index[v.index<=0] <- length(v.NXbase) + 10
  v.RRC.ni <- getNiVec(v.RRCbase, v.index)
  v.RRQ.ni <- getNiVec(v.RRQbase, v.index)
  v.RRD.ni <- getNiVec(v.RRDbase, v.index)
  v.RRE.ni <- getNiVec(v.RREbase, v.index)
  v.RR <- rep(1, n.i)
  v.RR[M_it=="N"] <- 1
  v.RR[M_it=="C"] <- v.RRC.ni[M_it=="C"]
  v.RR[M_it=="Q"] <- v.RRQ.ni[M_it=="Q"]
  v.RR[M_it=="D"] <- v.RRD.ni[M_it=="D"]
  v.RR[M_it=="E"] <- v.RRE.ni[M_it=="E"]
  v.RR
}

getRR <- function(v.RRold, M_it, v.index){
  v.index[v.index<=0] <- length(v.NXbase) + 10
  v.bT.ni <- getNiVec(v.bTbase, v.index)
  v.RRC.ni <- getNiVec(v.RRCbase, v.index)
  v.RRQ.ni <- getNiVec(v.RRQbase, v.index)
  v.RRD.ni <- getNiVec(v.RRDbase, v.index)
  v.RRE.ni <- getNiVec(v.RREbase, v.index)
  v.RR <- getInitRR(M_it, v.index)
  v.worseToN <- M_it=="N" & v.RRold>1
  v.RR[v.worseToN] <- 1 + (v.RRold[v.worseToN] - 1) * v.bT.ni[v.worseToN]
  v.RR[M_it=="C"] <- v.RRC.ni[M_it=="C"]
  v.RR[M_it=="Q"] <- 1 + (v.RRold[M_it=="Q"] - 1) * v.bT.ni[M_it=="Q"]
  v.worseToD <- M_it=="D" & v.RRold>v.RRD.ni
  v.RR[v.worseToD] <- v.RRD.ni[v.worseToD] + (v.RRold[v.worseToD] - v.RRD.ni[v.worseToD]) * v.bT.ni[v.worseToD]
  v.worseToE <- M_it=="E" & v.RRold>v.RRE.ni
  v.RR[v.worseToE] <- v.RRE.ni[v.worseToE] + (v.RRold[v.worseToE] - v.RRE.ni[v.worseToE]) * v.bT.ni[v.worseToE]
  v.RR
}

# The Probs function that updates the transition probabilities of every cycle is shown below.
Probs <- function(M_it, v.index, v.RR) { 
  # M_it:   health state occupied by individual i at cycle t (character variable)
  # dur:    the duration of being sick (sick/sicker)
  v.index[v.index<=0] <- length(v.NXbase) + 10
  m.p.it <- matrix(NA, n.s, n.i)     # create vector of state transition probabilities
  rownames(m.p.it) <- v.n            # assign names to the vector
  
  # Update base transition rates
  v.NX.ni <- getNiVec(v.NXbase, v.index)
  v.toX.ni <- v.RR * v.NX.ni
  
  v.NC.primer.ni <- getNiVec(v.NC.primer, v.index)
  v.NC.ni <- v.NC.primer.ni 
  
  v.CQ.primer.ni <- getNiVec(v.CQ.primer, v.index)
  v.CQ.ni <- v.CQ.primer.ni 
  
  v.QC.primer.ni <- getNiVec(v.QC.primer, v.index)
  v.QC.ni <- v.QC.primer.ni 
  
  # Ecig transitions
  v.NE.ni <- getNiVec(v.NE.primer, v.index); v.ND.ni <- getNiVec(v.ND.primer, v.index)
  v.NE.ni[v.index>15] <- 0;  v.ND.ni[v.index>15] <- 0;
  v.CD.ni <- getNiVec(v.CD.primer, v.index); v.CE.ni <- getNiVec(v.CE.primer, v.index)  
  v.QD.ni <- getNiVec(v.QD.primer, v.index); v.QE.ni <- getNiVec(v.QE.primer, v.index)  
  v.DC.ni <- getNiVec(v.DC.primer, v.index); v.DQ.ni <- getNiVec(v.DQ.primer, v.index)
  v.DE.ni <- getNiVec(v.DE.primer, v.index); v.EC.ni <- getNiVec(v.EC.primer, v.index)  
  v.ED.ni <- getNiVec(v.ED.primer, v.index); v.EN.ni <- getNiVec(v.EN.primer, v.index)
  
  v.NC.ni <- v.NC.ni * (1-v.NE.ni-v.ND.ni)
  v.CQ.ni <- v.CQ.ni * (1-v.CD.ni-v.CE.ni)
  v.QC.ni <- v.QC.ni * (1-v.QE.ni-v.QD.ni)
  
  v.NC.ni <- v.NC.ni * (1-v.toX.ni); v.NE.ni <- v.NE.ni * (1-v.toX.ni); v.ND.ni <- v.ND.ni * (1-v.toX.ni)
  v.CQ.ni <- v.CQ.ni * (1-v.toX.ni); v.CD.ni <- v.CD.ni * (1-v.toX.ni); v.CE.ni <- v.CE.ni * (1-v.toX.ni)
  v.QC.ni <- v.QC.ni * (1-v.toX.ni); v.QE.ni <- v.QE.ni * (1-v.toX.ni); v.QD.ni <- v.QD.ni * (1-v.toX.ni)
  v.DC.ni <- v.DC.ni * (1-v.toX.ni); v.DQ.ni <- v.DQ.ni * (1-v.toX.ni); v.DE.ni <- v.DE.ni * (1-v.toX.ni)
  v.EC.ni <- v.EC.ni * (1-v.toX.ni); v.EN.ni <- v.EN.ni * (1-v.toX.ni); v.ED.ni <- v.ED.ni * (1-v.toX.ni)
  
  v.NN.ni <- 1 - v.NC.ni - v.NE.ni - v.ND.ni - v.toX.ni
  v.CC.ni <- 1 - v.CQ.ni - v.CD.ni - v.CE.ni - v.toX.ni 
  v.QQ.ni <- 1 - v.QC.ni - v.QE.ni - v.QD.ni - v.toX.ni
  v.DD.ni <- 1 - v.DC.ni - v.DQ.ni - v.DE.ni - v.toX.ni
  v.EE.ni <- 1 - v.EC.ni - v.EN.ni - v.ED.ni - v.toX.ni
  
  youngN <- M_it=="N"&v.index<=15
  m.p.it[,youngN] <- rbind(v.NN.ni[youngN], v.NC.ni[youngN], 0, v.ND.ni[youngN], v.NE.ni[youngN], v.toX.ni[youngN])   # transition probabilities when never smoke
  youngE <- M_it=="E"&v.index<=15
  m.p.it[,youngE] <- rbind(v.EN.ni[youngE], v.EC.ni[youngE], 0, v.ED.ni[youngE], v.EE.ni[youngE], v.toX.ni[youngE])
  
  oldN <- M_it=="N"&v.index>15
  m.p.it[,oldN] <- rbind(v.NN.ni[oldN], v.NC.ni[oldN], 0, 0, 0, v.toX.ni[oldN])   # transition probabilities when never smoke
  oldE <- M_it=="E"&v.index>15
  m.p.it[,oldE] <- rbind(0, v.EC.ni[oldE], v.EN.ni[oldE], v.ED.ni[oldE], v.EE.ni[oldE], v.toX.ni[oldE])
  
  m.p.it[,M_it == "C"] <- rbind(0, v.CC.ni[M_it=="C"], v.CQ.ni[M_it=="C"], v.CD.ni[M_it=="C"], v.CE.ni[M_it=="C"], v.toX.ni[M_it=="C"])   # transition probabilities when current smoke
  m.p.it[,M_it == "Q"] <- rbind(0, v.QC.ni[M_it=="Q"], v.QQ.ni[M_it=="Q"], v.QD.ni[M_it=="Q"], v.QE.ni[M_it=="Q"], v.toX.ni[M_it=="Q"])   # transition probabilities when quit smoke
  m.p.it[,M_it == "D"] <- rbind(0, v.DC.ni[M_it=="D"], v.DQ.ni[M_it=="D"], v.DD.ni[M_it=="D"], v.DE.ni[M_it=="D"], v.toX.ni[M_it=="D"])
  m.p.it[,M_it == "X"]  <- c(0, 0, 0, 0, 0, 1) # transition probabilities when dead   
  # cat("\n")
  # print(m.p.it)
  # cat("\n")
  ifelse(colSums(m.p.it) == 1, return(t(m.p.it)), print("Probabilities do not sum to 1")) # return the transition probabilities or produce an error
}


### Costs function
# The Costs function estimates the costs at every cycle.
getInitU <- function(M_it, v.index){
  v.index[v.index<=0] <- length(v.NXbase) + 10
  u.C.ni <- getNiVec(u.Cbase, v.index)
  u.Q.ni <- getNiVec(u.Qbase, v.index)
  u.D.ni <- getNiVec(u.Dbase, v.index)
  u.E.ni <- getNiVec(u.Ebase, v.index)
  u <- rep(0, n.i)
  u[M_it=="N"] <- 1
  u[M_it=="C"] <- u.C.ni[M_it=="C"]
  u[M_it=="Q"] <- u.Q.ni[M_it=="Q"]
  u[M_it=="D"] <- u.D.ni[M_it=="D"]
  u[M_it=="E"] <- u.E.ni[M_it=="E"]
  u[v.index<1 | v.index>70] <- 0
  u
}

getU <- function(u.old, M_it, v.index){
  v.index[v.index<=0] <- length(v.NXbase) + 10
  u.bT.ni <- getNiVec(u.bTbase, v.index)
  u.C.ni <- getNiVec(u.Cbase, v.index)
  u.Q.ni <- getNiVec(u.Qbase, v.index)
  u.D.ni <- getNiVec(u.Dbase, v.index)
  u.E.ni <- getNiVec(u.Ebase, v.index)
  u <- getInitU(M_it, v.index)
  u.worseToN <- M_it=="N" & u.old!=0 & u.old<1
  u[u.worseToN] <- 1 - (1 - u.old[u.worseToN]) * u.bT.ni[u.worseToN]
  u[M_it=="C"] <- u.C.ni[M_it=="C"]
  u[M_it=="Q"] <- 1 - (1 - u.old[M_it=="Q"]) * u.bT.ni[M_it=="Q"]
  u.worseToD <- M_it=="D" & u.old<u.D.ni
  u[u.worseToD] <- u.D.ni[u.worseToD] - (u.D.ni[u.worseToD] - u.old[u.worseToD]) * u.bT.ni[u.worseToD]
  u.worseToE <- M_it=="E" & u.old<u.E.ni
  u[u.worseToE] <- u.E.ni[u.worseToE] - (u.E.ni[u.worseToE] - u.old[u.worseToE]) * u.bT.ni[u.worseToE]
  u[M_it == "X"]  <- 0        # update the utility if dead
  u[v.index<1 | v.index>70] <- 0
  u
}

Effs <- function (u, cl = 1) {
  # M_it: health state occupied by individual i at cycle t (character variable)
  # v.index
  # dur:  the duration of being sick/sicker
  # cl:   cycle length (default is 1)
  QALYs <-  u * cl            # calculate the QALYs during cycle t
  return(QALYs)                  # return the QALYs
}
##################################### Run the simulation ##################################
# START SIMULATION
p = Sys.time()
sim_no_trt  <- MicroSim(v.M_1, v.age, n.i, n.t, v.n, X = v.x, d.c, d.e, Trt = FALSE, seed = 200) 
comp.time = Sys.time() - p
comp.time

# PRINT DATA
sim_no_trt$TR
# sim_no_trt$m.M

# SAVE DATA
saveRDS(sim_no_trt$TR, file.path(outDir, paste0(scenarioName,"_","SG",big,"_TR.rds")))
saveRDS(sim_no_trt$colSumME, file.path(outDir, paste0(scenarioName,"_","SG",big,"_colSumME.rds")))

