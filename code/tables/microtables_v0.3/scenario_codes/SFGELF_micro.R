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
    
    m.p <- Probs(m.M[, t], v.index, v.RR, t)           # calculate the transition probabilities at cycle t 
    
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


# The Probs function that updates the transition probabilities of every cycle is shown below.
Probs <- function(M_it, v.index, v.RR, t) { 
  # M_it:   health state occupied by individual i at cycle t (character variable)
  # dur:    the duration of being sick (sick/sicker)
  v.index[v.index<=0] <- length(v.NXbase) + 10
  m.p.it <- matrix(NA, n.s, n.i)     # create vector of state transition probabilities
  rownames(m.p.it) <- v.n            # assign names to the vector
  
  # SFG
  if(t >= tStartSFG){
    durSFG <- t-tStartSFG
    v.NC.primer <- v.NC.primer*(1-d.c)^durSFG
    v.NC.primer[ageInitCigIdx:(ageInitCigIdx+durSFG)] <- pmin(mean(v.NC.primer[1:5]), v.NC.primer[ageInitCigIdx:(ageInitCigIdx+durSFG)])
    v.CQ.primer[ageInitCigIdx:(ageInitCigIdx+durSFG)] <- pmax(mean(v.CQ.primer[1:5]), v.CQ.primer[ageInitCigIdx:(ageInitCigIdx+durSFG)])
    v.QC.primer[ageInitCigIdx:(ageInitCigIdx+durSFG)] <- pmin(mean(v.QC.primer[1:5]), v.QC.primer[ageInitCigIdx:(ageInitCigIdx+durSFG)])
  }
  
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
  
  if(t >= tStartSFG){
    durSFG <- t-tStartSFG
    v.NE.ni  <- v.NE.ni * ((1 + a.ecig)^durSFG)
  }
  
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

