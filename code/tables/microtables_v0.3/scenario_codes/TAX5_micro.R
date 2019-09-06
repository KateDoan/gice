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
    m.E[,t + 1] <- Effs( u, cl=1)
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


Probs <- function(M_it, v.index, v.RR, t) { 
  # M_it:   health state occupied by individual i at cycle t (character variable)
  # dur:    the duration of being sick (sick/sicker)
  v.index[v.index<=0] <- length(v.NXbase) + 10
  m.p.it <- matrix(NA, n.s, n.i)     # create vector of state transition probabilities
  rownames(m.p.it) <- v.n            # assign names to the vector
  
  if(t<50){
    v.NC.primer <- v.NC.primer * ( fNC^ (((t-1)%/%5) + 1) )
    v.CQ.primer <- v.CQ.primer * ( c(rep(fCQ1, 14), rep(fCQ2, numAge-14)) ^ (((t-1)%/%5) + 1) )
    v.QC.primer <- v.QC.primer * ( c(rep(fQC1, 14), rep(fQC2, numAge-14)) ^ (((t-1)%/%5) + 1) )
  } else {
    v.NC.primer <- v.NC.primer * (fNC^10)
    v.CQ.primer <- v.CQ.primer * ( c(rep(fCQ1, 14), rep(fCQ2, numAge-14)) ^ 10 )
    v.QC.primer <- v.QC.primer * ( c(rep(fQC1, 14), rep(fQC2, numAge-14)) ^ 10 )
  }
  
  # Update base transition rates
  v.NX.ni <- getNiVec(v.NXbase, v.index)
  
  v.toX.ni <- v.RR * v.NX.ni
  
  v.NC.primer.ni <- getNiVec(v.NC.primer, v.index)
  v.NC.ni <- v.NC.primer.ni * (1-v.toX.ni)
  
  v.CQ.primer.ni <- getNiVec(v.CQ.primer, v.index)
  v.CQ.ni <- v.CQ.primer.ni * (1-v.toX.ni)
  
  v.QC.primer.ni <- getNiVec(v.QC.primer, v.index)
  v.QC.ni <- v.QC.primer.ni * (1-v.toX.ni)
  
  m.p.it[,M_it == "N"] <- rbind(1-v.NC.ni[M_it=="N"]-v.toX.ni[M_it=="N"], v.NC.ni[M_it=="N"], 0, v.toX.ni[M_it=="N"])   # transition probabilities when never smoke
  m.p.it[,M_it == "C"] <- rbind(0, 1-v.CQ.ni[M_it=="C"]-v.toX.ni[M_it=="C"], v.CQ.ni[M_it=="C"], v.toX.ni[M_it=="C"])   # transition probabilities when current smoke
  m.p.it[,M_it == "Q"] <- rbind(0, v.QC.ni[M_it=="Q"], 1-v.QC.ni[M_it=="Q"]-v.toX.ni[M_it=="Q"], v.toX.ni[M_it=="Q"])   # transition probabilities when quit smoke
  m.p.it[,M_it == "X"]  <- c(0, 0, 0, 1) # transition probabilities when dead   
  # cat("\n")
  # print(m.p.it)
  # cat("\n")
  ifelse(colSums(m.p.it) == 1, return(t(m.p.it)), print("Probabilities do not sum to 1")) # return the transition probabilities or produce an error
}
