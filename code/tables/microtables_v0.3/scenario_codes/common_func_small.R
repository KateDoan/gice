##################################### Functions ###########################################
##################################### Helper functions ####################################
getNiVec <- function(v.base, v.index){
  v.ni <- v.base[v.index]
  v.ni[is.na(v.ni)] <- 0
  v.ni
}
##################################### Main functions ####################################
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
#### Probability function
# The Probs function that updates the transition probabilities of every cycle is shown below.
getInitRR <- function(M_it, v.index){
  v.index[v.index<=0] <- length(v.NXbase) + 10
  v.RRC.ni <- getNiVec(v.RRCbase, v.index)
  v.RRQ.ni <- getNiVec(v.RRQbase, v.index)
  v.RR <- rep(1, n.i)
  v.RR[M_it=="N"] <- 1
  v.RR[M_it=="C"] <- v.RRC.ni[M_it=="C"]
  v.RR[M_it=="Q"] <- v.RRQ.ni[M_it=="Q"]
  v.RR
}

getRR <- function(v.RRold, M_it, v.index){
  v.index[v.index<=0] <- length(v.NXbase) + 10
  v.bT.ni <- getNiVec(v.bTbase, v.index)
  v.RRC.ni <- getNiVec(v.RRCbase, v.index)
  v.RRQ.ni <- getNiVec(v.RRQbase, v.index)
  v.RR <- getInitRR(M_it, v.index)
  v.RR[M_it=="N"] <- 1
  v.RR[M_it=="C"] <- v.RRC.ni[M_it=="C"]
  v.RR[M_it=="Q"] <- 1 + (v.RRold[M_it=="Q"] - 1) * v.bT.ni[M_it=="Q"]
  v.RR
}

### Costs function
# The Costs function estimates the costs at every cycle.

getInitU <- function(M_it, v.index){
  v.index[v.index<=0] <- length(v.NXbase) + 10
  u.C.ni <- getNiVec(u.Cbase, v.index)
  u.Q.ni <- getNiVec(u.Qbase, v.index)
  u <- rep(0, n.i)
  u[M_it=="N"] <- 1
  u[M_it=="C"] <- u.C.ni[M_it=="C"]
  u[M_it=="Q"] <- u.Q.ni[M_it=="Q"]
  u[v.index<1 | v.index>70] <- 0
  u
}

getU <- function(u.old, M_it, v.index){
  v.index[v.index<=0] <- length(v.NXbase) + 10
  u.bT.ni <- getNiVec(u.bTbase, v.index)
  u.C.ni <- getNiVec(u.Cbase, v.index)
  u.Q.ni <- getNiVec(u.Qbase, v.index)
  u <- getInitU(M_it, v.index)
  u[M_it=="N"] <- 1
  u[M_it=="C"] <- u.C.ni[M_it=="C"]
  u[M_it=="Q"] <- 1 - (1 - u.old[M_it=="Q"]) * u.bT.ni[M_it=="Q"]
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