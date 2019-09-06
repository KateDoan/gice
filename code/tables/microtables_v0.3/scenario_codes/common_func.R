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