RRCprimer <- c(2.8, 2.8, 2.8, 2.8, 2.8, 2.5, 2.0)
ExcessRisk_Q <- 0.05
RRQprimer <- 1 + (RRCprimer-1) * ExcessRisk_Q
RRC_mat <- createMat(RRCprimer)
RRQ_mat <- createMat(RRQprimer)
deathRateQ <- deathRateN*RRQ_mat
deathRateC <- deathRateN*RRC_mat