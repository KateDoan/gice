RREprimer <- 1 + (RRCprimer-1) * ExcessRisk_E
if(ExcessRisk_D == "geom_mean"){
  RRDprimer <- sqrt(RRCprimer * RREprimer)
} else {
  RRDprimer <- 1 + (RRCprimer-1) * ExcessRisk_D
}
RRE_mat <- createMat(RREprimer)
RRD_mat <- createMat(RRDprimer)
deathRateE <- deathRateN*RRE_mat
deathRateD <- deathRateN*RRD_mat