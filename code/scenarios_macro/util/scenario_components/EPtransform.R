pCWantQuit <- rateCQ/RT_CQ_R

rateNE <- rateNE * fInitEcigPres * fInitEcig
rateCE <- RT_CE_E * pCWantQuit 
rateQE <- rateQE * fPres
rateND <- rateND * fInitEcigPres
rateCD <- RT_CD_E * pCWantQuit 
rateQD <- rateQD * fPres

rateNC <- rateNC * (1-rateNE-rateND)
rateCQ <- RT_CQ_E * pCWantQuit 
rateQC <- rateQC * (1-rateQE-rateQD)

rateNN <- 1 - rateNC - rateNE - rateND
rateCC <- 1 - rateCQ - rateCD - rateCE
rateQQ <- 1 - rateQC - rateQE - rateQD
rateDD <- 1 - rateDC - rateDQ - rateDE
rateEE <- 1 - rateEC - rateEN - rateED