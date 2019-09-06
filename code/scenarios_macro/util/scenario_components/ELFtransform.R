fELF = 1/2
rateNE <- rateNE * fInitEcig
row18 <- getAgeIndex(18, startAge)
rowAgeELF <- getAgeIndex(ELFAge, startAge)
rateNE[row18:rowAgeELF, ] <- rateNE[row18:rowAgeELF, ] * fELF
rateCD[row18:rowAgeELF,] <- rateCD[row18:rowAgeELF, ] * fELF
rateCE[row18:rowAgeELF,] <- rateCE[row18:rowAgeELF, ] * fELF
rateQE[row18:rowAgeELF,] <- rateQE[row18:rowAgeELF, ] * fELF
rateND[row18:rowAgeELF,] <- rateND[row18:rowAgeELF, ] * fELF
rateQD[row18:rowAgeELF,] <- rateQD[row18:rowAgeELF, ] * fELF

rateNC <- rateNC * (1-rateNE-rateND)
rateCQ <- rateCQ * (1-rateCD-rateCE)
rateQC <- rateQC * (1-rateQE-rateQD)

rateNN <- 1 - rateNC - rateNE - rateND
rateCC <- 1 - rateCQ - rateCD - rateCE
rateQQ <- 1 - rateQC - rateQE - rateQD
rateDD <- 1 - rateDC - rateDQ - rateDE
rateEE <- 1 - rateEC - rateEN - rateED
