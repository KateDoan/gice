# Never Smoker
numNArray <- array(0, c(numAge, numYear))
numNArray[1, ] <- numStartAges
numNArray[, 1] <- datNumN2017

# Current Smoker
numCArray <- array(0, c(numAge, numYear))
numCArray[, 1] <- datNumC2017

# Ex Smoker
numQArray <- array(0, c(numAge, numYear))
numQArray[, 1] <- datNumQ2017

# Dual User
numDArray <- array(0, c(numAge, numYear))

# Ecig User
numEArray <- array(0, c(numAge, numYear))

for(t in seq(startYear+1, endYear)) {
  yr = getYearIndex(t, startYear)
  for(age in seq(startAge+1, endAge)) {
    ag = getAgeIndex(age, startAge)
    numNArray[ag, yr] =
      numNArray[ag-1, yr-1] * (1-deathRateN[ag-1, yr-1]) * rateNN[ag-1, yr-1] +
      numEArray[ag-1, yr-1] * (1-deathRateE[ag-1, yr-1]) * rateEN[ag-1, yr-1] 
    
    numCArray[ag, yr] =  
      numNArray[ag-1, yr-1] * (1-deathRateN[ag-1, yr-1]) * rateNC[ag-1, yr-1] + 
      numCArray[ag-1, yr-1] * (1-deathRateC[ag-1, yr-1]) * rateCC[ag-1, yr-1] +
      numQArray[ag-1, yr-1] * (1-deathRateQ[ag-1, yr-1]) * rateQC[ag-1, yr-1] +
      numDArray[ag-1, yr-1] * (1-deathRateD[ag-1, yr-1]) * rateDC[ag-1, yr-1] +
      numEArray[ag-1, yr-1] * (1-deathRateE[ag-1, yr-1]) * rateEC[ag-1, yr-1] 
    
    numQArray[ag, yr] =
      numCArray[ag-1, yr-1] * (1-deathRateC[ag-1, yr-1]) * rateCQ[ag-1, yr-1] +
      numQArray[ag-1, yr-1] * (1-deathRateQ[ag-1, yr-1]) * rateQQ[ag-1, yr-1] +
      numDArray[ag-1, yr-1] * (1-deathRateD[ag-1, yr-1]) * rateDQ[ag-1, yr-1] 
    
    numDArray[ag, yr] =
      numNArray[ag-1, yr-1] * (1-deathRateN[ag-1, yr-1]) * rateND[ag-1, yr-1] +
      numCArray[ag-1, yr-1] * (1-deathRateC[ag-1, yr-1]) * rateCD[ag-1, yr-1] +
      numQArray[ag-1, yr-1] * (1-deathRateQ[ag-1, yr-1]) * rateQD[ag-1, yr-1] +
      numDArray[ag-1, yr-1] * (1-deathRateD[ag-1, yr-1]) * rateDD[ag-1, yr-1] +
      numEArray[ag-1, yr-1] * (1-deathRateE[ag-1, yr-1]) * rateED[ag-1, yr-1] 
    
    numEArray[ag, yr] =  
      numNArray[ag-1, yr-1] * (1-deathRateN[ag-1, yr-1]) * rateNE[ag-1, yr-1] + 
      numCArray[ag-1, yr-1] * (1-deathRateC[ag-1, yr-1]) * rateCE[ag-1, yr-1] +
      numQArray[ag-1, yr-1] * (1-deathRateQ[ag-1, yr-1]) * rateQE[ag-1, yr-1] +
      numDArray[ag-1, yr-1] * (1-deathRateD[ag-1, yr-1]) * rateDE[ag-1, yr-1] +
      numEArray[ag-1, yr-1] * (1-deathRateE[ag-1, yr-1]) * rateEE[ag-1, yr-1]
  }
}