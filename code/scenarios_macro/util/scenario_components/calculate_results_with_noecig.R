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


for(t in seq(startYear+1, endYear)) {
  yr = getYearIndex(t, startYear)
  for(age in seq(startAge+1, endAge)) {
    ag = getAgeIndex(age, startAge)
    numNArray[ag, yr] =
      numNArray[ag-1, yr-1] * (1-deathRateN[ag-1, yr-1]) * rateNN[ag-1, yr-1] 
    
    numCArray[ag, yr] =  
      numNArray[ag-1, yr-1] * (1-deathRateN[ag-1, yr-1]) * rateNC[ag-1, yr-1] + 
      numCArray[ag-1, yr-1] * (1-deathRateC[ag-1, yr-1]) * rateCC[ag-1, yr-1] +
      numQArray[ag-1, yr-1] * (1-deathRateQ[ag-1, yr-1]) * rateQC[ag-1, yr-1]
    
    numQArray[ag, yr] =
      numQArray[ag-1, yr-1] * (1-deathRateQ[ag-1, yr-1]) * rateQQ[ag-1, yr-1] +
      numCArray[ag-1, yr-1] * (1-deathRateC[ag-1, yr-1]) * rateCQ[ag-1, yr-1]
  }
}
