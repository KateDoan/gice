startSFGYear <- getYearIndex(SFGBirth + ageInitCig, startYear)
rowAgeInitCig <- getAgeIndex(ageInitCig, startAge)
for(i in rowAgeInitCig:numAge) {
  rateNC[i, startSFGYear:numYear] <- min(mean(rateNC[1:5,1]),rateNC[i,1])
  rateCQ[i, startSFGYear:numYear] <- max(mean(rateCQ[1:5,1]),rateCQ[i,1])
  rateQC[i, startSFGYear:numYear] <- min(mean(rateQC[1:5,1]),rateQC[i,1])
  startSFGYear = startSFGYear + 1
  if(startSFGYear > numYear) 
    break
}

rateNN <- 1 - rateNC
rateCC <- 1 - rateCQ
rateQQ <- 1 - rateQC