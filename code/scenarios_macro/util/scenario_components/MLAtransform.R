rowMLAge <- getAgeIndex(MLAge-1, startAge)
row18 <- getAgeIndex(17, startAge)
rateNC[row18:rowMLAge-1,] <- rateNC[row18:rowMLAge-1,]*fMLA
rateNN <- 1 - rateNC
rateCC <- 1 - rateCQ
rateQQ <- 1 - rateQC