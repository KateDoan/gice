## Tax coefficients
taxCoefs <- readRDS(file.path(taxDir,"sintaxMeanCoefTax5.rds"))
fCQ1 <- taxCoefs['fCQ1']
fCQ2 <- taxCoefs['fCQ2']
fQC1 <- taxCoefs['fQC1']
fQC2 <- taxCoefs['fQC2']
fNC <- taxCoefs['fNC']

rateNCPost <- array(0, c(numAge,numYear))
rateCQPost <- array(0, c(numAge,numYear))
rateQCPost <- array(0, c(numAge,numYear))

rateNCPost[1:70,1] <- rateNC[1:70,1]
rateCQPost[1:14,1] <- rateCQ[1:14,1]
rateCQPost[15:70,1] <- rateCQ[15:70,1]
rateQCPost[1:14,1] <- rateQC[1:14,1]
rateQCPost[15:70,1] <- rateQC[15:70,1]

for(j in 2:51){
  rateNCPost[1:70,j] <- rateNC[1:70,j]*(fNC^(trunc((j-1)/5)+1))
  rateCQPost[1:14,j] <- rateCQ[1:14,j]*(fCQ1^(trunc((j-1)/5)+1))
  rateCQPost[15:70,j] <- rateCQ[15:70,j]*(fCQ2^(trunc((j-1)/5)+1))
  rateQCPost[1:14,j] <- rateQC[1:14,j]*(fQC1^(trunc((j-1)/5)+1))
  rateQCPost[15:70,j] <- rateQC[15:70,j]*(fQC2^(trunc((j-1)/5)+1))
}

rateNC <- rateNCPost
rateCQ <- rateCQPost
rateQC <- rateQCPost
rateNN <- 1 - rateNC
rateCC <- 1 - rateCQ
rateQQ <- 1 - rateQC
