## Tax coefficients
taxCoefs <- readRDS(file.path(taxDir,"sintaxMeanCoef.rds"))
fCQ1 <- taxCoefs['fCQ1']
fCQ2 <- taxCoefs['fCQ2']
fQC1 <- taxCoefs['fQC1']
fQC2 <- taxCoefs['fQC2']
fNC <- taxCoefs['fNC']
####################################################################################################
# Changes of rate due to tax
fNCvec <- c(fNC^(1:21%/%2), rep(fNC^10, length(22:numYear)))
fNCmat <- matrix(rep(fNCvec, numAge), byrow=T, nrow=numAge)
fCQ1vec <- c(fCQ1^(1:21%/%2), rep(fCQ1^10, length(22:numYear)))
fCQ1mat <- matrix(rep(fCQ1vec, 14), byrow=T, nrow=14)
fCQ2vec <- c(fCQ2^(1:21%/%2), rep(fCQ2^10, length(22:numYear)))
fCQ2mat <- matrix(rep(fCQ2vec, numAge-14), byrow=T, nrow=numAge-14) 
fCQmat <- rbind(fCQ1mat, fCQ2mat)
fQC1vec <- c(fQC1^(1:21%/%2), rep(fQC1^10, length(22:numYear)))
fQC1mat <- matrix(rep(fQC1vec, 14), byrow=T, nrow=14)
fQC2vec <- c(fQC2^(1:21%/%2), rep(fQC2^10, length(22:numYear)))
fQC2mat <- matrix(rep(fQC2vec, numAge-14), byrow=T, nrow=numAge-14) 
fQCmat <- rbind(fQC1mat, fQC2mat)

rateNCPost <- rateNC * fNCmat
rateCQPost <- rateCQ * fCQmat
rateQCPost <- rateQC * fQCmat

rateNC <- rateNCPost
rateCQ <- rateCQPost
rateQC <- rateQCPost
rateNN <- 1 - rateNC
rateCC <- 1 - rateCQ
rateQQ <- 1 - rateQC

