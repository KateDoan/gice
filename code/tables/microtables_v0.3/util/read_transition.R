##
smallMatDir <- "code/transitions/sin_jags/singapore_mcmc_free9/output"
bigMatDirUS <- "code/transitions/path1_pool/transition_output"
bigMatDirUK <- "code/transitions/uk_jags/ukv/output"
bigMatDirJP <- "code/transitions/japan_jags/output"
####################################################################################################
# ELF
####################################################################################################
# Create the rate sample
nSample <- 1

readSGTrans <- function(){
  nSmallMCMC <- 3e4
  smallMatRates <- readRDS(file.path(smallMatDir, "mod_csim.rds"))
  sampleSmallIdxs <- sample(1:nSmallMCMC, nSample)
  sampleSmallMatRates <- smallMatRates[sampleSmallIdxs,]
  
  NCprimer <- c(sampleSmallMatRates[69:136],rep(sampleSmallMatRates[136],2))
  CQprimer <- c(sampleSmallMatRates[1:68],rep(sampleSmallMatRates[68],2))
  QCprimer <- c(sampleSmallMatRates[137:204], rep(sampleSmallMatRates[204],2))
  
  SGtrans <- list(NCprimer=NCprimer, CQprimer=CQprimer, QCprimer=QCprimer)
  SGtrans
}

readEcigTrans <- function(big){
  if(big == "US"){
    # Create US sample
    nBigMCMC <- 1e5
    bigMatRates <- readRDS(file.path(bigMatDirUS, "USmod_csim.rds"))
    sampleBigIdxs <- sample(1:nBigMCMC, nSample)
    sampleBigMatRates <- bigMatRates[sampleBigIdxs,]
    
    curr_rates <- sampleBigMatRates
    NEprimer <- curr_rates[seq(5, by=25, length.out = 8)]
    CDprimer <- curr_rates[seq(9, by=25, length.out = 8)]
    CEprimer <- curr_rates[seq(10, by=25, length.out = 8)]
    QEprimer <- curr_rates[seq(15, by=25, length.out = 8)]
    DCprimer <- curr_rates[seq(17, by=25, length.out = 8)]
    DQprimer <- curr_rates[seq(18, by=25, length.out = 8)]
    DEprimer <- curr_rates[seq(20, by=25, length.out = 8)]
    ECprimer <- curr_rates[seq(22, by=25, length.out = 8)]
    EDprimer <- curr_rates[seq(24, by=25, length.out = 8)]
    # EQprimer <- curr_rates[seq(23, by=25, length.out = 8)]
    ENprimer <- curr_rates[seq(23, by=25, length.out = 8)]
    NDprimer <- curr_rates[seq(4, by=25, length.out = 8)]
    QDprimer <- curr_rates[seq(14, by=25, length.out = 8)]
    
    USTimesVec <- c(7, 7, 10, 10, 10, 10, 10, 6)
    rateNE <- rep(NEprimer, USTimesVec)
    rateCD <- rep(CDprimer, USTimesVec)
    rateCE <- rep(CEprimer, USTimesVec)
    rateQE <- rep(QEprimer, USTimesVec)
    rateDC <- rep(DCprimer, USTimesVec)
    rateDQ <- rep(DQprimer, USTimesVec)
    rateDE <- rep(DEprimer, USTimesVec)
    rateEC <- rep(ECprimer, USTimesVec)
    rateED <- rep(EDprimer, USTimesVec)
    # rateEQ <- rep(EQprimer, USTimesVec)
    rateEN <- rep(ENprimer, USTimesVec)
    rateND <- rep(NDprimer, USTimesVec)
    rateQD <- rep(QDprimer, USTimesVec)
  } else if(big == "UK"){
    # Create the UK sample
    nBigMCMC <- 3e5
    bigMatRates <- readRDS(file.path(bigMatDirUK, "uktrans_modcsim.rds"))
    sampleBigIdxs <- sample(1:nBigMCMC, nSample)
    sampleBigMatRates <- bigMatRates[sampleBigIdxs,]
    
    m <- matrix(sampleBigMatRates, byrow=T, nrow = 50)
    CQprimer=c(0,m[1:10,2]); CDprimer=c(0,m[1:10,3]); CEprimer=c(0,m[1:10,4]);
    DCprimer=c(0,m[11:20,1]); DQprimer=c(0,m[11:20,2]); DEprimer=c(0,m[11:20,4]);
    ENprimer=c(0,m[21:30,1]); ECprimer=c(0,m[21:30,2]); EDprimer=c(0,m[21:30,3]);
    NCprimer=c(0,m[31:40,2]); NDprimer=c(0,m[31:40,3]); NEprimer=c(0,m[31:40,4]);
    QCprimer=c(0,m[41:50,1]); QDprimer=c(0,m[41:50,3]); QEprimer=c(0,m[41:50,4])
    # 
    UKTimesVec <- c(4, 2, 3, rep(5,4), rep(10,3), 11)
    rateNE <- rep(NEprimer, UKTimesVec)
    rateND <- rep(NDprimer, UKTimesVec)
    rateCD <- rep(CDprimer, UKTimesVec)
    rateCE <- rep(CEprimer, UKTimesVec)
    rateQD <- rep(QDprimer, UKTimesVec)
    rateQE <- rep(QEprimer, UKTimesVec)
    rateDC <- rep(DCprimer, UKTimesVec)
    rateDQ <- rep(DQprimer, UKTimesVec)
    rateDE <- rep(DEprimer, UKTimesVec)
    rateEN <- rep(ENprimer, UKTimesVec)
    rateEC <- rep(ECprimer, UKTimesVec)
    rateED <- rep(EDprimer, UKTimesVec)
  } else if(big == "JP"){
    # Create the JP sample
    nBigMCMC <- 8e5
    bigMatRates <- readRDS(file.path(bigMatDirJP, "JP_modcsim.rds"))
    sampleBigIdxs <- sample(1:nBigMCMC, nSample)
    sampleBigMatRates <- bigMatRates[sampleBigIdxs,]    
    
    m <- sampleBigMatRates
    JPsampl1529 <- 881 + 1462
    JPsampl_total <- 8240
    NEyouth <- m['A[1,5]'] * JPsampl_total/JPsampl1529
    NEprimer <- c(0, NEyouth, rep(0,5))
    CDprimer <- c(0, rep(m['A[2,4]'], 5), 0)
    CEprimer <- c(0, rep(m['A[2,5]'], 5), 0)
    QEprimer <- c(0, rep(m['A[3,5]'], 5), 0)
    DCprimer <- c(0, rep(m['A[4,2]'], 5), 0)
    DQprimer <- c(0, rep(m['A[4,3]'], 5), 0)
    DEprimer <- c(0, rep(m['A[4,5]'], 5), 0)
    ECprimer <- c(0, rep(m['A[5,2]'], 5), 0)
    EDprimer <- c(0, rep(m['A[5,4]'], 5), 0)
    ENprimer <- c(0, rep(m['A[5,1]'], 5), 0)
    NDprimer <- c(0, rep(m['A[1,4]'], 5), 0)
    QDprimer <- c(0, rep(m['A[3,4]'], 5), 0)
    # 
    JPTimesVec <- c(4,15,10,10,10,10,11)
    rateNE <- rep(NEprimer, JPTimesVec)
    rateND <- rep(NDprimer, JPTimesVec)
    rateCD <- rep(CDprimer, JPTimesVec)
    rateCE <- rep(CEprimer, JPTimesVec)
    rateQD <- rep(QDprimer, JPTimesVec)
    rateQE <- rep(QEprimer, JPTimesVec)
    rateDC <- rep(DCprimer, JPTimesVec)
    rateDQ <- rep(DQprimer, JPTimesVec)
    rateDE <- rep(DEprimer, JPTimesVec)
    rateEC <- rep(ECprimer, JPTimesVec)
    rateED <- rep(EDprimer, JPTimesVec)
    rateEN <- rep(ENprimer, JPTimesVec)
  }
  
  ecigTrans <- list(NE=rateNE, ND=rateND, CD=rateCD, CE=rateCE, QD=rateQD,
                    QE=rateQE, DC=rateDC, DQ=rateDQ, DE=rateDE, EC=rateEC,
                    ED=rateED, EN=rateEN)
  
  ecigTrans
}
