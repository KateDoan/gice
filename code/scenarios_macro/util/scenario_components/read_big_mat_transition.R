if(big == "US"){
  bigMatDir <- bigMatDirUS
  tm1217 <- readRDS(file.path(bigMatDir,"params_pos1217.rds"))
  tm1824 <- readRDS(file.path(bigMatDir,"params_pos1824.rds"))
  tm2534 <- readRDS(file.path(bigMatDir,"params_pos2534.rds"))
  tm3544 <- readRDS(file.path(bigMatDir,"params_pos3544.rds"))
  tm4554 <- readRDS(file.path(bigMatDir,"params_pos4554.rds"))
  tm5564 <- readRDS(file.path(bigMatDir,"params_pos5564.rds"))
  tm6574 <- readRDS(file.path(bigMatDir,"params_pos6574.rds"))
  tm75plus <- readRDS(file.path(bigMatDir,"params_pos75plus.rds"))
  
  for(s in c("NE","CD","CE","QE","DC","DQ","DE","EC","ED","EQ","ND","QD")){
    eval(parse(text=paste0(s,"primer <- c(tm1217$",s,",tm1824$",s,",tm2534$",s,",tm3544$",s,",tm4554$",s,",tm5564$",s,",tm6574$",s,",tm75plus$",s, ")")))
    eval(parse(text=paste0(s,"primer[is.na(",s,"primer)]<-0")))
  }
  ENprimer <- EQprimer
  USTimesVec <- c(7, 7, 10, 10, 10, 10, 10, 6)
  rateNE <- createMat(NEprimer, USTimesVec)
  rateCD <- createMat(CDprimer, USTimesVec)
  rateCE <- createMat(CEprimer, USTimesVec)
  rateQE <- createMat(QEprimer, USTimesVec)
  rateDC <- createMat(DCprimer, USTimesVec)
  rateDQ <- createMat(DQprimer, USTimesVec)
  rateDE <- createMat(DEprimer, USTimesVec)
  rateEC <- createMat(ECprimer, USTimesVec)
  rateED <- createMat(EDprimer, USTimesVec)
  rateEN <- createMat(ENprimer, USTimesVec)
  rateND <- createMat(NDprimer, USTimesVec)
  rateQD <- createMat(QDprimer, USTimesVec)
  
  # mean_rates <- readRDS(file.path(bigMatDir, "USmean_modcsim.rds"))
  # NEprimer <- mean_rates[seq(5, by=25, length.out = 8)]
  # CDprimer <- mean_rates[seq(9, by=25, length.out = 8)]
  # CEprimer <- mean_rates[seq(10, by=25, length.out = 8)]
  # QEprimer <- mean_rates[seq(15, by=25, length.out = 8)]
  # DCprimer <- mean_rates[seq(17, by=25, length.out = 8)]
  # DQprimer <- mean_rates[seq(18, by=25, length.out = 8)]
  # DEprimer <- mean_rates[seq(20, by=25, length.out = 8)]
  # ECprimer <- mean_rates[seq(22, by=25, length.out = 8)]
  # EDprimer <- mean_rates[seq(24, by=25, length.out = 8)]
  # EQprimer <- mean_rates[seq(23, by=25, length.out = 8)]
  # NDprimer <- mean_rates[seq(4, by=25, length.out = 8)]
  # QDprimer <- mean_rates[seq(14, by=25, length.out = 8)]
  # 
  # USTimesVec <- c(7, 7, 10, 10, 10, 10, 10, 6)
  # rateNE <- createMat(NEprimer, USTimesVec)
  # rateCD <- createMat(CDprimer, USTimesVec)
  # rateCE <- createMat(CEprimer, USTimesVec)
  # rateQE <- createMat(QEprimer, USTimesVec)
  # rateDC <- createMat(DCprimer, USTimesVec)
  # rateDQ <- createMat(DQprimer, USTimesVec)
  # rateDE <- createMat(DEprimer, USTimesVec)
  # rateEC <- createMat(ECprimer, USTimesVec)
  # rateED <- createMat(EDprimer, USTimesVec)
  # rateEQ <- createMat(EQprimer, USTimesVec)
  # rateND <- createMat(NDprimer, USTimesVec)
  # rateQD <- createMat(QDprimer, USTimesVec)
} else if(big == "JP"){
  bigMatDir <- bigMatDirJP
  mean_rates <- readRDS(file.path(bigMatDir, "mean_rates.rds"))
  JPsampl1529 <- 881 + 1462
  JPsampl_total <- 8240
  NEyouth <- mean_rates['A[1,5]'] * JPsampl_total/JPsampl1529
  (NEprimer <- c(0, NEyouth, rep(0,5)))
  (CDprimer <- c(0, rep(mean_rates['A[2,4]'], 5), 0))
  (CEprimer <- c(0, rep(mean_rates['A[2,5]'], 5), 0))
  (QEprimer <- c(0, rep(mean_rates['A[3,5]'], 5), 0))
  (DCprimer <- c(0, rep(mean_rates['A[4,2]'], 5), 0))
  (DQprimer <- c(0, rep(mean_rates['A[4,3]'], 5), 0))
  (DEprimer <- c(0, rep(mean_rates['A[4,5]'], 5), 0))
  (ECprimer <- c(0, rep(mean_rates['A[5,2]'], 5), 0))
  (EDprimer <- c(0, rep(mean_rates['A[5,4]'], 5), 0))
  (ENprimer <- c(0, rep(mean_rates['A[5,1]'], 5), 0))
  (NDprimer <- c(0, rep(mean_rates['A[1,4]'], 5), 0))
  (QDprimer <- c(0, rep(mean_rates['A[3,4]'], 5), 0))
  # 
  JPTimesVec <- c(4,15,10,10,10,10,11)
  rateNE <- createMat(NEprimer, JPTimesVec)
  rateND <- createMat(NDprimer, JPTimesVec)
  rateCD <- createMat(CDprimer, JPTimesVec)
  rateCE <- createMat(CEprimer, JPTimesVec)
  rateQD <- createMat(QDprimer, JPTimesVec)
  rateQE <- createMat(QEprimer, JPTimesVec)
  rateDC <- createMat(DCprimer, JPTimesVec)
  rateDQ <- createMat(DQprimer, JPTimesVec)
  rateDE <- createMat(DEprimer, JPTimesVec)
  rateEC <- createMat(ECprimer, JPTimesVec)
  rateED <- createMat(EDprimer, JPTimesVec)
  rateEN <- createMat(ENprimer, JPTimesVec)
} else if(big == "UK"){
  bigMatDir <- bigMatDirUK
  uktrans_list <- readRDS(file.path(bigMatDir, "uktrans_list.rds"))
  (NEprimer <- c(0, uktrans_list$NE))
  (CDprimer <- c(0, uktrans_list$CD))
  (CEprimer <- c(0, uktrans_list$CE))
  (QEprimer <- c(0, uktrans_list$QE))
  (DCprimer <- c(0, uktrans_list$DC))
  (DQprimer <- c(0, uktrans_list$DQ))
  (DEprimer <- c(0, uktrans_list$DE))
  (ECprimer <- c(0, uktrans_list$EC))
  (EDprimer <- c(0, uktrans_list$ED))
  (ENprimer <- c(0, uktrans_list$EN))
  (NDprimer <- c(0, uktrans_list$ND))
  (QDprimer <- c(0, uktrans_list$QD))
  # 
  UKTimesVec <- c(4, 2, 3, rep(5,4), rep(10,3), 11)
  rateNE <- createMat(NEprimer, UKTimesVec)
  rateND <- createMat(NDprimer, UKTimesVec)
  rateCD <- createMat(CDprimer, UKTimesVec)
  rateCE <- createMat(CEprimer, UKTimesVec)
  rateQD <- createMat(QDprimer, UKTimesVec)
  rateQE <- createMat(QEprimer, UKTimesVec)
  rateDC <- createMat(DCprimer, UKTimesVec)
  rateDQ <- createMat(DQprimer, UKTimesVec)
  rateDE <- createMat(DEprimer, UKTimesVec)
  rateEC <- createMat(ECprimer, UKTimesVec)
  rateED <- createMat(EDprimer, UKTimesVec)
  rateEN <- createMat(ENprimer, UKTimesVec)
} 