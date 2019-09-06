bigMatDirUS <- file.path("code", "transitions", "path1_pool", "transition_output")
bigMatDirUK <- file.path("code", "transitions", "uk_jags", "ukv", "output")
bigMatDirJP <- file.path("code", "transitions", "japan_jags", "output")
#######################################################################################################
readEcigTrans <- function(big){
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
    rateNE <- rep(NEprimer, USTimesVec)
    rateCD <- rep(CDprimer, USTimesVec)
    rateCE <- rep(CEprimer, USTimesVec)
    rateQE <- rep(QEprimer, USTimesVec)
    rateDC <- rep(DCprimer, USTimesVec)
    rateDQ <- rep(DQprimer, USTimesVec)
    rateDE <- rep(DEprimer, USTimesVec)
    rateEC <- rep(ECprimer, USTimesVec)
    rateED <- rep(EDprimer, USTimesVec)
    rateEN <- rep(ENprimer, USTimesVec)
    rateND <- rep(NDprimer, USTimesVec)
    rateQD <- rep(QDprimer, USTimesVec)
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
    rateNE <- rep(NEprimer, UKTimesVec)
    rateND <- rep(NDprimer, UKTimesVec)
    rateCD <- rep(CDprimer, UKTimesVec)
    rateCE <- rep(CEprimer, UKTimesVec)
    rateQD <- rep(QDprimer, UKTimesVec)
    rateQE <- rep(QEprimer, UKTimesVec)
    rateDC <- rep(DCprimer, UKTimesVec)
    rateDQ <- rep(DQprimer, UKTimesVec)
    rateDE <- rep(DEprimer, UKTimesVec)
    rateEC <- rep(ECprimer, UKTimesVec)
    rateED <- rep(EDprimer, UKTimesVec)
    rateEN <- rep(ENprimer, UKTimesVec)
  }
  
  ecigTrans <- list(NE=rateNE, ND=rateND, CD=rateCD, CE=rateCE, QD=rateQD,
                    QE=rateQE, DC=rateDC, DQ=rateDQ, DE=rateDE, EC=rateEC,
                    ED=rateED, EN=rateEN)
  
  ecigTrans
}