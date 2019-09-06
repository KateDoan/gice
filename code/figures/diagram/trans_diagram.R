library(grid)

###########################################################################################
## In directories
bigMatDirUS <- "code/transitions/path1_pool/transition_output"
bigMatDirUK <- "code/transitions/uk_jags/ukv/output"
bigMatDirJP <- "code/transitions/japan_jags/output"
## Out directories
outDir <- "code/figures/diagram/output"
###########################################################################################

# Helper functions
get_intercept_slope <- function(x1, x2, y1, y2){
    m <- (y2-y1)/(x2-x1)
    c <- y1 - m*x1
    return(list(m=m, c=c))
}

get_y <- function(x3, x1, x2, y1, y2){
    line <- get_intercept_slope(x1, x2, y1, y2)
    y3 <- line$m*x3 + line$c
    y3
}

# Graph functions
bubble = function(x,y,main,sub,colour1,colour2)# 1 is inside, 2 is outside
{
  a=0.3;b=0.1
  temp=seq(0,a-b,length.out = 100)
  xd=c(-b-temp,rev(-b-temp),b+temp,rev(b+temp))#,b+temp,rev(b+temp),rev(-b-temp))
  yd=c(sqrt((a-b)^2-temp^2),rev(-sqrt((a-b)^2-temp^2)),-sqrt((a-b)^2-temp^2),rev(sqrt((a-b)^2-temp^2)))
  grid.polygon(x+xd,y+yd,default.units = 'native',gp=gpar(col=colour2,fill=colour1,lwd=3))
  #grid.points(x,y,pch=16,gp=gpar(cex=10,col=colour1))
  #grid.points(x,y,pch=1 ,gp=gpar(cex=10,col=colour2))
  grid.text(main,x,unit(y,'native')+unit(0.15,'lines'),gp=gpar(cex=4.5),default.units = 'native')
  grid.text(sub,x,unit(y,'native')+unit(-1.5,'lines'),gp=gpar(cex=1),default.units = 'native')
}

arow = function(x1,x2,y1,y2, lineWidth=1)
{
  grid.lines(c(x1,x2),c(y1,y2),default.units = 'native',arrow = arrow(length=unit(1,'lines'),type='closed'),gp=gpar(fill=1, lwd=lineWidth))
}

plotTransitionWithLabel <- function(plotName, tm=matrix(1, ncol=5, nrow=5), opt="EN"){
    # tm: transition matrix
    max_width = 10
    tm_old = matrix(paste0(round(tm*100,1), "%"), nrow=5)
    tm = tm * max_width
    png(file.path(outDir,plotName),height=10,width=16,units='cm',res=400,pointsize=10)
    pushViewport(plotViewport(c(1,1,1,1),xscale=c(-0.85,1.85),yscale=c(0.2,1.8)))
    grid.rect()
    
    arow(-0.2, 0.2, 1.5, 1.5, tm[1,2]) #NC
    grid.text(tm_old[1,2], 0.2-0.2, 1.5+0.05, default.units = "native")
    # arow(0.8, 1.2, 1.5, 1.5) #CQ
    arow(0.8, 1.2, 1.5+0.045, 1.5+0.045, tm[2,3]) #CQ
    grid.text(tm_old[2,3], 1.2-0.2, 1.5+0.045+0.05, default.units = "native")
    arow(1.2, 0.8, 1.5-0.045, 1.5-0.045, tm[3,2]) #QC
    grid.text(tm_old[3,2], 0.8+0.2, 1.5-0.045-0.05, default.units = "native")
    # arow(0.3, 0.7, 0.5, 0.5) #ED
    arow(0.3, 0.7, 0.5+0.045, 0.5+0.045, tm[5,4]) #ED
    grid.text(tm_old[5,4], 0.7-0.2, 0.5+0.045+0.05, default.units = "native")
    arow(0.7, 0.3, 0.5-0.045, 0.5-0.045, tm[4,5]) #DE
    grid.text(tm_old[4,5], 0.3+0.2, 0.5-0.045-0.05, default.units = "native")
    # arow(0.4, 0.1, get_y(0.4, 0.5, 0, 1.5, 0.5), get_y(0.1, 0.5, 0, 1.5, 0.5)) #CE
    arow(0.45, 0.15, get_y(0.45, 0.55, 0.05, 1.5, 0.5), get_y(0.15, 0.55, 0.05, 1.5, 0.5), tm[2,5]) #CE
    grid.text(tm_old[2,5], 0.15+0.13, get_y(0.15+0.13, 0.55+0.05, 0.05+0.05, 1.5, 0.5), default.units = "native", rot = 60)
    arow(0.055, 0.35, get_y(0.055, 0.45, -0.05, 1.5, 0.5), get_y(0.35, 0.45, -0.05, 1.5, 0.5), tm[5,2]) #EC
    grid.text(tm_old[5,2], 0.35-0.13, get_y(0.35-0.13, 0.45-0.05, -0.05-0.05, 1.5, 0.5), default.units = "native", rot = 65)
    # arow(0.6, 0.9, get_y(0.6, 0.5, 1, 1.5, 0.5), get_y(0.9, 0.5, 1, 1.5, 0.5)) #CD
    arow(0.6-0.05, 0.9-0.05, get_y(0.6-0.05, 0.5-0.05, 1-0.05, 1.5, 0.5), get_y(0.9-0.05, 0.5-0.05, 1-0.05, 1.5, 0.5), tm[2,4]) #CD
    grid.text(tm_old[2,4], 0.9-0.05-0.11, get_y(0.9-0.05-0.11, 0.5-0.05-0.05, 1-0.05-0.05, 1.5, 0.5), default.units = "native", rot = 295)
    arow(0.9+0.045, 0.6+0.055, get_y(0.9+0.045, 0.5+0.05, 1+0.05, 1.5, 0.5), get_y(0.6+0.055, 0.5+0.05, 1+0.05, 1.5, 0.5), tm[4,2]) #DC
    grid.text(tm_old[4,2], 0.6+0.055+0.11, get_y(0.6+0.055+0.11, 0.5+0.05+0.05, 1+0.05+0.05, 1.5, 0.5), default.units = "native", rot = 300)
    # arow(0.24, 1.27, get_y(0.24, 0, 1.5, 0.5, 1.5), get_y(1.27, 0, 1.5, 0.5, 1.5)) #EQ
    arow(1.24+0.066, 0.25+0.01, get_y(1.24+0.065, 0+0.075, 1.5+0.075, 0.5, 1.5), get_y(0.25+0.01, 0+0.075, 1.5+0.075, 0.5, 1.5), tm[3,5]) #QE
    grid.text(tm_old[3,5], 0.25+0.01+0.2, get_y(0.25+0.01+0.2, 0+0.075+0.07, 1.5+0.075+0.07, 0.5, 1.5), default.units = "native", rot = 33)
    if(opt == "EQ"){
      arow(0.24-0.045, 1.27-0.04, get_y(0.24-0.045, 0-0.075, 1.5-0.075, 0.5, 1.5), get_y(1.27-0.04, 0-0.075, 1.5-0.075, 0.5, 1.5), tm[5,3]) #EQ
      grid.text(tm_old[5,3], 1.27-0.03-0.2, get_y(1.27-0.03-0.2, 0-0.075-0.1, 1.5-0.075-0.1, 0.5, 1.5), default.units = "native", rot = 33)
    }
    
    if(opt == "EN"){
      # arow(-0.4, -0.1, get_y(-0.4, -0.5, 0, 1.5, 0.5), get_y(-0.1, -0.5, 0, 1.5, 0.5), tm[1,5]) #NE
      arow(-0.4-0.05, -0.1-0.05, get_y(-0.4-0.05, -0.5-0.05, 0-0.05, 1.5, 0.5), get_y(-0.1-0.05, -0.5-0.05, 0-0.05, 1.5, 0.5), tm[1,5]) #NE
      grid.text(tm_old[1,5], -0.1-0.05-0.11, get_y(-0.1-0.05-0.11, -0.5-0.05-0.04, 0-0.05-0.04, 1.5, 0.5), default.units = "native", rot = 295)
      arow(-0.1+0.05, -0.4+0.05, get_y(-0.1+0.05, -0.5+0.05, 0+0.05, 1.5, 0.5), get_y(-0.4+0.05, -0.5+0.05, 0+0.05, 1.5, 0.5), tm[5,1]) #EN
      grid.text(tm_old[5,1], -0.4+0.05+0.11, get_y(-0.4+0.05+0.11, -0.5+0.05+0.05, 0+0.05+0.05, 1.5, 0.5), default.units = "native", rot = 300)
    }
    
    arow(1.105, 1.392, get_y(1.105, 1, 1.5, 0.5, 1.5), get_y(1.392, 1, 1.5, 0.5, 1.5), tm[4,3]) #DQ
    grid.text(tm_old[4,3], 1.392-0.13, get_y(1.392-0.13, 1-0.05, 1.5-0.05, 0.5, 1.5), default.units = "native", rot = 65)
    arow(1.4+0.1, 1.105+0.09, get_y(1.4+0.1, 1+0.1, 1.5+0.1, 0.5, 1.5), get_y(1.105+0.09, 1+0.1, 1.5+0.1, 0.5, 1.5), tm[3,4]) #QD
    grid.text(tm_old[3,4], 1.105+0.09+0.03, get_y(1.105+0.09+0.03, 1+0.05, 1.5+0.05, 0.5, 1.5), default.units = "native", rot = 65)
    
    if(opt == "EN"){
      arow(-0.5+0.24, 1-0.24, get_y(-0.5+0.24,-0.5,1,1.5,0.5), get_y(1-0.24,-0.5,1,1.5,0.5), tm[1,4]) #ND
      grid.text(tm_old[1,4], 1-0.24-0.11, get_y(1-0.24-0.11,-0.5+0.06,1+0.06,1.5,0.5), default.units = "native", rot = 325) #ND
    }
    
    bubble(-0.5,1.5,'N','Never smoker','skyblue','blue')
    bubble(0.5,1.5,'C','Current smoker','skyblue','blue')
    bubble(1.5,1.5,'Q','Ex-smoker','skyblue','blue')
    bubble(0,0.5,'E','E-cig user','orange','darkorange')
    bubble(1,0.5,'D','Dual user','orange','darkorange')
    dev.off()
}
##########################################################################################
# Transition plots  for Japan
meanRates <- readRDS(file.path(bigMatDirJP, "mean_rates.rds"))
JPsampl1529 <- 881 + 1462
JPsampl_total <- 8240
NEyouth <- meanRates['A[1,5]'] * JPsampl_total/JPsampl1529
tm_young <- rbind(c(1-meanRates['A[1,2]']-meanRates['A[1,4]']-NEyouth,meanRates['A[1,2]'], 0,meanRates['A[1,4]'],NEyouth),
            c(0,meanRates['A[2,2]'],meanRates['A[2,3]'],meanRates['A[2,4]'],meanRates['A[2,5]']),
            c(0,meanRates['A[3,2]'],meanRates['A[3,3]'],meanRates['A[3,4]'],meanRates['A[3,5]']),
            c(0,meanRates['A[4,2]'],meanRates['A[4,3]'],meanRates['A[4,4]'],meanRates['A[4,5]']),
            c(meanRates['A[5,1]'],meanRates['A[5,2]'], 0, meanRates['A[5,4]'],meanRates['A[5,5]']))
# plotTransition("japan_transition.png", tm)
plotTransitionWithLabel(paste0("EN", "japan_transition_with_label.png"), tm_young, opt="EN")

tm_old <- rbind(c(1-meanRates['A[1,2]']-meanRates['A[1,4]'],meanRates['A[1,2]'], 0,meanRates['A[1,4]'],0),
                  c(0,meanRates['A[2,2]'],meanRates['A[2,3]'],meanRates['A[2,4]'],meanRates['A[2,5]']),
                  c(0,meanRates['A[3,2]'],meanRates['A[3,3]'],meanRates['A[3,4]'],meanRates['A[3,5]']),
                  c(0,meanRates['A[4,2]'],meanRates['A[4,3]'],meanRates['A[4,4]'],meanRates['A[4,5]']),
                  c(0,meanRates['A[5,2]'], meanRates['A[5,1]'], meanRates['A[5,4]'],meanRates['A[5,5]']))
plotTransitionWithLabel(paste0("EQ", "japan_transition_with_label.png"), tm_old, opt="EQ")
##########################################################################################
# Transition plots for the US
tm1217 <- readRDS(file.path(bigMatDirUS,"params_pos1217.rds"))
tm1824 <- readRDS(file.path(bigMatDirUS,"params_pos1824.rds"))
tm2534 <- readRDS(file.path(bigMatDirUS,"params_pos2534.rds"))
tm3544 <- readRDS(file.path(bigMatDirUS,"params_pos3544.rds"))
tm4554 <- readRDS(file.path(bigMatDirUS,"params_pos4554.rds"))
tm5564 <- readRDS(file.path(bigMatDirUS,"params_pos5564.rds"))
tm6574 <- readRDS(file.path(bigMatDirUS,"params_pos6574.rds"))
tm75plus <- readRDS(file.path(bigMatDirUS,"params_pos75plus.rds"))

label_vec <- c("1217","1824","2534","3544","4554","5564","6574","75plus")
tmlist <- list(tm1217,tm1824,tm2534,tm3544,tm4554,tm5564,tm6574,tm75plus)

for(i in 1:2){
    tm <- rbind(c(tmlist[[i]]$NN,tmlist[[i]]$NC,0,tmlist[[i]]$ND,tmlist[[i]]$NE),
                c(0,tmlist[[i]]$CC,tmlist[[i]]$CQ,tmlist[[i]]$CD,tmlist[[i]]$CE),
                c(0,tmlist[[i]]$QC,tmlist[[i]]$QQ,tmlist[[i]]$QD,tmlist[[i]]$QE),
                c(0,tmlist[[i]]$DC,tmlist[[i]]$DQ,tmlist[[i]]$DD,tmlist[[i]]$DE),
                c(tmlist[[i]]$EQ, tmlist[[i]]$EC, 0,tmlist[[i]]$ED,tmlist[[i]]$EE))
    tm[is.na(tm)] <- 0
    plotTransitionWithLabel(paste0("us_transition_",label_vec[i],"_with_label.png"), tm, opt="EN")
}

for(i in 3:8){
  tm <- rbind(c(1-tmlist[[i]]$NC,tmlist[[i]]$NC,0,0,0),
              c(0,tmlist[[i]]$CC,tmlist[[i]]$CQ,tmlist[[i]]$CD,tmlist[[i]]$CE),
              c(0,tmlist[[i]]$QC,tmlist[[i]]$QQ,tmlist[[i]]$QD,tmlist[[i]]$QE),
              c(0,tmlist[[i]]$DC,tmlist[[i]]$DQ,tmlist[[i]]$DD,tmlist[[i]]$DE),
              c(0, tmlist[[i]]$EC, tmlist[[i]]$EQ,tmlist[[i]]$ED,tmlist[[i]]$EE))
  tm[is.na(tm)] <- 0
  plotTransitionWithLabel(paste0("us_transition_",label_vec[i],"_with_label.png"), tm, opt="EQ")
}
##########################################################################################
# Transition plots for the UK
ukrates <- readRDS(file.path(bigMatDirUK, "uktrans_list.rds"))
age_label <- c("1516","1719","2024", "2529", "3034",
               "3539", "4049", "5059", "6069", "7079")
for(i in 1:3){
    tm <- rbind(c(ukrates$NN[i], ukrates$NC[i], 0, ukrates$ND[i], ukrates$NE[i]),
                c(0, ukrates$CC[i], ukrates$CQ[i], ukrates$CD[i], ukrates$CE[i]),
                c(0, ukrates$QC[i], ukrates$QQ[i], ukrates$QD[i], ukrates$QE[i]),
                c(0, ukrates$DC[i], ukrates$DQ[i], ukrates$DD[i], ukrates$DE[i]),
                c(ukrates$EN[i], ukrates$EC[i], 0, ukrates$ED[i], ukrates$EE[i]))
    
    plotTransitionWithLabel(paste0("uk_transition_", age_label[i], "plus_with_label.png"), tm, opt="EN")
}

for(i in 4:10){
  tm <- rbind(c(1-ukrates$NC[i], ukrates$NC[i], 0, 0, 0),
              c(0, ukrates$CC[i], ukrates$CQ[i], ukrates$CD[i], ukrates$CE[i]),
              c(0, ukrates$QC[i], ukrates$QQ[i], ukrates$QD[i], ukrates$QE[i]),
              c(0, ukrates$DC[i], ukrates$DQ[i], ukrates$DD[i], ukrates$DE[i]),
              c(0, ukrates$EC[i], ukrates$EN[i], ukrates$ED[i], ukrates$EE[i]))
  
  plotTransitionWithLabel(paste0("uk_transition_", age_label[i], "plus_with_label.png"), tm, opt="EQ")
}