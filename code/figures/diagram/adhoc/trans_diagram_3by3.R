library(grid)
###########################################################################################
## In directories

## Out directories
outDir <- "code/figures/diagram/adhoc/output"
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
  grid.text(main,x,unit(y,'native')+unit(0.15,'lines'),gp=gpar(cex=5),default.units = 'native')
  grid.text(sub,x,unit(y,'native')+unit(-1.5,'lines'),gp=gpar(cex=1),default.units = 'native')
}

arow = function(x1,x2,y1,y2, lineWidth=1)
{
  grid.lines(c(x1,x2),c(y1,y2),default.units = 'native',arrow = arrow(length=unit(1,'lines'),type='closed'),gp=gpar(fill=1, lwd=lineWidth))
}

plotTransition3By3 <- function(plotName, tm=matrix(1, ncol=5, nrow=5)){
    # tm: transition matrix
    max_width = 10
    tm = tm * max_width
    png(file.path(outDir,plotName),height=4,width=16,units='cm',res=400,pointsize=10)
    pushViewport(plotViewport(c(1,1,1,1),xscale=c(-0.85,1.85),yscale=c(1.2, 1.8)))
    grid.rect()
    bubble(-0.5,1.5,'N','Never smoker','skyblue','blue')
    bubble(0.5,1.5,'C','Current smoker','skyblue','blue')
    bubble(1.5,1.5,'Q','Ex-smoker','skyblue','blue')
    
    arow(-0.2, 0.2, 1.5, 1.5, tm[1,2]) #NC
    # arow(0.8, 1.2, 1.5, 1.5) #CQ
    arow(0.8, 1.2, 1.5+0.045, 1.5+0.045, tm[2,3]) #CQ
    arow(1.2, 0.8, 1.5-0.045, 1.5-0.045, tm[3,2]) #QC
    
    grid.text('x',y=unit(-3,'lines'))
    grid.text('y',x=unit(-3,'lines'),rot=90)
    popViewport()
    dev.off()
}
plotTransitionWithLabel3By3 <- function(plotName, tm=matrix(1, ncol=5, nrow=5)){
    # tm: transition matrix
    tm = round(tm, 3)
    max_width = 10
    tm_old = tm
    tm = tm * max_width
    png(file.path(outDir,plotName),height=4,width=16,units='cm',res=400,pointsize=10)
    pushViewport(plotViewport(c(1,1,1,1),xscale=c(-0.85,1.85),yscale=c(1.2,1.8)))
    grid.rect()
    bubble(-0.5,1.5,'N','Never smoker','skyblue','blue')
    bubble(0.5,1.5,'C','Current smoker','skyblue','blue')
    bubble(1.5,1.5,'Q','Ex-smoker','skyblue','blue')
    
    arow(-0.2, 0.2, 1.5, 1.5, tm[1,2]) #NC
    grid.text(tm_old[1,2], 0.2-0.2, 1.5+0.05, default.units = "native")
    # arow(0.8, 1.2, 1.5, 1.5) #CQ
    arow(0.8, 1.2, 1.5+0.045, 1.5+0.045, tm[2,3]) #CQ
    grid.text(tm_old[2,3], 1.2-0.2, 1.5+0.045+0.05, default.units = "native")
    arow(1.2, 0.8, 1.5-0.045, 1.5-0.045, tm[3,2]) #QC
    grid.text(tm_old[3,2], 0.8+0.2, 1.5-0.045-0.05, default.units = "native")
    
    grid.text('x',y=unit(-3,'lines'))
    grid.text('y',x=unit(-3,'lines'),rot=90)
    popViewport()
    dev.off()
}
