library(grid)
###########################################################################################
## Out directories
outDir <- "code/figures/diagram/adhoc/output"
###########################################################################################
# Data
NC="55.1% (50.0%, 59.7%)"; NE="8.9% (6.7%, 11.6%)"
NEC="2.3% (0.9%, 4.0%)"; NED="2.4% (1.2%, 4.3%)"; NEQ="3.9% (2.1%, 6.3%)"
## Plot setting
a=0.3; b=0.1
cCol = 'skyblue'; cBorderCol = 'blue' # Color for non-ecig bubble
eCol = 'orange'; eBorderCol = 'darkorange' # Color for ecig bubble
qCol = NA; qBorderCol = 'darkgrey' # Color for question bubble
###########################################################################################
# Helper functions
bubble = function(x,y,main,sub,colour1,colour2) {
  # 1 is inside, 2 is outside
  # a=0.3;b=0.1
  temp=seq(0,a-b,length.out = 100)
  xd=c(-b-temp,rev(-b-temp),b+temp,rev(b+temp))#,b+temp,rev(b+temp),rev(-b-temp))
  yd=c(sqrt((a-b)^2-temp^2),rev(-sqrt((a-b)^2-temp^2)),-sqrt((a-b)^2-temp^2),rev(sqrt((a-b)^2-temp^2)))
  grid.polygon(x+xd,y+yd,default.units = 'native',gp=gpar(col=colour2,fill=colour1,lwd=3))
  #grid.points(x,y,pch=16,gp=gpar(cex=10,col=colour1))
  #grid.points(x,y,pch=1 ,gp=gpar(cex=10,col=colour2))
  grid.text(main,x,unit(y,'native')+unit(0.15,'lines'),gp=gpar(cex=5),default.units = 'native')
  grid.text(sub,x,unit(y,'native')+unit(-1.5,'lines'),gp=gpar(cex=1),default.units = 'native')
}

arow = function(x1,x2,y1,y2, lineWidth=1) {
  grid.lines(c(x1,x2),c(y1,y2),default.units = 'native',arrow = arrow(length=unit(0.5,'lines'), angle = 30, type='closed'),
                                                        gp=gpar(fill=1, lwd=lineWidth))
}

get_y <- function(x3, x1, x2, y1, y2) {
    m <- (y2-y1)/(x2-x1)
    c <- y1 - m*x1
    y3 <- m*x3 + c
    y3
}
###########################################################################################
# Plot 1
png(file.path(outDir,"plot1.png"),height=10,width=16,units='cm',res=400,pointsize=10)
pushViewport(plotViewport(c(1,0,1,0),xscale=c(-0.35,2.5),yscale=c(0, 1.8)))
# grid.rect()
yupper <- 1.3; ylower <- 0.5
xleft <- 0; xmid <- 0.7; xright <- 1.4
# Upper row
bubble(xleft,yupper,'N','Never smoker', cCol, cBorderCol)
bubble(xmid,yupper,'C','Current smoker', cCol, cBorderCol)
bubble(xright,yupper,'?',' ', qCol, qBorderCol)
arow(xleft+a, xleft+a+0.1, yupper, yupper) #NC
arow(xleft+3*a+0.1, xleft+3*a+0.2, yupper, yupper) #C?
grid.text(NC, 2.1, yupper, gp=gpar(cex=1.2, fontface="bold"), default.units = 'native')
# Lower row
bubble(xleft,ylower,'N','Never smoker', cCol, cBorderCol)
bubble(xmid,ylower,'E','E-cig vaper', eCol, eBorderCol)
bubble(xright,ylower,'?',' ', qCol, qBorderCol)
arow(xleft+a, xleft+a+0.1, ylower, ylower) #NE
arow(xleft+3*a+0.1, xleft+3*a+0.2, ylower, ylower) #E?
grid.text(NE, 2.1, ylower, gp=gpar(cex=1.2, fontface="bold"), default.units = 'native')
popViewport()
dev.off()

# Plot2
png(file.path(outDir,"plot2.png"),height=10,width=16,units='cm',res=400,pointsize=10)
pushViewport(plotViewport(c(1,0,1,0),xscale=c(-0.35,2.5),yscale=c(0, 1.8)))
# grid.rect()
ymid <- 0.9; yupper <- 1.5; ylower <- 0.3
xleft <- 0; xmid <- 0.7; xright <- 1.4
# Middle row
bubble(xleft, ymid, 'N','Never smoker', cCol, cBorderCol)
bubble(xmid, ymid, 'E','E-cig vaper', eCol, eBorderCol)
bubble(xright, ymid, 'D','Dual smoker', eCol, eBorderCol)
arow(xleft+a, xleft+a+0.1, ymid, ymid) #NC
arow(xleft+3*a+0.1, xleft+3*a+0.2, ymid, ymid) #CQ
grid.text(NED, 2.1, ymid, gp=gpar(cex=1.2, fontface="bold"), default.units = 'native')
# Upper row
bubble(xright,yupper,'C','Current smoker',cCol, cBorderCol)
arow(xmid+0.2, xright-0.2, get_y(xmid+0.2,xmid,xright, ymid, yupper), get_y(xright-0.2, xmid, xright, ymid, yupper))
grid.text(NEC, 2.1, yupper, gp=gpar(cex=1.2, fontface="bold"), default.units = 'native')
# Lower row
bubble(xright,ylower,'Q','Ex-smoker',cCol, cBorderCol)
arow(xmid+0.2, xright-0.2, get_y(xmid+0.2,xmid,xright, ymid, ylower), get_y(xright-0.2, xmid, xright, ymid, ylower))
grid.text(NEQ, 2.1, ylower, gp=gpar(cex=1.2, fontface="bold"), default.units = 'native')
popViewport()
dev.off()