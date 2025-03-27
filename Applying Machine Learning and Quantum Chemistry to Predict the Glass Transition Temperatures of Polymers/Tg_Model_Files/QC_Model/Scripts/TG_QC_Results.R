rm(list=ls())
graphics.off()
library(tidyverse)
library(readxl)
library(MASS)

home="C:/Users/khickey/Documents/Gauss_Polymers/"

# Data file
data=read_excel(paste(home,"Measure_Tg_QC.xlsx",sep=""),sheet="1_2_3_1M_Class",range="A1:AM85",na="NA")


# omit unnecessary columns and remove NAs
data_clean=data.frame(data[,-c(2:12)])
data_clean=na.omit(data_clean)
# Convert to celsius
data_clean$Tg=data_clean$Tg-273.15

# omit poor performing parameters
data_2=subset(data_clean,select=-c(chemical,kow_rad,ether,log_Kow,acryl,quad_avg,qp,alpha_avg,Radius,
                                   TPSA,logP,natoms,MW,nON,nOHNH,nviol,volume))
# Stepwise regression
full=lm(Tg ~ ., data=data_2)
step=stepAIC(full,direction="both")

#################################### Plots #######################################################
#   set size of graphic
x11(height=8,width=9)
fig_holder=paste0(home)
#png(paste(fig_holder,"/Tg_Model3_C_TNR_Blue_nolab.png",sep=""),height=8,width=9,units="in",res=500)

windowsFonts(A=windowsFont("Times New Roman"))
#  colors
colPF=grey.colors(6)
colP=c("grey85","white")
colP=c("blue","red")
colL="grey80"
colCL=c("grey95","grey85")
cexL=2
cexP=3
cexT=1.5
cexTitle=1.7
cexleg=1.5
cexleg2=1.75
# margins
marV=2
par(oma=c(4,4,2,0))
par(mar=c(0,0,0,0))
par(fig=c(0,1,0,1))

# Axes dimensions
par(new=F)
yMin=-120; yMax=330
xMin=-120 ; xMax=330

yLim=c(yMin,yMax)
xLim=c(xMin,xMax)
# Set plot space
plot(NA,axes=FALSE,xlab="",ylab="",xlim=xLim,ylim=yLim,family="Times New Roman")
# plot 1:1 and error lines
sigma=summary(step)$sigma
lines(x=c(xMin,xMax),y=c(yMin,yMax),lwd=3,xpd=NA,col="grey",lty=1)
lines(x=c(xMin+sigma,xMax),y=c(yMin,yMax-sigma),lwd=3,xpd=NA,col="grey",lty=3)
lines(x=c(xMin,xMax-sigma),y=c(yMin+sigma,yMax),lwd=3,xpd=NA,col="grey",lty=3)
points(y=unlist(c(step$fitted.values)),x=unlist(c(data_clean$Tg)),pch=21,bg="blue",lwd=1.5,cex=cexP,xpd=NA)
# Calculate model error (RMSE)
rmse=sqrt(mean((step$fitted.values-data_clean$Tg)^2))

# Axis Parameters
xS=seq(xMin,xMax,50)
xAxisLab=as.character(xS)
axis(1,at=xS,labels=xAxisLab,cex=cexT,cex.axis=1.5,family="A")
xLab=expression(paste("Measured T"[g]," (",degree,"C)",sep=""))
mtext(side=1,line=3.0,text=xLab,cex=cexT,family="A")

yS=seq(yMin,yMax,50)
yAxisLab=as.character(yS)
axis(2,at=yS,labels=yAxisLab,cex=cexT,cex.axis=1.5,family="A")
yLab=expression(paste("Predicted T"[g]," (",degree,"C)",sep=""))
mtext(side=2,line=2.25,text=yLab,cex=cexT,family="A")

title="Pred. vs. Obs. Rate Constants"
# Pull constants and set title
Eth=round(step$coefficients[2],2)
cv=round(step$coefficients[3],2)
qp=round(step$coefficients[4],2)
quad=round(step$coefficients[5],2)
int=round(step$coefficients[1],2)
mult_r=round(summary(step)$r.squared,2)
n=length(data_clean$Tg)
se=round(summary(step)$sigma,2)

text(x=xMin,y=yMax+20,labels=bquote("n ="~.(n)*"; "~R^2==.(mult_r)*"; RMSE ="~.(round(rmse,digits=2))),
     cex=cexT,font=1,pos=4,xpd=NA,family="A")

# Print Model Summaries
print(summary(step))
print(paste0("rsme: ",rmse))
# Toggle graphics window for file saves
#graphics.off()