library(openxlsx)
library(tidyverse)
selectPrice <- read.xlsx("D:/BondPrice/SelectedBond.xlsx",sheet="Sheet1")
dirtyPriceC = data.frame(dirty115=rep(0,11), dirty114=rep(0,11),dirty113=rep(0,11),dirty110=rep(0,11),
                         dirty109=rep(0,11),dirty108=rep(0,11),dirty107=rep(0,11),dirty106=rep(0,11),
                         dirty103=rep(0,11),dirty102=rep(0,11))
ytmValue = data.frame(ytm115=rep(0,11), ytm114=rep(0,11),ytm113=rep(0,11),ytm110=rep(0,11),
                      ytm109=rep(0,11),ytm108=rep(0,11),ytm107=rep(0,11),ytm106=rep(0,11),
                      ytm103=rep(0,11),ytm102=rep(0,11))
spotValue = data.frame(spot115=rep(0,11),spot114=rep(0,11),spot113=rep(0,11),spot110=rep(0,11),
                       spot109=rep(0,11),spot108=rep(0,11),spot107=rep(0,11),spot106=rep(0,11),
                       spot103=rep(0,11),spot102=rep(0,11))
selectPrice <- cbind(selectPrice,dirtyPriceC)
selectPrice <- cbind(selectPrice,ytmValue)
selectPrice <- cbind(selectPrice,spotValue)
counter <- data.frame(100,99,98,95,94,93,92,91,88,87)



## calculate dirty price in first bond
for (days in 1:10) {
  countDown <- (236-counter[days])
  selectPrice[1,days+15] <- selectPrice[1,days+3]+selectPrice$Coupon[1]*(countDown/365)
}
## calculate dirty price in second bond
for (days in 1:10) {
  countDown <- (236-counter[days])
  selectPrice[2,days+15] <- selectPrice[2,days+3]+selectPrice$Coupon[2]*(countDown/365)
}
## calculate dirty price in 3 bond
for (days in 1:10) {
  countDown <- (222-counter[days])
  selectPrice[3,days+15] <- selectPrice[3,days+3]+selectPrice$Coupon[3]*(countDown/365)
}## calculate dirty price in 4 bond
for (days in 1:10) {
  countDown <- (236-counter[days])
  selectPrice[4,days+15] <- selectPrice[4,days+3]+selectPrice$Coupon[4]*(countDown/365)
}## calculate dirty price in 5 bond
for (days in 1:10) {
  countDown <- (236-counter[days])
  selectPrice[5,days+15] <- selectPrice[5,days+3]+selectPrice$Coupon[5]*(countDown/365)
}## calculate dirty price in 6 bond
for (days in 1:10) {
  countDown <- (145-counter[days])
  selectPrice[6,days+15] <- selectPrice[6,days+3]+selectPrice$Coupon[6]*(countDown/365)
}## calculate dirty price in 7 bond
for (days in 1:10) {
  countDown <- (236-counter[days])
  selectPrice[7,days+15] <- selectPrice[7,days+3]+selectPrice$Coupon[7]*(countDown/365)
}## calculate dirty price in 8 bond
for (days in 1:10) {
  countDown <- (145-counter[days])
  selectPrice[8,days+15] <- selectPrice[8,days+3]+selectPrice$Coupon[8]*(countDown/365)
}## calculate dirty price in 9s bond
for (days in 1:10) {
  countDown <- (236-counter[days])
  selectPrice[9,days+15] <- selectPrice[9,days+3]+selectPrice$Coupon[9]*(countDown/365)
}## calculate dirty price in 10s bond
for (days in 1:10) {
  countDown <- (236-counter[days])
  selectPrice[10,days+15] <- selectPrice[10,days+3]+selectPrice$Coupon[10]*(countDown/365)
}## calculate dirty price in 10s bond
for (days in 1:10) {
  countDown <- (236-counter[days])
  selectPrice[11,days+15] <- selectPrice[11,days+3]+selectPrice$Coupon[11]*(countDown/365)
}

##Begin to calculate the YTM 
bval <- function(i, cf, t=seq(along = cf))sum(cf / (1 + i)^t)
ytm <- function(cf) {uniroot(bval, c(0, 1), cf = cf)$root}

##First try to calculate the first example, MAYBE HAVE PROBLEM WITH PAR VALUE
cf <- c(-selectPrice[1,1+15],rep(selectPrice$Coupon[1]*1000/2,10),1000+selectPrice$Coupon[1]*1000/2)
ytm(cf)


##Calculate ytm for #1 bond
for (days in 1:10) {
  cf <- c(-selectPrice[1,days+15],100+selectPrice$Coupon[1]*100/2)
  selectPrice[1,days+25] <- ytm(cf)
}##Calculate ytm for #2 bond
for (days in 1:10) {
  cf <- c(-selectPrice[2,days+15],rep(selectPrice$Coupon[2]*100/2,1),100+selectPrice$Coupon[2]*100/2)
  selectPrice[2,days+25] <- ytm(cf)
}##Calculate ytm for #3 bond
for (days in 1:10) {
  cf <- c(-selectPrice[3,days+15],rep(selectPrice$Coupon[3]*100/2,2),100+selectPrice$Coupon[3]*100/2)
  selectPrice[3,days+25] <- ytm(cf)
}##Calculate ytm for #4 bond
for (days in 1:10) {
  cf <- c(-selectPrice[4,days+15],rep(selectPrice$Coupon[4]*100/2,3),100+selectPrice$Coupon[4]*100/2)
  selectPrice[4,days+25] <- ytm(cf)
}##Calculate ytm for #5 bond
for (days in 1:10) {
  cf <- c(-selectPrice[5,days+15],rep(selectPrice$Coupon[5]*100/2,4),100+selectPrice$Coupon[5]*100/2)
  selectPrice[5,days+25] <- ytm(cf)
}##Calculate ytm for #6 bond
for (days in 1:10) {
  cf <- c(-selectPrice[6,days+15],rep(selectPrice$Coupon[6]*100/2,5),100+selectPrice$Coupon[6]*100/2)
  selectPrice[6,days+25] <- ytm(cf)
}##Calculate ytm for #7 bond
for (days in 1:10) {
  cf <- c(-selectPrice[7,days+15],rep(selectPrice$Coupon[7]*100/2,6),100+selectPrice$Coupon[7]*100/2)
  selectPrice[7,days+25] <- ytm(cf)
}##Calculate ytm for #8 bond
for (days in 1:10) {
  cf <- c(-selectPrice[8,days+15],rep(selectPrice$Coupon[8]*100/2,7),100+selectPrice$Coupon[8]*100/2)
  selectPrice[8,days+25] <- ytm(cf)
}##Calculate ytm for #9 bond
for (days in 1:10) {
  cf <- c(-selectPrice[9,days+15],rep(selectPrice$Coupon[9]*100/2,8),100+selectPrice$Coupon[9]*100/2)
  selectPrice[9,days+25] <- ytm(cf)
}##Calculate ytm for #10 bond
for (days in 1:10) {
  cf <- c(-selectPrice[10,days+15],rep(selectPrice$Coupon[10]*100/2,9),100+selectPrice$Coupon[10]*100/2)
  selectPrice[10,days+25] <- ytm(cf)
}##Calculate ytm for #11 bond
for (days in 1:10) {
  cf <- c(-selectPrice[11,days+15],rep(selectPrice$Coupon[11]*100/2,10),100+selectPrice$Coupon[11]*100/2)
  selectPrice[11,days+25] <- ytm(cf)
}


##Begin to draw curve
xdata <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5)
plot(xdata,selectPrice$ytm115 , type="o", col="blue",
     xlab = "Year to Maturity",ylab = "Yield to Maturity",main = "YTM curve")
lines(xdata, selectPrice$ytm114, col="red",lty=1)
lines(xdata, selectPrice$ytm113, col="chartreuse",lty=1)
lines(xdata, selectPrice$ytm110, col="brown",lty=1)
lines(xdata, selectPrice$ytm109, col="darkorchid",lty=1)
lines(xdata, selectPrice$ytm108, col="cyan",lty=1)
lines(xdata, selectPrice$ytm107, col="darkred",lty=1)
lines(xdata, selectPrice$ytm106, col="darkgreen",lty=1)
lines(xdata, selectPrice$ytm103, col="darkorange",lty=1)
lines(xdata, selectPrice$ytm102, col="darkgoldenrod",lty=1)
legend("topright",legend=c("ytm115","ytm114","ytm113","ytm110","ytm109","ytm108",
                           "ytm107","ytm106","ytm103","ytm102"),
       text.col=c("blue","red","chartreuse","brown","darkorchid","cyan","darkred",
               "darkgreen","darkorange","darkgoldenrod"),pt.lwd=0.5,x.intersp = 0.5,ncol=3, cex=0.5)

## Set up functions for calculating forward rate
## 1st spot rate
spotRate0 <- function(dirty, notional,t2M){
  spot<-(-(log(dirty/notional)/t2M))
  return(spot)
}
## 2nd spot rate
spotRate1 <- function(dirty,coupon,spot0,t1,t2){
  f.spot1<-function(rate) coupon*exp(1)^(-spot0*t1)+(100+coupon)*exp(1)^(-rate*t2)-dirty
  spot<-uniroot(f.spot1,interval=c(-10,10),extendInt="no")$root
  return(spot)
}
## Test: spotRate1(99.30279,0.75/2,0.005,1/3,1/4)
## 3nd spot rate
spotRate2 <- function(dirty,coupon,spot0,s2,t1,t2,t3){
  f.spot2<-function(rate) coupon*exp(1)^(-spot0*t1)+(coupon)*exp(1)^(-s2*t2)+(100+coupon)*exp(1)^(-rate*t3)-dirty
  spot<-uniroot(f.spot2,interval=c(-10,10),extendInt="no")$root
  return(spot)
}
## 4nd spot rate
spotRate3 <- function(dirty,coupon,spot0,s2,s3,t1,t2,t3,t4){
  f.spot3<-function(rate) coupon*exp(1)^(-spot0*t1)+(coupon)*exp(1)^(-s2*t2)+(coupon)*exp(1)^(-s3*t3)+
    (100+coupon)*exp(1)^(-rate*t4)-dirty
  spot<-uniroot(f.spot3,interval=c(-10,10),extendInt="no")$root
  return(spot)
}
## 5th spot rate
spotRate4 <- function(dirty,coupon,spot0,s2,s3,s4,t1,t2,t3,t4,t5){
  f.spot4<-function(rate) coupon*exp(1)^(-spot0*t1)+(coupon)*exp(1)^(-s2*t2)+(coupon)*exp(1)^(-s3*t3)+
    (coupon)*exp(1)^(-s4*t4)+(100+coupon)*exp(1)^(-rate*t5)-dirty
  spot<-uniroot(f.spot4,interval=c(-10,10),extendInt="no")$root
  return(spot)
}
## 6th spot rate
spotRate5 <- function(dirty,coupon,spot0,s2,s3,s4,s5,t1,t2,t3,t4,t5,t6){
  f.spot5<-function(rate) coupon*exp(1)^(-spot0*t1)+(coupon)*exp(1)^(-s2*t2)+(coupon)*exp(1)^(-s3*t3)+
    (coupon)*exp(1)^(-s4*t4)+(coupon)*exp(1)^(-s5*t5)+(100+coupon)*exp(1)^(-rate*t6)-dirty
  spot<-uniroot(f.spot5,interval=c(-10,10),extendInt="no")$root
  return(spot)
}
## 7th spot rate
spotRate6 <- function(dirty,coupon,spot0,s2,s3,s4,s5,s6,t1,t2,t3,t4,t5,t6,t7){
  f.spot6<-function(rate) coupon*exp(1)^(-spot0*t1)+(coupon)*exp(1)^(-s2*t2)+(coupon)*exp(1)^(-s3*t3)+
    (coupon)*exp(1)^(-s4*t4)+(coupon)*exp(1)^(-s5*t5)+(coupon)*exp(1)^(-s6*t6)+(100+coupon)*exp(1)^(-rate*t7)-dirty
  spot<-uniroot(f.spot6,interval=c(-10,10),extendInt="no")$root
  return(spot)
}
## 8th spot rate
spotRate7 <- function(dirty,coupon,spot0,s2,s3,s4,s5,s6,s7,t1,t2,t3,t4,t5,t6,t7,t8){
  f.spot7<-function(rate) coupon*exp(1)^(-spot0*t1)+(coupon)*exp(1)^(-s2*t2)+(coupon)*exp(1)^(-s3*t3)+
    (coupon)*exp(1)^(-s4*t4)+(coupon)*exp(1)^(-s5*t5)+(coupon)*exp(1)^(-s6*t6)+
    (coupon)*exp(1)^(-s7*t7)+(100+coupon)*exp(1)^(-rate*t8)-dirty
  spot<-uniroot(f.spot7,interval=c(-10,10),extendInt="no")$root
  return(spot)
}
## 9th spot rate
spotRate8 <- function(dirty,coupon,spot0,s2,s3,s4,s5,s6,s7,s8,t1,t2,t3,t4,t5,t6,t7,t8,t9){
  f.spot8<-function(rate) coupon*exp(1)^(-spot0*t1)+(coupon)*exp(1)^(-s2*t2)+(coupon)*exp(1)^(-s3*t3)+
    (coupon)*exp(1)^(-s4*t4)+(coupon)*exp(1)^(-s5*t5)+(coupon)*exp(1)^(-s6*t6)+
    (coupon)*exp(1)^(-s7*t7)+(coupon)*exp(1)^(-s8*t8)+(100+coupon)*exp(1)^(-rate*t9)-dirty
  spot<-uniroot(f.spot8,interval=c(-10,10),extendInt="no")$root
  return(spot)
}
## 10th spot rate
spotRate9 <- function(dirty,coupon,spot0,s2,s3,s4,s5,s6,s7,s8,s9,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10){
  f.spot9<-function(rate) coupon*exp(1)^(-spot0*t1)+(coupon)*exp(1)^(-s2*t2)+(coupon)*exp(1)^(-s3*t3)+
    (coupon)*exp(1)^(-s4*t4)+(coupon)*exp(1)^(-s5*t5)+(coupon)*exp(1)^(-s6*t6)+
    (coupon)*exp(1)^(-s7*t7)+(coupon)*exp(1)^(-s8*t8)+(coupon)*exp(1)^(-s9*t9)+(100+coupon)*exp(1)^(-rate*t10)-dirty
  spot<-uniroot(f.spot9,interval=c(-10,10),extendInt="no")$root
  return(spot)
}
## 11th spot rate
spotRate10 <- function(dirty,coupon,spot0,s2,s3,s4,s5,s6,s7,s8,s9,s10,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11){
  f.spot10<-function(rate) coupon*exp(1)^(-spot0*t1)+(coupon)*exp(1)^(-s2*t2)+(coupon)*exp(1)^(-s3*t3)+
    (coupon)*exp(1)^(-s4*t4)+(coupon)*exp(1)^(-s5*t5)+(coupon)*exp(1)^(-s6*t6)+(coupon)*exp(1)^(-s9*t9)+
    (coupon)*exp(1)^(-s7*t7)+(coupon)*exp(1)^(-s8*t8)+(coupon)*exp(1)^(-s10*t10)+(100+coupon)*exp(1)^(-rate*t11)-dirty
  spot<-uniroot(f.spot10,interval=c(-10,10),extendInt="no")$root
  return(spot)
}

## apply function to each row
## 1st row
for (days in 1:10) {
  selectPrice[1,days+35] <- spotRate0(selectPrice[1,days+15],100+selectPrice$Coupon[1]*100/2,1/4)
}
## 2nd row
for (days in 1:10) {
  selectPrice[2,days+35] <- spotRate1(selectPrice[2,days+15],selectPrice$Coupon[2]*100/2,selectPrice[1,days+35],1/3,0.5)
}
## 3nd spot rate
for (days in 1:10) {
  selectPrice[3,days+35] <- spotRate2(selectPrice[3,days+15],selectPrice$Coupon[3]*100/2,
                                      selectPrice[1,days+35],selectPrice[2,days+35],1/3,0.5,0.5)
}
## 4nd spot rate
for (days in 1:10) {
  selectPrice[4,days+35] <- spotRate3(selectPrice[4,days+15],selectPrice$Coupon[4]*100/2,
                                      selectPrice[1,days+35],selectPrice[2,days+35],selectPrice[3,days+35],
                                      1/3,0.5,0.5,0.5)
}
## 5th spot rate
for (days in 1:10) {
  selectPrice[5,days+35] <- spotRate4(selectPrice[5,days+15],selectPrice$Coupon[5]*100/2,
                                      selectPrice[1,days+35],selectPrice[2,days+35],selectPrice[3,days+35],selectPrice[4,days+35],
                                      1/3,0.5,0.5,0.5,0.5)
}
## 6th spot rate
for (days in 1:10) {
  selectPrice[6,days+35] <- spotRate5(selectPrice[6,days+15],selectPrice$Coupon[6]*100/2,
                                      selectPrice[1,days+35],selectPrice[2,days+35],selectPrice[3,days+35],selectPrice[4,days+35],selectPrice[5,days+35],
                                      1/3,0.5,0.5,0.5,0.5,1/4)
}
## 7th spot rate
for (days in 1:10) {
  selectPrice[7,days+35] <- spotRate6(selectPrice[7,days+15],selectPrice$Coupon[7]*100/2,
                                      selectPrice[1,days+35],selectPrice[2,days+35],selectPrice[3,days+35],selectPrice[4,days+35],
                                      selectPrice[5,days+35],selectPrice[6,days+35],
                                      1/3,0.5,0.5,0.5,0.5,1/4,0.75)
}
## 8th spot rate
for (days in 1:10) {
  selectPrice[8,days+35] <- spotRate7(selectPrice[8,days+15],selectPrice$Coupon[8]*100/2,
                                      selectPrice[1,days+35],selectPrice[2,days+35],selectPrice[3,days+35],selectPrice[4,days+35],
                                      selectPrice[5,days+35],selectPrice[6,days+35],selectPrice[7,days+35],
                                      1/3,0.5,0.5,0.5,0.5,1/4,0.75,1/4)
}
## 9th spot rate
for (days in 1:10) {
  selectPrice[9,days+35] <- spotRate8(selectPrice[9,days+15],selectPrice$Coupon[9]*100/2,
                                      selectPrice[1,days+35],selectPrice[2,days+35],selectPrice[3,days+35],selectPrice[4,days+35],
                                      selectPrice[5,days+35],selectPrice[6,days+35],selectPrice[7,days+35],selectPrice[8,days+35],
                                      1/3,0.5,0.5,0.5,0.5,1/4,0.75,1/4,0.75)
}
## 10th spot rate
for (days in 1:10) {
  selectPrice[10,days+35] <- spotRate9(selectPrice[10,days+15],selectPrice$Coupon[10]*100/2,
                                      selectPrice[1,days+35],selectPrice[2,days+35],selectPrice[3,days+35],selectPrice[4,days+35],
                                      selectPrice[5,days+35],selectPrice[6,days+35],selectPrice[7,days+35],selectPrice[8,days+35],
                                      selectPrice[9,days+35],
                                      1/3,0.5,0.5,0.5,0.5,1/4,0.75,1/4,0.75,0.5)
}
## 11th spot rate
for (days in 1:10) {
  selectPrice[11,days+35] <- spotRate10(selectPrice[11,days+15],selectPrice$Coupon[11]*100/2,
                                       selectPrice[1,days+35],selectPrice[2,days+35],selectPrice[3,days+35],selectPrice[4,days+35],
                                       selectPrice[5,days+35],selectPrice[6,days+35],selectPrice[7,days+35],selectPrice[8,days+35],
                                       selectPrice[9,days+35],selectPrice[10,days+35],
                                       1/3,0.5,0.5,0.5,0.5,1/4,0.75,1/4,0.75,0.5,0.5)
}

##Begin to draw curve for spot curve
xdata2 <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5)
plot(xdata2,selectPrice$spot115 , type="o", col="blue",
     xlab = "Year to Maturity",ylab = "Spot Rate",main = "Spot Curve")
lines(xdata2, selectPrice$spot114, col="red",lty=1)
lines(xdata2, selectPrice$spot113, col="chartreuse",lty=1)
lines(xdata2, selectPrice$spot110, col="brown",lty=1)
lines(xdata2, selectPrice$spot109, col="darkorchid",lty=1)
lines(xdata2, selectPrice$spot108, col="cyan",lty=1)
lines(xdata2, selectPrice$spot107, col="darkred",lty=1)
lines(xdata2, selectPrice$spot106, col="darkgreen",lty=1)
lines(xdata2, selectPrice$spot103, col="darkorange",lty=1)
lines(xdata2, selectPrice$spot102, col="darkgoldenrod",lty=1)
legend("topleft",legend=c("spot115","spot114","spot113","spot110","spot109","spot108",
                           "spot107","spot106","spot103","spot102"),
       text.col=c("blue","red","chartreuse","brown","darkorchid","cyan","darkred",
                  "darkgreen","darkorange","darkgoldenrod"),ncol=3, cex=0.5)

### Calculate the forward curve
forwardTable <- matrix(rep(0,40),ncol=10,byrow=TRUE)
for (i in 1:4) {
    forwardTable[i,1]<-((1+selectPrice$spot115[1+2*i])^(i+1)/(1+selectPrice$spot115[1]))^(1/i)-1
}
for (i in 1:4) {
  forwardTable[i,2]<-((1+selectPrice$spot114[1+2*i])^(i+1)/(1+selectPrice$spot114[1]))^(1/i)-1
}
for (i in 1:4) {
  forwardTable[i,3]<-((1+selectPrice$spot113[1+2*i])^(i+1)/(1+selectPrice$spot113[1]))^(1/i)-1
}
for (i in 1:4) {
  forwardTable[i,4]<-((1+selectPrice$spot110[1+2*i])^(i+1)/(1+selectPrice$spot110[1]))^(1/i)-1
}
for (i in 1:4) {
  forwardTable[i,5]<-((1+selectPrice$spot109[1+2*i])^(i+1)/(1+selectPrice$spot109[1]))^(1/i)-1
}
for (i in 1:4) {
  forwardTable[i,6]<-((1+selectPrice$spot108[1+2*i])^(i+1)/(1+selectPrice$spot108[1]))^(1/i)-1
}
for (i in 1:4) {
  forwardTable[i,7]<-((1+selectPrice$spot107[1+2*i])^(i+1)/(1+selectPrice$spot107[1]))^(1/i)-1
}
for (i in 1:4) {
  forwardTable[i,8]<-((1+selectPrice$spot106[1+2*i])^(i+1)/(1+selectPrice$spot106[1]))^(1/i)-1
}
for (i in 1:4) {
  forwardTable[i,9]<-((1+selectPrice$spot103[1+2*i])^(i+1)/(1+selectPrice$spot106[1]))^(1/i)-1
}
for (i in 1:4) {
  forwardTable[i,10]<-((1+selectPrice$spot102[1+2*i])^(i+1)/(1+selectPrice$spot106[1]))^(1/i)-1
}

##Begin to draw curve for forward
xdata3 <- c(1,2,3,4)
plot(xdata3,forwardTable[,10] , type="o", col="blue",
     xlab = "Years ",ylab = "Forward rate",main = "Forward Curve")
lines(xdata3, forwardTable[,1], col="red",lty=1)
lines(xdata3, forwardTable[,2], col="chartreuse",lty=1)
lines(xdata3, forwardTable[,3], col="brown",lty=1)
lines(xdata3, forwardTable[,4], col="darkorchid",lty=1)
lines(xdata3, forwardTable[,5], col="cyan",lty=1)
lines(xdata3, forwardTable[,6], col="darkred",lty=1)
lines(xdata3, forwardTable[,7], col="darkgreen",lty=1)
lines(xdata3, forwardTable[,8], col="darkorange",lty=1)
lines(xdata3, forwardTable[,9], col="darkgoldenrod",lty=1)
legend("topleft",legend=c("forward102","forward115","forward114","forward113","forward110","forward109",
                          "forward108","forward107","forward106","forward103"),
       text.col=c("blue","red","chartreuse","brown","darkorchid","cyan","darkred",
                  "darkgreen","darkorange","darkgoldenrod"), ncol=3,cex=0.55)



###Calculate covariance matrices for the time series of daily log-returns of yield 
X <- matrix(rep(0,45), nrow = 9, ncol = 5)
for (i in 1:9){
  X[i,1]<-log(selectPrice[3,35-i]/selectPrice[3,36-i])
}
for (i in 1:9){
  X[i,2]<-log(selectPrice[5,35-i]/selectPrice[5,36-i])
}
for (i in 1:9){
  X[i,3]<-log(selectPrice[7,35-i]/selectPrice[7,36-i])
}
for (i in 1:9){
  X[i,4]<-log(selectPrice[9,35-i]/selectPrice[9,36-i])
}
for (i in 1:9){
  X[i,5]<-log(selectPrice[9,35-i]/selectPrice[11,36-i])
}
Xfinal <- cov(X)

###Calculate covariance matrices for the forward rates
finalForward <- as.data.frame(t(forwardTable))
finalForwardM <-finalForward[order(nrow(finalForward):1),]
Yfinal <- cov(finalForwardM)

##Eigenvalues
logEigen<-eigen(Xfinal)$values
logVector<-eigen(Xfinal)$vectors
forwardEigen<-eigen(Yfinal)$values
forwardVector<-eigen(Yfinal)$vectors

