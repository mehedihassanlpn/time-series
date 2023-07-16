# time-series
#boxcox transformation if variance non-stationary
data()
setwd('C:\Users\Mehedi Hassan\Documents\R')
library(MASS)
data("AirPassengers")
AirPassengers
plot(AirPassengers)
y<-AirPassengers
y1<- log(AirPassengers)
plot(y1)
plot(y)
y2<- diff(y1)
plot(y2)
abline(lm(y2~time(y2)))
par(mfrow=c(2,1))
acf(y2)
pacf(y2)
# q=1 and p=0 and d=1


# DATA: Nile-------------
data(Nile)
Nile
plot.ts(Nile)
abline(lm(Nile~time(Nile)))
library(MASS)
boxcox(Nile~1) # if the boundary includes 1 transformation no need
library(tseries)
adf.test(Nile)
kpss.test(Nile)
plot.ts(diff(Nile))
adf.test(diff(Nile))
y1<- diff(Nile)
plot(y1)

par(mfrow=c(2,1))
acf(y1)
pacf(y1)
library(forecast)
Arima(Nile,order=c(0,1,1))$aicc
Arima(Nile,order=c(0,1,2))$aicc
Arima(Nile,order=c(1,1,2))$aicc

Fit<-Arima(Nile, order=c(0,1,2))
Fit
summary(Fit)
e<- resid(Fit)
e
tsdisplay(e)
qqnorm(e)
qqline(e)

Box.test(e, type = "Ljung")
install.packages("TSA")
ts.plot(e)
abline(h=0)
summary(Fit)
plot(forecast(Fit,))





# x.bar<- numeric(1000)
# for (i in 1:1000) {
# x<-rnorm(5)
# x.bar[i]<-mean(x)
# }
# hist(x.bar)
# 
# for(i in 1:100){
# x<-rchisq(50,1)
# x.bar[i]<-mean(x)}
# hist(x.bar)
# shapiro.test(x.bar)



# library(STA529L)
# Question()
# Mydata(09)

######################### DATA: Bev-------------
library(tseries)
data(bev)
y<-bev
y
summary(y)
# Checking variance stationary?----------------
plot.ts(y)
#variance not stationary, then boxcox transformation -------------
library(MASS)
boxcox(y~1)
y1<-y^.4
boxcox(y1~1)
plot.ts(y1)
#here from plot it is mean non-stationary
# if the boundary includes 1 transformation no need#
library(tseries)
# ADF test: checking mean stationary. null hypothesis:mean not stationary #
adf.test(y1)
## KPSS test: checking mean stationary. null hypothesis:mean not stationary #
kpss.test(y) # Here gives contradictory reults#
# For mean stationary we take difference#
y2<- diff(y1)
plot.ts(y2)
abline(h=0) # here mean stationry condition satisfied#
adf.test(y2) # here mean also stationry condition satisfied#
kpss.test(y1)
library(forecast)
# See the acf, pacf for selecting the order

tsdisplay(y2)#here acf cuts off after lag value 1 and pacf tails off(decreasing), so it is MA(1) process, p=0, q=1,d=1#
# Probable model ARIMA(0,1,1)
# Taking probable others model#
library(forecast)
Arima(bev,order = c(0,1,3))$bic # Give minimum bic#
Arima(bev,order = c(0,1,2))$bic
Arima(bev,order = c(1,1,3))$bic
Arima(bev,order = c(1,1,0))$bic
Arima(bev,order = c(1,1,2))$bic

Sharif <-Arima(bev,order = c(0,1,3))
Sharif
summary(Sharif)
e<-resid(Sharif)
e
hist(e)
shapiro.test(e)
qqnorm(e)
qqline(e) # e is not normal as p-value < .05
# Check acf, pacf of resiudals and portmanteau test to see whether white noise#
# If white noise then calculate forecast#
tsdisplay(e) #since acf, pacf inside the boundary it's follow white noise process#
#If they do not look like white noise, try a modified model#
e1<-e^2
tsdisplay(e1)# follw iid---------#
Box.test(e1, type="Ljung")# H0:residuals are iid
# install.packages("TSA")
# library(TSA)
# McLeod.Li.test(y=e1)
# plot(forecast(Fit))

library(tseries)


fit1<-garch(e,order = c(0,3))
fit2<-garch(e,order = c(0,2))
fit3<-garch(e,order = c(0,1))
fit4<-garch(e,order = c(1,3))
fit5<-garch(e,order = c(1,2))

AIC(fit1,fit2,fit3,fit4,fit5)

summary(fit1)




library(TSA)
plot(forecast(Sharif))
forecast(Sharif,5)
# yearly data, so gives forecast 30 years#
# by default 10 years#


library(tseries)
adf.test(Nile)
kpss.test(Nile)
plot.ts(diff(Nile))
adf.test(diff(Nile))
y1<- diff(Nile)
plot(y1)

par(mfrow=c(2,1))
acf(y1)
pacf(y1)
library(forecast)
Arima(Nile,order=c(0,1,1))$aicc
Arima(Nile,order=c(0,1,2))$aicc
Arima(Nile,order=c(1,1,2))$aicc

Fit<-Arima(Nile, order=c(0,1,2))
e<- resid(Fit)
tsdisplay(e)
qqnorm(e)
qqline(e)

Box.test(e, type = "Ljung")
install.packages("TSA")
ts.plot(e)
abline(h=0)
summary(Fit)
plot(forecast(Fit))

# 17.04.18-------------------------------

data("AirPassengers")
y<-AirPassengers
plot.ts(y)
library(MASS)
boxcox(y~1)
plot.ts(log(y))
y1<-log(y)
library(tseries)
adf.test(y1)
kpss.test(y1)
ys<-diff(y1,12)
plot.ts(ys)
ysd<-diff(ys)
plot.ts(ysd)
abline(h=0)
adf.test(ysd)
library(forecast)
tsdisplay(ysd,main = "")
Arima(AirPassengers, order=c(1,1,1), seasonal=list(order=c(0,1,1),period=12), lambda=0)$bic
Arima(AirPassengers, order=c(1,1,0), seasonal=list(order=c(0,1,1),period=12), lambda=0)$bic
Arima(AirPassengers, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12), lambda=0)$bic
Arima(AirPassengers, order=c(2,1,1), seasonal=list(order=c(0,1,1),period=12), lambda=0)$bic
Arima(AirPassengers, order=c(1,1,2), seasonal=list(order=c(0,1,1),period=12), lambda=0)$bic
Arima(AirPassengers, order=c(1,1,1), seasonal=list(order=c(0,1,2),period=12), lambda=0)$bic
Arima(AirPassengers, order=c(1,1,0), seasonal=list(order=c(0,1,2),period=12), lambda=0)$bic
Arima(AirPassengers, order=c(0,1,1), seasonal=list(order=c(0,1,2),period=12), lambda=0)$bic
Arima(AirPassengers, order=c(2,1,1), seasonal=list(order=c(0,1,2),period=12), lambda=0)$bic
Arima(AirPassengers, order=c(1,1,2), seasonal=list(order=c(0,1,2),period=12), lambda=0)$bic

Fit<-Arima(AirPassengers, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12), lambda=0)
summary(Fit)
plot(forecast(Fit))
res<-resid(Fit)
tsdisplay(res)
Box.test(res)

#data beersales
data("beersales")
y<-beersales
y
summary(y)
# Checking variance stationary?----------------
plot.ts(y)
#variance not stationary, then boxcox transformation -------------
library(MASS)
boxcox(y~1,lambda = seq(-2,3,length=1000))
# here lambd=.15-----
y1<-y^1.5
plot.ts(y1)
# variance stationary from the plot
library(tseries)

y2<-diff(y1,12)# remove seasonal variability#
plot.ts(y2)

#for monthly data,look at lags 12, 24, 36, and so on Judge the ACF and PACF at the seasonal lags in the same way you do for the earlier lags.
a<- rep(0,50)
a[c(12,24,36,48)]=1
cl<- ifelse(a==0,"white", "black")
cl
#See the acf, pacf for selecting the order(P,Q)#
par(mfrow=c(2,1))
acf(y2,50, col=cl)
pacf(y2,50, col=cl)
# Possible P = 1, Q = 0. Here D=1
# For mean stationary we take difference#
y3<-diff(y2)
plot.ts(y3)#mean stationary#
abline(h=0)
# ADF test: checking mean stationary. null hypothesis:mean not stationary #
## KPSS test: checking mean stationary. null hypothesis:mean not stationary #
# kpss.test(y1) # Here gives contradictory reults#
adf.test(y3)
library(forecast)
# See the acf, pacf for selecting the order
tsdisplay(y3)
# Possible p = 1, q = 1. Here d=1

# Search for the possibility of other orders
Arima(beersales, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12),lambda=1.5)$bic
Arima(beersales, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12),lambda=1.5)$bic
Arima(beersales, order=c(0,1,1), seasonal=list(order=c(0,1,2),period=12),lambda=1.5)$bic
Arima(beersales, order=c(1,1,1), seasonal=list(order=c(0,1,1),period=12),lambda=1.5)$bic
Arima(beersales, order=c(1,1,1), seasonal=list(order=c(0,1,2),period=12),lambda=1.5)$bic
# Arima(AirPassengers, order=c(1,1,1), seasonal=list(order=c(0,1,2),period=12),lambda=.15)$bic
# Arima(AirPassengers, order=c(1,1,0), seasonal=list(order=c(0,1,2),period=12),lambda=.15)$bic
# Arima(AirPassengers, order=c(0,1,1), seasonal=list(order=c(0,1,2),period=12),lambda=.15)$bic
# Arima(AirPassengers, order=c(2,1,1), seasonal=list(order=c(0,1,2),period=12),lambda=.15)$bic
# Arima(AirPassengers, order=c(1,1,2), seasonal=list(order=c(0,1,2),period=12),lambda=.15)$bic

Fit1<-Arima(AirPassengers, order=c(1,1,1), seasonal=list(order=c(0,1,2),period=12), lambda=1.5)
summary(Fit1)

# Check acf, pacf of resiudals and portmanteau test to see whether white noise#
# If white noise then calculate forecast#
e<-resid(Fit1)
tsdisplay(e)
qqnorm(e)
qqline(e)
#If they do not look like white noise, try a modified model#
# e1<-e^2
# tsdisplay(e1)
# Box.test(e1, type="Ljung")# H0:residuals are iid
plot(forecast(Fit1))
forecast(Fit1,5)# yearly data, so gives forecast 30 years#



plot.ts(beersales)
y<- beersales
library(MASS)
boxcox(y~1,lambda = seq(0,3,length=100))
y<-beersales
y1<-beersales^1.5
plot(y1)
adf.test(y)
yd<-diff(y1,12)
y2<-diff(yd)
plot(y2)
adf.test(y1)
par(mfrow=c(2,1))
acf(y1)
pacf(y1)
library(forecast)
tsdisplay(y2)



a<- rep(0,50)
a[c(12,24,36,48)]=1
cl<- ifelse(a==0,"white", "black")
cl
acf(y2,50, col=cl)
pacf(y2,50, col=cl)
#30.04.18-----------------------------
data(Nile)
?ses
ses(forecast)
x<-c(2,5,6,7,12,13,8)
y<-c(7,9,6,12,13,15,10)
g<-function(a1,b1) sum(y-a1-b1*x)^2
a1<-seq(-10,20,length=100)
b1<-seq(0,10,length=100)
# SSE<-outer(a1,b1,g)
# persp(a1,b1,SSE)

SSE<-matrix(NA, ncol = 100, nrow = 100)
for (i in 1:100) 
  for (j in 1:100) SSE[i,j]= g(a1[i],b1[j])
persp(a1,b1,SSE)

#08.05.18

e<-rnorm(200)

z<-numeric(200)

z[1]<- e[1] 
for (i in 2 :200){
  z[i]= e[i]*sqrt(1+.5*z[i-1]^2)
}
plot.ts(z)
acf(z)
acf(z^2)

## 09.05.18---------------------------
library(itsmr)
data<- Sunspots
plotc(Sunspots)
y<- ts(Sunspots, start =1770)
plotc(y)
plot(y, col="blue", type="o")
library(MASS)
boxcox(y~1)
summary(y)
y1<-y+1
boxcox(y1~1)
y2<-y1^.25
plotc(y2)
library(tseries)
plotc(y2)
abline(h=0)
adf.test(y2)
par(mfrow=c(2,1))
acf(y2)
pacf(y2)

library(forecast)
Arima(y2,order=c(2,0,0))$aicc
Arima(y2,order=c(2,0,1))$aicc
Arima(y2,order=c(1,0,0))$aicc

Fit<-Arima(y2,order=c(2,0,0))
e<- resid(Fit)
qqnorm(e)
qqline(e)
tsdisplay(e)
e1<-e^2
tsdisplay(e1)
par(mfrow=c(2,1))
acf(e1)
pacf(e1)

#garch(e,order=c(0,1))$aicc
gfit<-garch(e,order = c(0,1))
summary(gfit)

#13.05.18

y<- 2*cos(2)
t=1:96
cos1<-cos(2*pi*t*4/96)
cos2<-cos(2*pi*(t*14/96+.3))
y<-2*cos1+3*cos2
plot(y,type = "o")
cos3=cos(2*pi*(t*24/96))
yt2<-y+4*cos3
plot(yt2,type = "o")
library(TSA)
periodogram(yt2)



data("Nile")
plot(Nile, ylab="Oil (millions of tonnes)",xlab="Year")

fit1 <- ses(Nile, alpha=0.2, initial="simple", h=3)
fit2 <- ses(Nile, alpha=0.6, initial="simple", h=3)
fit3 <- ses(Nile, h=3)
plot(fit1, PI=FALSE, ylab="Oil (millions of tonnes)",
     xlab="Year", main="", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topright",lty=1, col=c(1,"blue","red","green"),
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1)


fitted(fit1)
fitted(fit2)
fitted(fit3)
summary(fit1)
summary(fit2)
summary(fit3)
SSE1<-sum((Nile-fitted(fit1))^2)
SSE1

SSE2<-sum((Nile-fitted(fit2))^2)
SSE2
SSE3<-sum((Nile-fitted(fit3))^2)
SSE3


data("Nile")
fit1 <- holt(Nile, alpha=0.8, beta=0.2, initial="simple", h=5)#linear trend---
fit2 <- holt(Nile, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=5)

plot(fit2, type="o", ylab="Air passengers in Australia (millions)", xlab="Year",
     fcol="white", plot.conf=FALSE)
lines(fitted(fit1), col="blue")
lines(fitted(fit2), col="red")
# lines(fitted(fit3), col="green")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
# lines(fit3$mean, col="green", type="o")
legend("topright", lty=1, col=c("black","blue","red"),
       c("Data","Holt's linear trend","Exponential trend"))

fitted(fit1)
fitted(fit2)
# # Results for first model:
fit1$model$states#value of l & b
fit2$model$states#value of l & b

# fit1$mean
summary(fit1)
summary(fit2)
length(air)

SSE1<-sum((Nile-fitted(fit1))^2)
SSE1

SSE2<-sum((Nile-fitted(fit2))^2)
SSE2
