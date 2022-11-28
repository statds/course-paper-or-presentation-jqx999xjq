library(astsa)
library(ggplot2)
#hpi data import
hpi = read.csv("C:/Users/Jerry Xu/Desktop/ATS_PROJECT/CTSTHPI.csv")
hpi.ts = ts(hpi$CTSTHPI,start = 1975,frequency = 4)

hpi.ts = na.omit(hpi.ts)
length(hpi.ts)
hpi.ts.cali = hpi.ts[1:180]
hpi.ts.hold = hpi.ts[181:190]

ts.plot(hpi.ts,ylab = "HPI(House Price Index)",main = "HPI Time Series from 1975 to 2022",lwd = 2)

#raw data ACF
acf(as.numeric(hpi.ts.cali),lag.max = 60, main = "ACF Plot for The First 60 Quar
    ters")
#ACFs Plot
par(mfrow = c(3,1))
acf(as.numeric(hpi.ts.cali),lag.max = 60, main = "ACF Plot for The First 60 Quarters")
acf(as.numeric(hpigr),lag.max = 60,main = "ACF Plot for The First 60 Quarter (Order = 1)")
acf(as.numeric(hpi.df2),lag.max = 60, main = "ACF Plot for The First 60 Quarter (Order = 2)")#Identifying MA(5)model

#transformation
#first order difference
hpigr = diff(log(hpi.ts.cali))
hpi.df1 = diff(hpi.ts.cali)
ts.plot(as.numeric(hpi.ts.cali))
acf(as.numeric(hpigr),lag.max = 60,main = "ACF Plot for The First 60 Quarter (Or
    der = 1)")
pacf(as.numeric(hpigr),lag.max = 60,main = "PACF Plot for The First 60 Quarter 
     (Order = 1)")

#Fit AR model based on AIC
ar(hpigr)
#Stationary Test
library(fUnitRoots)
adfTest(hpigr, lags=3, type = "ct")
adfTest(hpigr, lags=6, type = "ct")
adfTest(hpigr, lags = 10, type = "ct")

adfTest(hpigr, lags=3, type = "c")
adfTest(hpigr, lags=6, type = "c")
adfTest(hpigr, lags=10, type = "c")

adfTest(hpigr, lags=3, type = "nc")#%fit diff(log(hpi))with AR(3)model with no intercept is adequate#
adfTest(hpigr, lags=6, type = "nc")
adfTest(hpigr, lags=10, type = "nc")

#%Suppose we choose d = 2 

#second order difference
hpi.df2 = diff(as.numeric(hpi.df1))
ts.plot(as.numeric(hpi.df2),ylab = "Second Order Differenc of HPI",main = "Secon
        d Order Differenc of Calibration Part HPI",lwd = 2)
grid()
acf(as.numeric(hpi.df2),lag.max = 60, main = "ACF Plot for The First 60 Quarter 
    (Order = 2)")#Identifying MA(5)model
pacf(as.numeric(hpi.df2),lag.max = 60, main = "PACF Plot for The First 60 Quarte
     rs")# Identifying AR(2)model

#Fit AR model based on AIC
ar(hpi.df2)
#Stationary Test
adfTest(hpi.df2, lags=2, type = "c")
adfTest(hpi.df2, lags=2, type = "ct")
adfTest(hpi.df2, lags = 2, type = "nc")

#Choose the best model based on AIC when d = 2
d=2
np=2
nq=5 
outarima.all <- matrix(nrow=(np+1)*(nq+1), ncol=3)
colnames(outarima.all) = c("p", "q", "BIC")
for (p in 0:np){
  for (q in 0:nq) {
    hpi.df2.fit = sarima(hpi.df2,p,d,q,details = FALSE)
    outarima.all[p*(nq+1)+q+1,] <- c(p,q,hpi.df2.fit$BIC)
  }
}
outarima.all
# p = 2, d = 2, q = 2 model is chosen

#Estimation
library(astsa)

#Conditional Least Squared Estimation
hpi.df2.ar2ma2.css = arima(hpi.df2,order=c(2,2,2), method='CSS',
                       include.mean=TRUE)
hpi.df2.ar2ma2.css

#Diagnostic Checking
hpi.df2.ar2ma2.diag=sarima(hpi.df2, p=2, d=2, q=2, 
                           no.constant=FALSE,details=TRUE)

#in-sampling prediction
par(mfrow = c(2,1))
hpi.df2.ar2ma2.fore = predict(hpi.df2.ar2ma2.css,n.ahead=10)
hpi.df2.ar2ma2.fore$pred
hpi.df2.ar2ma2.fore$se

U = hpi.df2.ar2ma2.fore$pred + hpi.df2.ar2ma2.fore$se
L = hpi.df2.ar2ma2.fore$pred - hpi.df2.ar2ma2.fore$se

hpi.all.df2 = diff(diff(as.numeric(hpi.ts)))
plot(as.numeric(hpi.all.df2), type="o",xlab ="Time", ylab="HPI_Order_2",xlim = c(90,200),main = "In-Sample HPI Prediction (Order = 2)")
lines(hpi.df2.ar2ma2.fore$pred, col="red",type = "o")
lines(U,col="grey",lwd = 1)
lines(L,col="grey",lwd = 1)
abline(v=179,lty = "dashed",col = "grey")
grid()
#out-of-sample prediction
HPI_Order_2<-hpi.all.df2
hpi.ts.cali.fore=sarima.for(HPI_Order_2,n.ahead=10,2,2,2,main = "Out-of-Sample HPI Prediction (Order = 2)")

#Convert prediction result into real HPI value
#in-sample prediction result
par(mfrow = c(2,1))
HPI<-hpi.ts.cali
isp.fore = sarima.for(HPI,n.ahead=10,2,2,2,main = "In-Sample HPI Prediction")
lines(as.numeric(hpi.ts),type = "o")
#out-of-sample prediction result
HPI<-as.numeric(hpi.ts)
osp.fore = sarima.for(HPI,n.ahead=10,2,2,2,main = "Out-of-Sample HPI Prediction")
help("sarima")

###Construct ARMAX model###


#plot explanatory variables
pcpi<-read.csv("C:/Users/Jerry Xu/Desktop/ATS_PROJECT/CTPCPI.csv")
pop<-read.csv("C:/Users/Jerry Xu/Desktop/ATS_PROJECT/CTPOP.csv")
rate<-read.csv("C:/Users/Jerry Xu/Desktop/ATS_PROJECT/INTRATREARAT10Y.csv")
pcpi<-pcpi[,2]
pop.am<-pop[,2]

#Aggregate monthly data to yearly data.
library(lubridate)
library(dplyr)
rate.md = data.frame(date = dmy(rate$DATE),Rate = rate$REAINTRATREARAT10Y)
head(rate.md,n = 20)

rate.md$year = floor_date(rate.md$date,"year")
rate.mds= rate.md %>% group_by(year)%>% summarize(mean = mean(Rate))
print(rate.mds,n = 41)

hpi.md = data.frame(date = dmy(hpi$DATE),HPI = hpi$CTSTHPI)
head(hpi.md,n = 20)

hpi.md$year = floor_date(rate.md$date,"quarter")
rate.mds= rate.md %>% group_by(year)%>% summarize(mean = mean(Rate))
print(rate.mds,n = 41)

#preprocessing explanatory variables
hpi<-tapply(hpi.ts, floor(time(hpi.ts)), mean)[8:48]
hpi1<-data.frame(hpi)
hpi2<-hpi1
income<-pcpi[1:41]/10000
population<-pop.am[82:122]/1000000
rate<-rate.mds$mean


par(mfrow=c(2,2))
ts.plot(data = hpi2,ylab = "HPI",main = "House Price Index")
ts.plot(data = income,col = "orange",ylab = "PCPI (Unit = 10K U.S. Dollar)",main = "Pre Capita Personal Income")
ts.plot(data = population,col = "blue",ylab = "PGR (Unit = million)",main = "Population in Connecticut")
ts.plot(data = rate,ylab = "RATE",col = "red",main = "10-Year Real Interest Rate")
all = cbind(hpi2,income)
par(mfrow = c(1,1))
ts.plot(all,col = c(1,2,4))

pairs(cbind(hpi2,income,population,rate))
cor(cbind(hpi2,income,population,rate))#con linearity if found

hpi= hpi2$hpi
c.hpi2 = (hpi-mean(hpi))/sd(hpi)
c.income = (income-mean(income))/sd(income)
c.population = (population-mean(population))/sd(population)
c.rate = (rate-mean(rate))/sd(rate)

all = data.frame(c.hpi2,c.income,c.population,c.rate)
regfit = lm(c.hpi2~c.income+c.population+c.rate,na.action = NULL)#linear model
summary(regfit)

resid = resid(regfit)
par(mfrow = c(1,2))
acf(resid,lag.max = 30,main = "ACF of Regression Resids")
pacf(resid,lag.max = 30,main = "PACF of Regression Resids")

#ARIMAX automatic model selection
#Choose the best model based on BIC when d = 2
d=2
np=1
nq=3 
outarimax.all <- matrix(nrow=(np+1)*(nq+1), ncol=3)
colnames(outarimax.all) = c("p", "q", "BIC")
for (p in 0:np){
  for (q in 0:nq) {
    armaxfit = sarima(c.all[,1], p=p,d=d,q=q, xreg=c.all[,2:4],
                          no.constant = FALSE, details = FALSE)
    outarimax.all[p*(nq+1)+q+1,] <- c(p,q,armaxfit$BIC)
  }
}
outarimax.all

#ARMAX model joint estimation
#checking model adequacy
library(astsa)
armaxfitchk = sarima(c.all[,1], p=1,d=2,q=2, xreg=c.all[,2:4],
                     no.constant = FALSE, details = TRUE)
armaxfitchk

#prediction
#in-sample prediction result
par(mfrow = c(2,1))
HPI_Centralized<-c.all[1:37,1]
arimax.fore1 = sarima.for(HPI_Centralized,n.ahead=4,p=1,d=2,q=2, xreg =c.all[1:37,2:4],newxreg=c.all[38:41,2:4],
                         no.constant = FALSE,main = "In-Sample HPI Prediction")
lines(as.numeric(c.all[1:41,1]),type = "o")

#out-of-sample prediction result
##It’s expected that rates will continue to increase in 2023. The federal funds rate is 
##projected to range from 3.9% to 4.9% in 2023, according to the Federal Reserve
##If Connecticut continues to grow these rates, it is estimated that the population will 
##exceed 3.7 million in 2020, and approach 3.75 million by 2025.
rate.new = c(3.0,3.2,3.4,3.6)
pop.new = c(3.610,3.611,3.612,3.613)
pcpi.new = c(84000,86000,88000,90000)

rate.new.c = rate.all.c[42:45]
pop.new.c = pop.all.c[42:45]
pcpi.new.c = pcpi.all.c[42:45]

all.new.c<-data.frame(rate.new.c,pop.new.c,pcpi.new.c)
HPI_Centralized<-c.all[1:41,1]

arimax.fore2 = sarima.for(HPI_Centralized,n.ahead=4,p=1,d=2,q=2, xreg =c.all[1:41,2:4],newxreg=all.new.c,
                          no.constant = FALSE,main = "Out-of-Sample HPI Prediction")
hpi
