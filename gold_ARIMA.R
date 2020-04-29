df<- read.csv("XAUUSD_Candlestick_1_D_BID_05.05.2003-31.12.2019_all.csv", header = TRUE)
dfsw<- read.csv("XAUUSD_Candlestick_1_D_BID_05.05.2003-31.12.2019_wkdays.csv", header = TRUE)

library(lubridate)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(MASS)
library(tseries) 
library(CADFtest) 
library(quantmod)
library(car)
library(forecast)  
library(lmtest) 
library(tseries)
library(Metrics) 
library(urca) 

#Defining date as date
df$Gmt.time<- dmy_hms(df$Gmt.time)
dfsw$Gmt.time<- dmy_hms(dfsw$Gmt.time)

#trimming data
keep<- c("Gmt.time", "Close")

dfsw_insamp<- dfsw[keep] %>%
  filter(dfsw$Gmt.time <= as.POSIXct("2019-01-31")) #in sample data for SW test (n<5000, null value weekends filtered)

df_insamp<- df[keep] %>%
  filter(df$Gmt.time <= as.POSIXct("2019-01-31")) #in sample data

df_outsamp<- df[keep] %>%
  filter(df$Gmt.time > as.POSIXct("2019-01-31")) #out of sample data

## Gold close price time series with trendline
plot(df$Gmt.time, df$Close, type = "l", main = "Figure 1. Gold Close Price Time-Series", xlab = "Period", 
     ylab = "Close Price/$ per ounce", ylim = c(0, max(df$Close)*1.2))
abline(reg=lm(df$Close~df$Gmt.time), col="blue", lty=5)

#Density Distribution of Gold Close Price
range<- range(df$Close)
ggplot(df, aes(x=Close, xmin=0)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=round(diff(range,1)/50),
                 colour="black", fill="white",
                 ) +
  geom_density(alpha=.2, fill="#FF6666") + # Overlay with transparent density plot
  ggtitle("Figure 2. Density Distribution of Gold Close Price") + 
  xlab("Gold Close Price/$ per ounce") + 
  ylab("Density")


## QQ Plot of in sample data
ggqqplot(df_insamp$Close, conf.int = TRUE, title = "Figure 3. In-Sample Gold Data QQ Plot", 
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")

##In sample SW Test
shapiro.test(dfsw_insamp$Close)

# Shapiro-Wilk normality test
# 
# data:  dfsw_insamp$Close
# W = 0.93746, p-value < 2.2e-16


#Insample ADF test with AIC & BIC criterion
#AIC
adfnonea<-CADFtest(df_insamp$Close, type = c("none"), max.lag.Y = 10, criterion = ("AIC"))
adftrenda<-CADFtest(df_insamp$Close, type = c("trend"), max.lag.Y = 10, criterion = ("AIC"))
adfdrifta<-CADFtest(df_insamp$Close, type = c("drift"), max.lag.Y = 10, criterion = ("AIC"))

#BIC
adfnoneb<-CADFtest(df_insamp$Close, type = c("none"), max.lag.Y = 10, criterion = ("BIC"))
adftrendb<-CADFtest(df_insamp$Close, type = c("trend"), max.lag.Y = 10, criterion = ("BIC"))
adfdriftb<-CADFtest(df_insamp$Close, type = c("drift"), max.lag.Y = 10, criterion = ("BIC"))


#Log differences of data
df_logs<- head(df_insamp, -1)
df_insamp$log<- log(df_insamp$Close, base = 10)
df_logs$diff<- diff(df_insamp$log, 1)


#Density Distribution of Log Diff
range<- range(df_logs$diff)
ggplot(df_logs, aes(x=diff, xmin=0)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density on y-axis
                 binwidth = round(diff(range,1)/50,5),
                 colour="black", fill="white",
  ) +
  geom_density(alpha=.2, fill="#FF6666") + # Overlay with transparent density plot
  ggtitle("Figure 4. Density Distribution of Logged Differences (lag = 1)") + 
  xlab("Log(10) Gold Price") + 
  ylab("Density")


## qq plot of logged data
ggqqplot(df_logs$diff, conf.int = TRUE, title = "Figure 5. Logged Data QQ Plot (lag = 1)", 
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")


## Comparitive QQ Plots to determine what curve of qq plot for logged data is expected
lm<- lm(df_logs$diff ~ df_logs$Gmt.time, data = df_logs)$residual
i<- length(lm)
xi<- cbind(matrix(rnorm(12*i),nr=i),lm,matrix(rnorm(12*i),nr=i))
colnames(xi) = c(letters[1:12],"Z",letters[13:24])

opar = par()
par(mfrow=c(5,5));
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(1,1,1,1));

ytpos = (apply(xi,2,min)+3*apply(xi,2,max))/4
cn = colnames(xi)

for(n in 1:25) {
  qqnorm(xi[,n],axes=FALSE,ylab= colnames(xi)[n],xlab="",main="")
  qqline(xi[,n],col=2,lty=2)
  box("figure", col="darkgreen")
  text(-1.5,ytpos[n],cn[n])
}
par(opar)


##SW Test of log diffs
dfsw_logs<- head(dfsw_insamp, -1)
dfsw_insamp$log<- log(dfsw_insamp$Close, base = 10)
dfsw_logs$diff<- diff(dfsw_insamp$log, 1)
shapiro.test(dfsw_logs$diff)

# Shapiro-Wilk normality test
# 
# data:  dfsw_logs$diff
# W = 0.9183, p-value < 2.2e-16


#Yeo-Johnson Transformation ("Figures 7-11")
lm<- lm(dfsw_insamp$Close ~ dfsw_insamp$Gmt.time, data = dfsw_insamp) #create linear model
plot(lm) #check attributes of lm pre-transformation

yj<- boxCox(lm, family="yjPower", plotit = TRUE, lambda = seq(-2,2))
lam.max<- round(yj$x[which(yj$y==max(yj$y))]) #identify best lambda #set best lambda

dfsw_insamp$transf<- yjPower(dfsw_insamp$Close, lambda = lam.max) #apply transformation
lm2<- lm(dfsw_insamp$transf ~ dfsw_insamp$Gmt.time, data = dfsw_insamp) #create new lm
plot(lm2) #check attributes of lm post-transformation


##SW Test 2
shapiro.test(dfsw_insamp$transf) #verify SW of transformed data to determine use of YJ transformation

# Shapiro-Wilk normality test
# 
# data:  dfsw_insamp$transf
# W = 0.88033, p-value < 2.2e-16


#Density Distribution of YJ
range<- range(dfsw_insamp$transf)
ggplot(dfsw_insamp, aes(x=transf, xmin=5)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density on y-axis
                 binwidth = round(diff(range,1)/50,5),
                 colour="black", fill="white",
  ) +
  geom_density(alpha=.2, fill="#FF6666") + # Overlay with transparent density plot
  ggtitle("Figure 18. Density Distribution of YJ Transformed Data") + 
  xlab("Log(10) Gold Price") + 
  ylab("Density")


##Investigate example distribution
set.seed(123)  # for reproducibility
x<- rnorm(nrow(dfsw_insamp), 100, 10)
y<- x[x < 125] 
shapiro.test(y)

#SW test for confirmed normal distribution of [sample size] = [sample data] returns similar result, indicating 
#tests' over sensitivity to sample size


hist(y, prob=T, col="skyblue2", xlim=c(50, 150), main = "Figure x. Density Distribution of Test Data")
curve(dnorm(x, 100, 10), add=T, lwd=2, col="red") #visual inspection/confirmation of normality


ggqqplot(y, conf.int = TRUE, title = "Figure x. Sample Data QQ Plot", 
         xlab = "Theoretical Quantiles", ylab = "Sample Quantiles") #visual inspection/confirmation of normality


#ADF test YJ
adfnonea_yj<-CADFtest(dfsw_insamp$transf, type = c("none"), max.lag.Y = 10, criterion = ("AIC"))
adftrenda_yj<-CADFtest(dfsw_insamp$transf, type = c("trend"), max.lag.Y = 10, criterion = ("AIC"))
adfdrifta_yj<-CADFtest(dfsw_insamp$transf, type = c("drift"), max.lag.Y = 10, criterion = ("AIC"))

adfnoneb_yj<-CADFtest(dfsw_insamp$transf, type = c("none"), max.lag.Y = 10, criterion = ("BIC"))
adftrendb_yj<-CADFtest(dfsw_insamp$transf, type = c("trend"), max.lag.Y = 10, criterion = ("BIC"))
adfdriftb_yj<-CADFtest(dfsw_insamp$transf, type = c("drift"), max.lag.Y = 10, criterion = ("BIC"))


#ADF test Diffed
adfnonea_diff<-CADFtest(df_logs$diff, type = c("none"), max.lag.Y = 10, criterion = ("AIC"))
adftrenda_diff<-CADFtest(df_logs$diff, type = c("trend"), max.lag.Y = 10, criterion = ("AIC"))
adfdrifta_diff<-CADFtest(df_logs$diff, type = c("drift"), max.lag.Y = 10, criterion = ("AIC"))

adfnoneb_diff<-CADFtest(df_logs$diff, type = c("none"), max.lag.Y = 10, criterion = ("BIC"))
adftrendb_diff<-CADFtest(df_logs$diff, type = c("trend"), max.lag.Y = 10, criterion = ("BIC"))
adfdriftb_diff<-CADFtest(df_logs$diff, type = c("drift"), max.lag.Y = 10, criterion = ("BIC"))


##ADF results infer utilisation of log diffed data

#ACF & PACF
par(mfrow=c(1,2))
acf(df_logs$diff,10, main = "ACF") #AR = p = 0 or 4 or 5
pacf(df_logs$diff,10, main = "PACF") #MA = q = 0 or 5 or 10
mtext("Figure 14. ACF & PACF of Log Difference In Sample Data", side = 3, line = -1, outer = TRUE)


#ARIMA
dfinsamp_ts<- ts(df_insamp$Close, start = c(2003, 125), frequency = 365)

#test various ARIMA models and compare results
arima010<- arima(dfinsamp_ts, order = c(0,1,0), method = "ML")
arima015<- arima(dfinsamp_ts, order = c(0,1,5), method = "ML")
arima0110<- arima(dfinsamp_ts, order = c(0,1,10), method = "ML")
arima510<- arima(dfinsamp_ts, order = c(5,1,0), method = "ML")
arima810<- arima(dfinsamp_ts, order = c(8,1,0), method = "ML")
arima1010<- arima(dfinsamp_ts, order = c(10,1,0), method = "ML")
arima415<- arima(dfinsamp_ts, order = c(4,1,5), method = "ML", optim.control = list(maxit = 600))
arima4110<- arima(dfinsamp_ts, order = c(4,1,10), method = "ML", optim.control = list(maxit = 600))
arima515<- arima(dfinsamp_ts, order = c(5,1,5), method = "ML")
arima5110<- arima(dfinsamp_ts, order = c(5,1,10), method = "ML")
arima815<- arima(dfinsamp_ts, order = c(8,1,5), method = "ML", optim.control = list(maxit = 600))
arima8110<- arima(dfinsamp_ts, order = c(8,1,10), method = "ML", optim.control = list(maxit = 600))
arima1015<- arima(dfinsamp_ts, order = c(10,1,5), method = "ML", optim.control = list(maxit = 600))
arima10110<- arima(dfinsamp_ts, order = c(10,1,10), method = "ML", optim.control = list(maxit = 600))
                 

#ARIMA(0,1,0) statistics
summary(arima010)


#ARIMA(0,1,0) Residuals
tsdisplay(arima010$residuals, main = "Figure 15. ARIMA(0,1,0) Model Residuals", lag.max = 25)

#ARIMA(4,1,5) Residuals
tsdisplay(arima415$residuals, main = "Figure 16. ARIMA(4,1,5) Model Residuals", lag.max = 25)


#ARIMA(4,1,5) statistics
summary(arima415)
coeftest(arima415)
coefci(arima415)


#ARIMA forecast
arima_forecast<- forecast(arima415, h=334, level=c(70, 95))

plot(arima_forecast, main = "Figure 17. ARIMA(0,1,0) Forecast", 
     xlab = "Period", ylab = "Gold Price /$ per ounce")

#RW
set.seed(1234)
rwlen<- nrow(df_insamp)

rwf<- rwf(dfinsamp_ts, h = 334, level = c(70, 95), drift = FALSE)
autoplot(rwf)

#Compare models
summary(rwf)

df_outsamp$rwcompare<-rwf$mean-df_outsamp$Close
df_outsamp$arimacompare<-arima_forecast$mean-df_outsamp$Close

df_compare<- data.frame(df_outsamp$Gmt.time,df_outsamp$rwcompare,df_outsamp$arimacompare)

