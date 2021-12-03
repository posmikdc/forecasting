##### Set-Up #####
library(zoo)
library(forecast)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(dplyr)

rm(list=ls())

demand.df <- read.csv("product_demand.csv")
demand.df$Order_Demand <- as.numeric(demand.df$Order_Demand) #Convert Order_Demand to numeric variable
demand.df$Date <- as.Date(demand.df$Date) #Convert Date to Date type variable

##### EDA #####
### Variable Types
summary(demand.df)
str(demand.df)
dim(demand.df)

### Categories
unique(demand.df$Warehouse) #There are 4 distinct values
unique(demand.df$Product_Category) #There are 33 distinct values

### Missing Data in Date Column
sum(is.na(demand.df)) #There is a total of 21,708 missing values in the two numeric variables

missing.values <- demand.df %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 

missing.values %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='Variable', y="number of missing values", title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#There are 11239 Missing values in the Date column, and 10469 missing values in the Order_Demand Column
#Let us eliminate these values, such that
demand.df <- demand.df %>%
  drop_na(Date) %>% #This gets rid of all missing date values
  drop_na(Order_Demand) #This gets rid of all missing Order_Demand values
dim(demand.df)
#*****************************************************************************#
## Aggregate (sum) Demand by Month
demand.df$MonthYear <- format(as.Date(demand.df$Date), "%Y-%m") 
demand.df.bymonth <- aggregate(Order_Demand ~ MonthYear, 
                               data = demand.df, 
                               FUN = sum, 
                               na.rm = TRUE)
demand.df.bymonth

# Plot order demands by Year-Month
ggplot(data=demand.df.bymonth, aes(x=MonthYear, y=Order_Demand, group=1)) +
  geom_line()+
  geom_point()

## Data are missing for 2011's several months, and the data up to November 2011 was
## significantly lower than other data points later. The similar things happened to
## January 2017. Since these outliers are in the first and last periods of in the dataset,
## we will take out these out of our model for forecasting.
demand.df.bymonth <- demand.df.bymonth[c(8:67), ]

## Create time series with the new dataset that has Year-Month and sum of Order Demand
df.ts <- ts(demand.df.bymonth$Order_Demand, start = c(2012,1), frequency = 12)

# Seasonal plot
ggseasonplot(df.ts)

n <- length(df.ts)
# Since the data show seasonality, we want to consider the following state-space 
# model: Seasonal SES (ANA), Additive (AAA), Multiplicative (MAM), and Holt Winters (MMM)
models <- c("ANA","AAA","MAM","MMM") #Model repertoire to evaluate
k <- length(models)

# Consider AIC
# We do not need to any validation/hold-out set and will use the complete available sample
AIC <- array(NA,c(k,1),dimnames=list(models,"AIC"))
for (i in 1:k){
  fit <- ets(df.ts,model=models[i],damped=FALSE)
  AIC[i] <- fit$aic
}
loc_best_AIC <- which(AIC == min(AIC))
paste0("The best model according to AIC is ",models[loc_best_AIC],".")
loc_worst_AIC <- which(AIC == max(AIC))
paste0("The worst model according to AIC is ",models[loc_worst_AIC],".")

### Hold-out Data for Model Selection
## Set the forecast horizon to 1 month
horizon <- 1
# Let us set test data which contains 24 observations.
test <- window(df.ts,start=tail(time(df.ts),24)[1])
m <- length(test)

# Produce forecasts and calculate holdout accuracy (MAE)
mae <- array(NA,c(m-horizon+1,k))
rmse <- array(NA,c(m-horizon+1,k))
for (i in 1:(m-horizon+1)){
  fitsample <- df.ts[1:(n-m+i-1)]
  fitsample <- ts(fitsample,frequency=frequency(df.ts),start=start(df.ts))
  for (j in 1:k){
    fit <- ets(fitsample,model=models[j],damped=FALSE)
    frc <- forecast(fit,h=horizon)$mean
    print(df.ts[(n-m+i):(n-m+i+horizon-1)] - frc)
    mae[i,j] <- mean(abs(df.ts[(n-m+i):(n-m+i+horizon-1)] - frc))
    rmse[i,j] <- sqrt(mean((df.ts[(n-m+i):(n-m+i+horizon-1)] - frc)^2))
  }
}
# ANA, AAA, MAM, MM
MAE <- apply(na.omit(mae),2,mean)
RMSE <- apply(na.omit(rmse),2,mean)
# RMSE <- apply(na.omit(rmse),2,mean)

statespace_error_matrix <- rbind(MAE, RMSE)
statespace_error_matrix

loc_best_MAE <- which(MAE == min(MAE))
paste0("The best model according to holdout MAE is ",models[loc_best_MAE],".")
loc_worst_MAE <- which(MAE == max(MAE))
paste0("The worst model according to holdout MAE is ",models[loc_worst_MAE],".")

#*****************************************************************************#

### K-Term Moving Average One-step Ahead Forecast
ts.estimate<-window(df.ts, end=2014)
ts.test<-window(df.ts, start=2015)
n.in<-length(ts.estimate)
m<-length(ts.test)
n.in;m

ts.estimate<-window(df.ts, end=2014)
i<-2
n.in<-n-m+i-1
n.in

ts.estimate <- window(df.ts, end=2014)
length(ts.estimate)

maorder<-c(5,7)
maorder

frc <- matrix(NA, nrow = m, ncol = 2)
colnames(frc) <- paste("MA", maorder)
rownames(frc) <- time(ts.test)
frc

for (i in 1:m){
  ts.estimate <- df.ts[1:(n-m+i-1)]
  for (j in 1:length(maorder)){
    # The forecast is the last k observations, as prescribed by maorder
    frc[i,j] <- mean(tail(ts.estimate,maorder[j]))
  }
}
frc

frc.ts <- ts(frc, start=start(ts.test), freq = frequency(ts.test) )
head(frc.ts, 5)

plot(df.ts,ylab="Order Demand")
lines(frc.ts[,1], col=2)
lines(frc.ts[,2], col=3)
legend("topright",c("Data",colnames(frc)),col=c("black",2,3),lty=1)

err<-na.omit(matrix(rep(ts.test,2),ncol=2) - frc.ts)
err

RMSE <- sqrt(apply(err^2,2,mean))
MAE <- apply(abs(err),2,mean)
Metrics <- rbind(RMSE,MAE)
Metrics

## Evaluate Model
rw.err <- df.ts[(n-m+1):n] - df.ts[(n-m):(n-1)]
rw.err
rw.MAE <- mean(abs(rw.err))
rw.MAE
RelMAE <- MAE/rw.MAE
RelMAE
rw.err.in <- df.ts[2:(n-m)] - df.ts[1:(n-m)]
rw.err.in
rw.MAE.in <- mean(abs(rw.err.in))
rw.MAE.in
MASE <- MAE/rw.MAE.in
MASE

Metrics <- rbind(RMSE,MAE, RelMAE, MASE)
round(Metrics, 3)

### SES Model
alpha <- c(0.2,0.5,0.8)
alpha
frc.SES <- matrix(NA, nrow = m, ncol = 3)
colnames(frc.SES) <- paste("alpha=", alpha)
rownames(frc.SES) <- time(ts.test)
frc.SES

for (i in 1:m){
  ts.estimate <- df.ts[1:(n-m+i-1)]
  for (j in 1:length(alpha)){
    fit.model <- ets(ts.estimate,model="ANN",alpha=alpha[j])
    fit.model$initstate[1] <- window(df.ts,start=2012,end=2014)
    fit.model <- ets(ts.estimate,model=fit.model,use.initial.values=TRUE)
    
    # Produce the 1-step ahead forecast
    frc.SES[i,j] <- forecast(fit.model,h=1)$mean
  }
}
frc.SES

frc.SES.ts <- ts(frc.SES, start=start(ts.test), freq = frequency(ts.test) )
head(frc.SES.ts, 5)

plot(gdp.ts,ylab="Order Demand")
lines(frc.SES.ts[,1], col=2)
lines(frc.SES.ts[,2], col=3)
lines(frc.SES.ts[,3], col=4)
legend("topright",c("Data",colnames(frc.SES)),col=c("black",2,3,4),lty=1)

## Evaluate Model
err.SES<-na.omit(matrix(rep(ts.test,2),ncol=2) - frc.SES)
err.SES

RMSE.SES <- sqrt(apply(err.SES^2,2,mean))
MAE.SES <- apply(abs(err.SES),2,mean)
Metrics.SES <- rbind(RMSE.SES, MAE.SES)
Metrics.SES

rw.err.SES <- df.ts[(n-m+1):n] - df.ts[(n-m):(n-1)]
rw.err.SES

rw.MAE.SES <- mean(abs(rw.err.SES))
rw.MAE.SES

RelMAE.SES <- MAE.SES/rw.MAE.SES
RelMAE.SES

rw.err.in.SES <- df.ts[2:(n-m)] - df.ts[1:(n-m)]
rw.err.in.SES
rw.MAE.in.SES <- mean(abs(rw.err.in))
rw.MAE.in.SES

MASE.SES <- MAE.SES/rw.MAE.in.SES
MASE.SES

Metrics.SES <- rbind(RMSE.SES, MAE.SES, RelMAE.SES, MASE.SES)
round(Metrics.SES, 3)


#******************************************************************************#

### ARIMA, SARIMA
## Create time series with the new dataset that has Year-Month and sum of Order Demand

y <- df.ts
n <- length(y)
test <- window(y,start=2015)
m <- length(test)
acf(y)
pacf(y)
d1y <- diff(y,1)
plot(d1y)

frc <- array(NA,c(m,1),dimnames=list(time(test),"ARIMA"))
for (i in 1:m){
  fitsample <- y[1:(n-m+i-1)]
  fitsample <- ts(fitsample,frequency=frequency(y),start=start(y))
  # Fit ARIMA
  fit <- auto.arima(fitsample)
  # Produce the 1-step ahead forecast - the function and syntax is the same as for ets
  frc[i,1] <- forecast(fit,h=1)$mean
}

frc <- ts(frc,frequency=frequency(test),start=start(test))

cmp <- brewer.pal(3,"Set1")

plot(y,ylab="GDP change")
lines(frc,col=cmp[1])
legend("topright",c("Data",colnames(frc)),col=c("black",cmp),lty=1)

e <- test - frc

RMSE <- sqrt(apply(e^2,2,mean))
MAE <- apply(abs(e),2,mean)
E <- rbind(RMSE,MAE)
print(round(E,3))

resid <- residuals(fit)
hist(resid)

qqnorm(resid)
qqline(resid)

sfrc <- array(NA,c(m,1),dimnames=list(time(test),"SARIMA"))
for (i in 1:m){
  fitsample <- y[1:(n-m+i-1)]
  fitsample <- ts(fitsample,frequency=frequency(y),start=start(y))
  fit <- auto.sarima(fitsample)
  sfrc[i,1] <- forecast(fit,h=1)$mean
}

sfrc <- ts(sfrc,frequency=frequency(test),start=start(test))

plot(y,ylab="GDP change")
lines(sfrc,col=cmp[1])
legend("topright",c("Data",colnames(sfrc)),col=c("black",cmp),lty=1)

se <- test - sfrc

sRMSE <- sqrt(apply(se^2,2,mean))
sMAE <- apply(abs(se),2,mean)
sE <- rbind(sRMSE,sMAE)
print(round(sE,3))

resid <- residuals(fit)
hist(resid)

qqnorm(resid)
qqline(resid)

#******************************************************************************#

# Produce forecast for test data using ANA model
frc <- array(NA,c(m,1),dimnames=list(time(test),"ANA"))
for (i in 1:m){
  fitsample <- df.ts[1:(n-m+i-1)]
  fitsample <- ts(fitsample,frequency=frequency(df.ts),start=start(df.ts))
  # Fit ANA
  fit <- ets(fitsample,model="ANA",damped=FALSE)
  # Produce the 1-step ahead forecast 
  frc[i,1] <- forecast(fit,h=horizon)$mean
}

frc
# Change frc to time series data
frc <- ts(frc, frequency=frequency(test),start=start(test))
  
plot(df.ts,ylab="Order Demand")
lines(frc,col="red")

legend("topright",c("Data",colnames(frc)),col=c("black","red"),lty=1)
