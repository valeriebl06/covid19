set.seed(7)

#importing libraries

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,data.table, fpp2, tseries, gridExtra)

#importing the dataset

covid <- read.csv("data_2021-Jul-13.csv")

#printing the structure of the dataset

dim(covid)
covid[c(1,472),]

#ordering the df by date

covid <- setorder(covid, order=date)

#checking for NAs

anyNA(covid)

#converting the dataset into a time series

covid.ts <- ts(covid[,5], frequency=365, start=c(2020, 83), end=c(2021,188))

#splitting the dataset into a training & test set

train.ts <- window(covid.ts, end=c(2021, 174))
test.ts <- window(covid.ts, start=c(2021, 175))

#printing the structure of training & test ts

length(train.ts)
start(train.ts)
end(train.ts)
length(test.ts)
start(test.ts)
end(test.ts)

#plotting the train set & loess line for trend & seasonality

autoplot(train.ts)+
  geom_line()+
  stat_smooth(method='loess', se=F, col='orange')+
  labs(title='Daily COVID-19 hospitalisations in the UK', 
       x='days from 2020/03/23 to 2021/06/23', y='number of admissions')

#finding a d value to fix heteroscedasticity

adf.test(train.ts)

ndiffs(train.ts)

train.diff <- diff(train.ts)

adf.test(train.diff)

#plotting the ACF & PACF plots

tsdisplay(train.diff, main='COVID-19 Daily Hospitalisations in the UK', 
          xlab='days - from 2020-03-03 to 2021-06-23', 
          ylab='number of admissions')


#fitting the model

model.arima <- arima(train.ts, order=c(1,1,1))

summary(model.arima)

model.auto <- auto.arima(train.ts, stepwise=F, ic='aicc', trace=T, seasonal=F)

summary(model.auto)

#forecasting 14 days & printing confidence intervals

forecast.arima <- forecast(model.arima, 14)

forecast.arima

#comparing forecasts vs actuals w/ plots

predicted <- autoplot(forecast.arima)+
  ylab('nb of admissions')
observed <- autoplot(covid.ts, 
                     main='Daily COVID-19 Hospitalisations- Actuals')+
  ylab('nb of admissions')

grid.arrange(predicted, observed)

#fitting a holt's model

model.holt <- holt(train.ts, 14, initial='simple')

summary(model.holt)

#comparing forecasts vs actuals

holt <- autoplot(model.holt)+ ylab('number of admissions')
grid.arrange(holt, observed)

#comparing models

forecast.auto <- forecast(model.auto, 14)

accuracy(forecast.arima, test.ts)
accuracy(forecast.auto, test.ts)
accuracy(model.holt, test.ts)
