# 16.01.2025
# Load required ibraries
library(forecast)
library(tseries)
library(dplyr)
library(ggplot2)
library(readr)

# Working Directory -------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory
# Oilgold data

gold <- read_csv("main/Data_sets/oilgold.csv")
gold <- gold |>
  mutate(mydate = as.Date(date,format="%d%b%Y"))


# Plot ------------------------------------------------------------------

ggplot(gold,aes(x=mydate, y=goldprice))+
  geom_line()+
  labs(title="", x="Date", y="Gold price")


# ADF Test ----------------------------------------------------------------

# we are doing arima here but
# exponential smoothing would also be possible

# we see trend in the data


# Stationarity test -------------------------------------------------------
adf_1 <- adf.test(gold$goldprice)
adf_1

# Augmented Dickey-Fuller Test
# 
# data:  gold$goldprice
# Dickey-Fuller = -0.7967, Lag order = 18, p-value = 0.9619
# alternative hypothesis: stationary

# not stationary because
# p-value 0.919 > 0.05 --> fail to reject h0 (our series has unitroot -> is not stationary) -> not stationary

# unitroot_kpss
library(fpp3)
unitroot_kpss() #TODO

# ADFC Test ---------------------------------------------------------------

# sword criteria for lag selection
N = 5842 # number of observations
12*(N/100)^0.25
# [1] 33.17582
#TODO: open question: round() or floor()?

adf_2 <- adf.test(gold$goldprice, k=33) # k=lag
adf_2

# using ur.df test
library(urca)
ur_adf1 <- ur.df(gold$goldprice, lags=33, type="drift")
ur_adf1
summary(ur_adf1)

# if value of test statistics  is less than critical value -> h0 (has unit root-> not stationary) is rejected -> stationary
# if absolute value of test statistic, it has to be bigger than critical value
#but we fail to reject because -1.1081 is greater than -2.86 -> not stationary
#Value of test-statistic is: -1.1081 1.2687 
# just for the trend /drift parameter : 1.2687 
# concentrate on tau
#Critical values for test statistics: 
#  1pct  5pct 10pct
#tau2 -3.43 -2.86 -2.57
#phi1  6.43  4.59  3.78

# not stationary -> we need to take a difference


# First differencing ------------------------------------------------------
diff_gold <- diff(gold$goldprice)

gold <- gold %>% 
  mutate(diff_gold = c(NA,diff(gold$goldprice)))
# view(gold)


# plot --------------------------------------------------------------------


ggplot(gold,aes(x=mydate, y=diff_gold))+
  geom_line()+
  labs(title="first difference of gold", x="Date", y="price")

# fluctuating around 0... constant mean


# adf for 1st difference of gold ------------------------------------------



diff_adf <- ur.df(diff_gold, lags=33) # we can see no patterns in it, so no type
diff_adf
summary(diff_adf)

# Value of test-statistic is: -14.045 
# 
# Critical values for test statistics: 
#   1pct  5pct 10pct
# tau1 -2.58 -1.95 -1.62

#-14.045 < -1.62 # use 5% in the exams: -14.045 < -1.95
# we reject the h0 (has unit root -> is not stationary) -> stationary
# 1st diff is stationary

#tools > modify shortcuts to see rstudio shortcuts or alt+shift+k

# ACF and PACF plot for the 1st difference gold --------------------------------

par(mfrow=c(1,2))
Acf(gold$diff_gold, main="differenced gold ACF", na.action = na.pass)
Pacf(gold$diff_gold, main="differenced gold PACF", na.action = na.pass)


# ARIMA Models ------------------------------------------------------------

# ACF graph determines MA
# PACF graph determines AR

arima111 <- Arima(gold$diff_gold, order = c(1,0,1)) # if using difference, then.. 0 for I because using the differenced data already
arima111 <- Arima(gold$goldprice, order = c(1,1,1), include.drift=TRUE) # not using difference, I=1 to difference it
summary(arima111)
arima313 <- Arima(gold$goldprice, order = c(3,1,3), include.drift=TRUE)
summary(arima313)
arima311 <- Arima(gold$goldprice, order = c(3,1,1), include.drift=TRUE)
summary(arima311)
arima113 <- Arima(gold$goldprice, order = c(1,1,3), include.drift=TRUE)
summary(arima113)
#arima13113 <- Arima(gold$goldprice, order = c(13,1,13), include.drift=TRUE) #abigail excluded this because it took too long to comput

?Arima
# include mean or include.constant?
# if no difference used, then only include a constant if trend in original dataset
# if unsure use akaike AIC to just choose the best
AIC(arima111,arima313,arima311,arima113)
# best model of these 4: arima313 3,1,3 with a drift


# residual check ----------------------------------------------------------
checkresiduals(arima313)

# h0 (no autocorrelation)
# p > 0.05 accept (fail to reject) -> no autocorrelation BUT number of lags is 10 and there are significant spikes at bigger lags

# residual plot shows model arima313 is not good because
# not normally distributed (bottom right) because of outliers

# -> none of the arima111,arima313,arima311,arima113 are good

# automate
auto_best <- auto.arima(gold$goldprice, trace=TRUE,approximation = FALSE, stepwise = FALSE)

# residual analysis homework:
# interpret the test results of the best model found

# my solution attempt
arima212 <- Arima(gold$goldprice, order = c(2,1,2), include.drift=FALSE)
checkresiduals(arima212)

# -> still significant outliers(?)

