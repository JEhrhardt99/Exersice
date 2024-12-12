# Information -------------------------------------------------------------

# Forecasting Methods WiSe 24/25
# Jannes Ehrhardt
# Exercice V

# Commands for version control with git:
# cd PATH
# git status
# git add .
# git commit -m "header"
# git push -u origin main


# clear global environment
rm(list = ls())



# Packages ----------------------------------------------------------------


# Install

# install.packages("tidyverse")


# Load

library(tidyverse)
library(estimatr)
library(modelsummary)
library(visdat)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(haven)
library(Hmisc)
library(expss)
library(dplyr)
library(forecast)
library(lubridate)
library(zoo)
library(xts)
library(gridExtra)
library(estimatr)
library(lubridate)
library(generics)
library(fpp2)
library(tseries)



# Working Directory -------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory


# Load Data ---------------------------------------------------------------

aus <- austa

autoplot(aus)

# explain the plot with regards to the components of the time series and suggest a method/model for forecasting


# >> Halts Winters Linear Trend Model (see lectures)


# Exponential Smoothing with Trend ----------------------------------------

aus_holt <- holt(aus,
                 h = 5)

summary(aus_holt)

# use this output to write down the equotation of the model:

# gt <- 0.999 * t + (1 -0.999)gt_1 + bt_1
# 
# bt <- 
#   
# fc <- 


autoplot(aus_holt) + 
  autolayer(fitted(aus_holt), series = "Fitted") +
  autolayer(forecast(aus_holt, h = 5), series = "Forecast") +
  xlab("Year") +
  ylab("Passenger Kilometers") +
  ggtitle("Holt's Exponential Smoothing with Trend") +
  theme_tufte() +
  theme(legend.position = "bottom")


checkresiduals(aus_holt)

# Residuals have normal dist around 0, residuals plot looks like a random walk around 0
# ACF are within the confidence intervals, so no significant spikes that indicate autocorrelation



# Import Electricity ------------------------------------------------------

elec <- read_csv("main/Data_sets/electricity-usa.csv")


str(elec)

elec <- elec %>%
  mutate(time = as.Date(as.yearmon(year, 
                                   format = "%Y %B")),
         month = month(time),
         ID = row_number()
  )



# plot ts using ggplot2

ggplot(elec, aes(x = time, y = eletpus)) +
  geom_line(color = "blue") +
  xlab("Year") +
  ylab("Electricity net Generation") +
  ggtitle("Electricity net Generation in the USA") +
  theme_stata() +
  theme(legend.position = "bottom")


# plot ts using ggplot2 and reformat the ylabs shown number

ggplot(elec, aes(x = time, y = eletpus)) +
  geom_line(color = "blue") +
  scale_y_continuous(labels = scales::comma) +
  xlab("Year") +
  ylab("Electricity net Generation") +
  ggtitle("Electricity net Generation in the USA") +
  theme_stata() +
  theme(legend.position = "bottom")


# >> it is a multiplicative seasonality in the time series

# >> Halt Winters with multiplicative seasonality can be applied


# Halt Winters Multiplicative ---------------------------------------------


# convert to ts object

elec_ts <- ts(elec$eletpus, 
              start = c(1973, 1), # start date
              frequency = 12)     # monthly seasonality

# split into training and test data

elec_train <- window(elec_ts, 
                     end = c(2009, 12))

elec_test <- window(elec_ts,
                    start = c(2010, 1),
                    end = c(2010, 11))


# start the analysis

elec <- elec %>%
  mutate(train_test = ifelse(row_number() <= length(elec_train), "train", "test"))


# OLS Reg -----------------------------------------------------------------

reg_ols <- lm_robust(eletpus ~ ID + as.factor(month),
                      data = dplyr::filter(elec, train_test == "train")
                      )

summary(reg_ols)

modelsummary(reg_ols)


# do the forecast


# Insample Prediction -----------------------------------------------------


elec <- elec %>%
  dplyr::mutate(forecast = predict(reg_ols, newdata = .))


# elec <- elec %>%
#   mutate(
#     residuals = eletpus - forecast,
#     error = ifelse(train_test == "train", residuals, NA_real_)
#     )

elec <- elec %>%
  mutate(
    residuals = eletpus - forecast,
    error = ifelse(train_test == "test", eletpus - forecast, residuals)
  )

# Accuracy measures for the Test set -------------------------------------------------------------------------

test_metrics <- elec %>%
  dplyr::filter(train_test == "test") %>%
  dplyr::summarize(
    MAE = mean(abs(error), na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    MAPE = mean(abs(error / eletpus), na.rm = TRUE) * 100
  )


test_metrics


# Plot forecast vs actual -------------------------------------------------


ggplot(elec, aes(x = time, y = eletpus)) +
  geom_line(color = "blue") +
  geom_line(aes(y = forecast), color = "red") +
  geom_point(data = filter(elec, train_test == "test"), aes(y = eletpus), color = "blue") +
  geom_point(data = filter(elec, train_test == "test"), aes(y = forecast), color = "red") +
  xlab("Year") +
  ylab("Electricity net Generation") +
  ggtitle("Electricity net Generation in the USA") +
  theme_stata() +
  theme(legend.position = "bottom")



ggplot(elec, aes(x = time)) +
  geom_line(aes(y = eletpus, color = "Actual")) +
  geom_line(aes(y = forecast, color = "Forecast")) +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  theme_stata() 


hw_m <- hw(elec_train, seasonal = "multiplicative", h = 11)         
summary(hw_m)



autoplot(hw_m) +
  autolayer(fitted(hw_m), series = "Fitted") +
  autolayer(forecast(hw_m, h = 11), series = "Forecast") +
  xlab("Year") +
  ylab("Electricity net Generation") +
  ggtitle("Holt Winters Multiplicative Seasonality") +
  theme_tufte() +
  theme(legend.position = "bottom")




generics::accuracy(hw_m, elec_test)



# Holt Winters Additative Model -------------------------------------------


hw_a <- hw(elec_train, seasonal = "additive", h = 11)

summary(hw_a)

autoplot(hw_a) +
  autolayer(fitted(hw_a), series = "Fitted") +
  autolayer(forecast(hw_a, h = 11), series = "Forecast") +
  xlab("Year") +
  ylab("Electricity net Generation") +
  ggtitle("Holt Winters Additive Seasonality") +
  theme_tufte() +
  theme(legend.position = "bottom")

generics::accuracy(hw_a, elec_test)


# What other model could we use?


# Additive Seasonal with log ----------------------------------------------

elec <- elec %>%
  mutate(
    eletpus_log = log(eletpus)
    )


elec_train_log <- ts(elec$eletpus_log[1:444], 
                     start = c(1973, 1), # start date
                     frequency = 12)     # monthly seasonality)

elec_test_log <- ts(elec$eletpus_log[445:455],
                        start = c(2010, 1),
                        end = c(2010, 11),
                        frequency = 12)




hw_log <- hw(elec_train_log, seasonal = "additive", h = 11)

summary(hw_log)

autoplot(hw_log) +
  autolayer(fitted(hw_log), series = "Fitted") +
  autolayer(forecast(hw_log, h = 11), series = "Forecast") +
  xlab("Year") +
  ylab("Electricity net Generation") +
  ggtitle("Holt Winters Additive Seasonality with Log") +
  theme_tufte() +
  theme(legend.position = "bottom")


generics::accuracy(hw_log, elec_test_log)

# to compare, we need to transform the forecast back to the original scale


# Back Transform ----------------------------------------------------------

bck <- exp(hw_log$mean)


# Compute accuracy measure for log ----------------------------------------



MAE_log = mean(abs(elec_test - bck), na.rm = TRUE)
RMSE_log = sqrt(mean((elec_test - bck)^2, na.rm = TRUE))
MAPE_log = mean(abs((elec_test - bck) / elec_test), na.rm = TRUE) * 100

# comparing
generics::accuracy(hw_a, elec_test)
generics::accuracy(hw_m, elec_test)

test_metrics
MAE_log
RMSE_log
MAPE_log










