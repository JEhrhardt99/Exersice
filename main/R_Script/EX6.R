# Information -------------------------------------------------------------

# Forecasting Methods WiSe 24/25
# Jannes Ehrhardt
# Exercice VI

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
library(fastDummies)



# Working Directory -------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory


# Load Data ---------------------------------------------------------------

# load the waren data set

waren <- read_csv("main/Data_sets/waren.csv")




# Date Format -------------------------------------------------------------

str(waren)

# Convert 'time' column using lubridate
waren$time <- format(ymd(paste0(sub("m", "-", waren$time), "-01")), "%Y-%m")


# Plot Ts -----------------------------------------------------------------

# convert waren to ts
waren_ts <- ts(waren$waren, start = c(1998, 1), frequency = 12)


# plot the time series using ggplot 2
autoplot(waren_ts)

# count missings in waren
sum(is.na(waren$waren))

# drop the last 2 rows 
waren <- waren[-c(1:2),]

str(waren)

# convert time to date
waren$time <- as.Date(paste(waren$time, "01", sep = "-"),
                      format = "%Y-%m-%d")
waren <- waren %>%
  mutate(year = str_sub(time, 1,4),
         month = str_sub(time, 6,7),
         time = as.Date(paste(year, month, "01", sep = "-"),
                        format = "%Y-%m-%d")
         )



ggplot(waren, aes(x = time)) +
  geom_line(aes(y = waren)) +
  theme_few() +
  labs(title = "Time Series of Waren",
       x = "Time",
       y = "Waren") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Time Series Conversion --------------------------------------------------

waren_ts <- ts(waren$waren, start = c(1998, 1), frequency = 12)


# Split Data --------------------------------------------------------------

# Training data: begininng until last month in 2009
# remaining data as test data

waren_train <- window(waren_ts, end = c(2009, 12))
waren_test <- window(waren_ts, start = c(2010, 1))


# Regression Linear Trend and Seasonal dummies ----------------------------

waren <- waren %>%
  mutate(train_test = ifelse(row_number() <= length(waren_train), "train", "test"))


reg_ols <- lm_robust(waren ~ as.factor(month),
                     data = dplyr::filter(waren, train_test == "train")
)

modelsummary(reg_ols)




# Predict -----------------------------------------------------------------

# Insample prediction

waren <- waren %>%
  dplyr::mutate(forecast = predict(reg_ols, newdata = .))




waren <- waren %>%
  mutate(
    residuals = waren - forecast,
    error = ifelse(train_test == "test", waren - forecast, residuals)
  )



# Accuracy measures for the Test set 
test_metrics <- waren %>%
  dplyr::filter(train_test == "test") %>%
  dplyr::summarize(
    MAE = mean(abs(error), na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    MAPE = mean(abs(error / waren), na.rm = TRUE) * 100
  )

test_metrics

str(waren)



ggplot(waren, aes(x = time, y = waren)) +
  geom_line(color = "blue") +
  geom_line(aes(y = forecast), color = "red") +
  geom_point(data = filter(waren, train_test == "test"), aes(y = waren), color = "blue") +
  geom_point(data = filter(waren, train_test == "test"), aes(y = forecast), color = "red") +
  xlab("Year") +
  ylab("Waren") +
  ggtitle("Waren") +
  theme_stata() +
  theme(legend.position = "bottom")



ggplot(waren, aes(x = time)) +
  geom_line(aes(y = waren, color = "Actual")) +
  geom_line(aes(y = forecast, color = "Forecast")) +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  theme_stata() 


# Holt-Winters -----------------------------------------------------------
# additive model

hw_a <- hw(waren_train,
           seasonal = "additive",
           h = 12)

summary(hw_a)


autoplot(hw_a) +
  autolayer(fitted(hw_a), series = "Fitted") +
  autolayer(forecast(hw_a, h = 12), series = "Forecast") +
  xlab("Year") +
  ylab("Waren") +
  ggtitle("Holt Winters Additive Seasonality") +
  theme_tufte() +
  theme(legend.position = "bottom")



generics::accuracy(hw_a, waren_test)








# multiplicative model

hw_m <- hw(waren_train,
           seasonal = "multiplicative",
           h = 12)

summary(hw_m)


autoplot(hw_m) +
  autolayer(fitted(hw_m), series = "Fitted") +
  autolayer(forecast(hw_m, h = 12), series = "Forecast") +
  xlab("Year") +
  ylab("Waren") +
  ggtitle("Holt Winters Multiplicative Seasonality") +
  theme_tufte() +
  theme(legend.position = "bottom")



generics::accuracy(hw_m, waren_test)














