# Information -------------------------------------------------------------

# Forecasting Methods WiSe 24/25
# Jannes Ehrhardt
# Exercice IV

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




# Working Directory -------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory


# Load Data ---------------------------------------------------------------



station <- read_csv("main/Data_sets/station5108.csv",
                show_col_types = FALSE)


# Data Inspection and Preprocessing ---------------------------------------------------------

str(station)

str(station$p)

# number of NAs in p variable
sum(is.na(station$p))


# are there negative values in p?

# test <- station %>%
#   filter(p < 0)

sum(station$p < 0)


# remove NAs in P


# Data preprocessing
station <- station |> 
  filter(complete.cases(p)) |>
  mutate(p = p * 100,
         mydate = as.Date(mydate,
                          format = "%d%b%Y"),
         year = year(mydate),
         quarter = quarter(mydate)
  )



# Split data --------------------------------------------------------------

station_train <- station[1:1593,] # the komma is to indicate that it is a row index
station_test <- station[1594:1824,]

# Splitting the data must be done by time, since it is a time series data set


# plot data --------------------------------------------------------------
# using ggplot2

# p: avg E5 fuel price

ggplot(station, aes(x = mydate, y = p)) +
  geom_line(color = "blue") +
  labs(title = "Avg E5 fuel price",
       x = "Date",
       y = "Price in Euro") +
  theme_stata() +
  theme(plot.title = element_text(hjust = 0.5))



# make the same graph and add a vertical line to indicate the split between train and test data

ggplot(station, aes(x = mydate, y = p)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = as.numeric(station$mydate[1593]), color = "black") +
  labs(title = "Avg E5 fuel price",
       x = "Date",
       y = "Price in Euro") +
  theme_stata() +
  theme(plot.title = element_text(hjust = 0.5))



# make the same graph and add diesel prices (pd)

ggplot(station, aes(x = mydate)) +
  geom_line(aes(y = p / 100, color = "E5")) +
  geom_line(aes(y = pd, color = "Diesel")) +
  geom_vline(xintercept = as.numeric(station$mydate[1593]), color = "black") +
  labs(title = "Avg E5 ad Diesel Fuel Price",
       x = "Date",
       y = "Price in Euro") +
  theme_stata() +
  theme(plot.title = element_text(hjust = 0.5))


# There is no clear global trend -> What Method to use? 

# Convert TS ---------------------------------------------



# example of getting the day
as.POSIXlt(x = "2014-05-15")$yday

# starting date
as.POSIXlt(x = "2014-01-01")$yday

# starting date of test data
as.POSIXlt(x = "2018-05-15")$yday

# Convert to time series object
station_train_ts <- ts(station_train$p, 
                       start = c(2014, 1), # January 2014
                       frequency = 365.25) # daily frequency accounting for leap year

# Convert to time series object
station_test_ts <- ts(station_test$p, 
                       start = c(2018, as.POSIXlt(x = "2018-05-15")$yday), # May 2018
                       frequency = 365.25) # daily frequency




# SES --------------------------------------------

# see exercise slide_1.pdf SSE formula

alpha <- seq(0.01, 0.99, by = 0.01)
alpha

sse_mat <- sapply(alpha, function(a) {
  ets(station_train_ts, 
      model = "ANN",   # A: Additive N: None Trend N: None Seasonality
      alpha = a)$mse 
  }
)


# create a tibble data set:
sse <- tibble(alpha = alpha,
              sse = sse_mat) |> # change scale
  mutate(ln_sse = log(sse)) # log transformation


# plot using ggplot2

ggplot(sse, aes(x = alpha, y = ln_sse)) +
  geom_line(color = "red") +
  labs(title = "SSE vs Alpha",
       x = "Alpha",
       y = "Log SSE") +
  theme_stata() +
  theme(plot.title = element_text(hjust = 0.5))


# what is the optimal alpha value?

opt_alpha <- sse |> 
  filter(ln_sse == min(ln_sse)) |> 
  pull(alpha)                      # pull works like $ inside the dplyr pack

opt_alpha

# Much simple (same method) ---------------------------

sse_model <- ets(station_train_ts, 
                 model = "ANN")

summary(sse_model)

# What are the forecasting equations for this? Input the summary stats into the equation:
# THIS WILL BE IN THE EXAM!

# Forecast Equaition and Update Equation. Put the values in there!

# Forecast -------------------------------------------------

# We are doing an ahead forecast (not insample forecast)

# forecast for simple exponential smoothing

fore_ses <- forecast(sse_model,
                     h = 10) # 10 days ahead

# this could also be calculated by hand using the equations with the parameters from above
# USE THIS AS EXERCISE FOR EXAM PREP

# plot the forecast using ggplot2 - autoplot

autoplot(fore_ses) +
  labs(title = "SES Forecast",
       x = "Date",
       y = "Price in Euro") +
  theme_stata() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data.frame(Time = time(sse_model$x),
                  original = as.numeric(sse_model$x),
                  fitted = as.numeric(sse_model$fitted)
       ),
       aes(x = Time)) +
  geom_line(aes(y = original), color = "yellow") +
  geom_line(aes(y = fitted), color = "blue") +
  theme_stata() 
  

# restrict to the last periods to better see the forecast

autoplot(fore_ses,
         xlim = c(2018.2 , 2018.38)) + # just a snapshot
  autolayer(fitted(fore_ses)) +
  labs(title = "SES Forecast",
       x = "Date",
       y = "Price in Euro") +
  theme_stata() +
  theme(plot.title = element_text(hjust = 0.5))


# restrict to the last periods to better see the forecast

autoplot(fore_ses,
         xlim = c(2018.35 , 2018.38)) + # just a snapshot
  autolayer(fitted(fore_ses)) +
  labs(title = "SES Forecast",
       x = "Date",
       y = "Price in Euro") +
  theme_stata() +
  theme(plot.title = element_text(hjust = 0.5))


# Accuracy Measures --------------------------------------------

fore_error <- function(actual, predicted){
  tibble(
    ME = mean(actual - predicted),
    MEA = mean(abs(actual - predicted)),
    MAPE = mean(100 * abs(actual - predicted) / actual),
    RMSE = sqrt(mean((actual - predicted)^2))
  )
}



errors <- fore_error(station_test_ts[1:10],
                     fore_ses$mean)
errors


# Homework --------------------------------------------------------

# Do it for the RAIN 30 Day forecast






























































































































































































































