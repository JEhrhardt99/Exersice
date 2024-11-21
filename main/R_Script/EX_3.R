# Information -------------------------------------------------------------

# Forecasting Methods WiSe 24/25
# Jannes Ehrhardt
# Exercice III

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




# Working Directory -------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory




# Load Data ---------------------------------------------------------------

dax <- read_csv("main/Data_sets/DAX.csv",
                 show_col_types = FALSE)


# Data Preprocessing ------------------------------------------------------

# rename columns:
dax <- dax %>%
  rename(adj_close = "Adj Close") %>%
  arrange(Date) 

# check structure data
class(dax$Date)
str(dax)


# Subject data for Adj_close
dax_ts <- dax %>%
  select(adj_close)

# set adj_close as time series object
dax_ts <- ts(dax_ts,
             start = c(2009, 1), # start date: Januar 2009
             frequency = 365.25) # Frequency: daily (also account for leap years every 4 years)


# plot dax_ts using ggplot2
autoplot(dax_ts) +
  theme_few() +
  labs(title = "DAX Time Series",
       x = "Year",
       y = "Adjusted Close Price") +
  theme(plot.title = element_text(hjust = 0.5))


# make it more pretty
ggplot(dax, 
       aes(x = Date, y = adj_close)) +
  geom_line(color = "blue") +
  theme_stata() +
  labs(title = "DAX Time Series",
       x = "Year",
       y = "Adjusted Close Price") +
  theme(plot.title = element_text(hjust = 0.5))

# Is there seasonality?
# There is some kind of seasonality, but it 
# is not very strong and it looks rather like additive seasonality.

# What do we do with it now?


# TS Analysis -------------------------------------------------------------

# Generate a new column

dax_ts <- dax %>%
  mutate(shares = "dax")

# save dataset

write_csv(dax_ts,
          "main/Data_sets/dax_1.csv")


# VW ----------------------------------------------------------------------

# Second data VW

# Load Data ---------------------------------------------------------------

VW <- read_csv("main/Data_sets/VW.csv",
                show_col_types = FALSE)


# Data Preprocessing ------------------------------------------------------

# rename columns:
VW <- VW %>%
  rename(adj_close = "Adj Close") %>%
  arrange(Date) 

# check structure data
class(VW$Date)
str(VW)


# Subject data for Adj_close
VW_ts <- VW %>%
  select(adj_close)

# set adj_close as time series object
VW_ts <- ts(VW_ts,
             start = c(2009, 1), # start date: Januar 2009
             frequency = 365.25) # Frequency: daily (also account for leap years every 4 years)


# plot dax_ts using ggplot2
autoplot(VW_ts) +
  theme_few() +
  labs(title = "DAX Time Series",
       x = "Year",
       y = "Adjusted Close Price") +
  theme(plot.title = element_text(hjust = 0.5))


# make it more pretty
ggplot(VW, 
       aes(x = Date, y = adj_close)) +
  geom_line(color = "blue") +
  theme_stata() +
  labs(title = "DAX Time Series",
       x = "Year",
       y = "Adjusted Close Price") +
  theme(plot.title = element_text(hjust = 0.5))





# add Shares Column
VW_ts <- VW %>%
  mutate(shares = "VW")


# save dataset

write_csv(VW_ts,
          "main/Data_sets/VW_1.csv")


# BASH --------------------------------------------------------------------

# Third data BASH

# Load Data ---------------------------------------------------------------

basf <- read_csv("main/Data_sets/BASF.csv",
               show_col_types = FALSE)


# Data Preprocessing ------------------------------------------------------

# rename columns:
basf <- basf %>%
  rename(adj_close = "Adj Close") %>%
  arrange(Date) 

# check structure data
class(basf$Date)
str(basf)


# Subject data for Adj_close
basf_ts <- basf %>%
  select(adj_close)

# set adj_close as time series object
basf_ts <- ts(basf_ts,
            start = c(2010, 8), # start date: Januar 2009
            frequency = 365.25) # Frequency: daily (also account for leap years every 4 years)


# plot dax_ts using ggplot2
autoplot(basf_ts) +
  theme_few() +
  labs(title = "DAX Time Series",
       x = "Year",
       y = "Adjusted Close Price") +
  theme(plot.title = element_text(hjust = 0.5))


# make it more pretty
ggplot(basf, 
       aes(x = Date, y = adj_close)) +
  geom_line(color = "blue") +
  theme_stata() +
  labs(title = "DAX Time Series",
       x = "Year",
       y = "Adjusted Close Price") +
  theme(plot.title = element_text(hjust = 0.5))





# add Shares Column
basf_ts <- basf %>%
  mutate(shares = "BSAF")


# save dataset

write_csv(basf_ts,
          "main/Data_sets/BSAF_1.csv")




# Append Data ------------------------------------------------------------------



stock <- rbind(dax_ts,
               VW_ts,
               basf_ts)

# clear environment 

rm(VW,
   dax,
   basf)


# rearange by shares and then date
stock <- stock %>%
  arrange(shares, Date)


# create unique ids by grouping
stock <- stock %>%
  group_by(shares) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()


# Centered Moving Average -----------------------------------------------------

# by using a predefined function 


# Co Pilot
# -----------------------------------------------------
# centered_ma <- function(x, k = 5) {
#   n <- length(x)
#   if (k %% 2 == 0) {
#     stop("k must be odd")
#   }
#   if (k > n) {
#     stop("k must be less than n")
#   }
#   half_k <- (k - 1) / 2
#   ma <- numeric(n)
#   for (i in 1:n) {
#     if (i < half_k + 1) {
#       ma[i] <- mean(x[1:(i + half_k)])
#     } else if (i > n - half_k) {
#       ma[i] <- mean(x[(i - half_k):n])
#     } else {
#       ma[i] <- mean(x[(i - half_k):(i + half_k)])
#     }
#   }
#   ma
# }
# 
# # apply function to stock data
# 
# stock <- stock %>%
#   group_by(ID) %>%
#   mutate(ma = centered_ma(adj_close, k = 5)) %>%
#   ungroup()
# -----------------------------------------------------





sm <- function(x, n = 5) {
  stats::filter(x, rep(1/n, n), sides = 2) # choose two sides because it is centered
}


# WRONG example!!!
stock$sm_5_WRONG <- sm(stock$adj_close)

# This does go over all the rows and does not stop at the end of the group.

# CORRECT example!!!
stock <- stock %>%
  group_by(ID) %>%
  mutate(sm_5_CORRECT = sm(adj_close)) %>%
  ungroup()







