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













