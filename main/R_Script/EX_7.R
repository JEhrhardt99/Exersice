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
library(ggplot2)
library(ggthemes)
library(forecast)
library(lubridate)
library(zoo)
library(estimatr)
library(lubridate)
library(generics)
library(fpp3)
library(tseries)
library(gridExtra)
library(mFilter)



# Working Directory -------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory


# Load Data ---------------------------------------------------------------

# load the waren data set
gdp <- read_csv("main/Data_sets/gdp.csv")


head(gdp)

# prepare data
gdp <- gdp %>%
  mutate(
    year = as.numeric(str_sub(time, 1,4)),
    quarter = as.numeric(str_sub(time, 6)),
    ID = row_number(),
    log_gdp = log(gdp)
         )



# Convert to TS -----------------------------------------------------------

gdp_ts <- ts(gdp$gdp,
             start = c(1991, 1),
             frequency = 4)


log_gdp_ts <- ts(gdp$log_gdp,,
                 start = c(1991, 1),
                 frequency = 4)

p1 <- autoplot(gdp_ts) +
  theme_minimal() +
  labs(title = "GDP",
       x = "Year",
       y = "GDP") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_stata()


p2 <- autoplot(log_gdp_ts) +
  theme_minimal() +
  labs(title = "Log GDP",
       x = "Year",
       y = "Log GDP") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_stata()


# combine both in 1 ggplot next to each other
grid.arrange(p1, p2, ncol = 2)





