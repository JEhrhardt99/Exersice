# Information -------------------------------------------------------------

# Forecasting Methods WiSe 24/25
# Jannes Ehrhardt
# Exercice VII

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
library(cowplot)
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


# combine both in 1 ggplot next to each other using cowplot package
plot_grid(p1, p2, labels = c("A", "B"), ncol = 2)



# Linear Detrending -------------------------------------------------------

lin_mod <- lm_robust(log_gdp ~ ID,
                     data = gdp)


# extract the fitted values (as ts-object):
lin_trend <- ts(lin_mod$fitted.values,
                start = c(1991, 1),
                frequency = 4)

# cycle component of the trend
lin_cycle <- log_gdp_ts - lin_trend


# plot that
autoplot(lin_cycle) +
  theme_minimal() +
  labs(title = "Linear Detrending",
       x = "Year",
       y = "Log GDP") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_stata()


# easier way would have been to use the hodrick filter:


# Hodrick-Prescott (HP) Filter: -------------------------------------------

# lambda value for cycle = 1600 (quarterly data)

hp_decomp <- hpfilter(log_gdp_ts,
                      freq = 16000)

# extract information from hp_decomp:
hp_trend <- hp_decomp$trend
hp_cycle <- hp_decomp$cycle

# combine these into a data.frame:
HP_out <- cbind(hp_decomp$x, 
                hp_decomp$trend,
                hp_decomp$cycle) 

colnames(HP_out) <- c("x", "trend", "cycle")

# store as df
HP_out <- as.data.frame(HP_out)

# easier way to plot
plot(hpfilter(log_gdp_ts, freq = 1600))

# what about the lambda value (freq)
# what if it is a monthy dataset: 14400
# annual: 10
# daily:  14400*30 (check it again)



# compare a little bit 

# Growth Rates -------------------------------------------------------------------------

gdp <- gdp %>%
  mutate(
    gr_log_gdp_1y = log_gdp - lag(log_gdp,
                                  4),       # lag 4 quarters = lag 1 year
    gr_log_gdp_1q = log_gdp - lag(log_gdp,
                                  1)        # lag 1 quarter
  )


# plot 

par(mfrow = c(1,1))
plot(gdp$gr_log_gdp_1y,
     type = "l",
     col = "darkblue",
     ylab = "Growth Rate",
     xlab = "Year",
     main = "Growth Rate of Log GDP (1 year lag)",
     lwd = 2)
lines(gdp$gr_log_gdp_1q,
      type = "l",
      col = "green",
      lwd = 2)
lines(HP_out$cycle,
      type = "l",
      col = "red",
      lwd = 2)
legend("bottomleft",
       legend = c("1 year growth", "1 quarter growth", "HP cycle"),
       col = c("darkblue", "green", "red"),
       lwd = 2,
       lty = 1,
       bty = "n"
       )
abline(h = 0,
       col = "grey")


# Question: Explain these plots and explain how the 1 year growth differs from the 1 quarter growth and the HP cycle.

# -> Same fluctuations can be seen. Gr of first quearter shows smaller deviations than one year and is more in line with HP cycle.

# -> HP cycle is combines both fluctuations and is more in between the actual fluctuations of the growth rates. It is a smoother version of the growth rates.

# -> LOOK THAT UP MORE CLOSELY FOR EXAM!












