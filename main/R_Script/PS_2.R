# Information -------------------------------------------------------------

# Forecasting Methods WiSe 24/25
# Jannes Ehrhardt
# Problem Set II

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
library(zoo)




# Working Directory -------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory




# Read Data ---------------------------------------------------------------



# Example for reading data from this repositories root folder:

# data <- read.csv("main/Data_sets/BASF.csv")
# write.csv(data, "main/Problem_Sets_Output/processed_data.csv")





df <- read.csv2("main/Data_sets/expo-impo-germany.csv",
                header = TRUE,
                sep = ",")




# Data preprocessing ------------------------------------------------------


df$time

# create month variable
df$month <- gsub(".*m(\\d)", "\\1", df$time)

# Convert number into monthname
df$month_name <- month.abb[as.numeric(df$month)]


# safe year as integer
df$year <- as.numeric(gsub("^(\\d{4}).*", "\\1", df$time))


# use zoo package to create date variable
df$date <- as.Date(as.yearmon(df$time, format = "%Ym%m"))

# make p_imports and p_exports as numeric 
df$p_imports <- as.numeric(df$p_imports)
df$p_exports <- as.numeric(df$p_exports)


ggplot(df, aes(x = date)) +
  geom_line(aes(y = exports, color = "Exports"), linewidth = 1.2) + 
  geom_line(aes(y = imports, color = "Imports"), linewidth = 1.2) + 
  labs(
    title = "Times Series",
    subtitle = "of German exports and imports",
    x = "Date",
    y = "Growth Rates",
    color = "Lines:"
  ) +
  theme_few(base_size = 10) +  # Minimal theme
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "top"
  )



ggplot(df, aes(x = date)) +
  geom_line(aes(y = p_exports, color = "% Exports"), linewidth = 1.2) + 
  geom_line(aes(y = p_imports, color = "% Imports"), linewidth = 1.2) + 
  labs(
    title = "Times Series",
    subtitle = "of German exports and imports",
    x = "Date",
    y = "Growth Rates",
    color = "Lines:"
  ) +
  theme_few(base_size = 10) +  # Minimal theme
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "top"
  )











# testing around

ts_data <- ts(df)


# decomposed <- decompose(ts_data, type = "multiplicative")
# 
# ts_data



# Assuming your data frame is named df
# Set start and frequency based on your data, assuming monthly data from 2000


exports_ts <- ts(df$exports, start = c(2000, 1), frequency = 12)



## Additative Model

# Decompose the exports time series
exports_decomp <- decompose(exports_ts)
# Plot the decomposition
plot(exports_decomp)


# STL decomposition of the exports time series
exports_stl <- stl(exports_ts, s.window = "periodic")
# Plot the decomposition
plot(exports_stl)





## Multiplicative Model

# Multiplicative decomposition of the exports time series
exports_decomp_mult <- decompose(exports_ts, type = "multiplicative")
# Plot the decomposition
plot(exports_decomp_mult)



# Take the log of the time series for multiplicative decomposition using STL
log_exports_ts <- log(exports_ts)
# Apply STL decomposition
exports_stl_mult <- stl(log_exports_ts, s.window = "periodic")
# Plot the decomposition, and exponentiate components if needed
plot(exports_stl_mult)















































































































































































































































































































































































































