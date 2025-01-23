# Information -------------------------------------------------------------

# Forecasting Methods WiSe 24/25
# Jannes Ehrhardt
# Exercice XI

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
library(ggplot2)
library(ggthemes)
library(forecast)
library(lubridate)
library(lubridate)
library(fpp3)
library(tseries)
library(cowplot)
library(urca)
library(readr)

# Working Directory -------------------------------------------------------

setwd("~/Desktop/Applied_Economics_and_Data_Science/Courses/WiSe_24_25/Forecasting_Methods/Exercise/main/R_Script") # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory

# Load Data ---------------------------------------------------------------

gold <- read_csv("main/Data_sets/oilgold.csv")

















