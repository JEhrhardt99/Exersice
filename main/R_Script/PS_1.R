# Information -------------------------------------------------------------

# Forecasting Methods WiSe 24/25
# Jannes Ehrhardt
# Problem Set I

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




# Working Directory -------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory




# Read Data ---------------------------------------------------------------



# Example for reading data from this repositories root folder:

# data <- read.csv("main/Data_sets/BASF.csv")
# write.csv(data, "main/Problem_Sets_Output/processed_data.csv")




df_STAN <- read.csv("main/Data_sets/STAN_ALL.csv")



























