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


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set working directory to script location
setwd("..") # Move up to the project root directory


































