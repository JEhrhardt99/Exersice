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
library(visdat)
library(dplyr)




# Working Directory -------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory




# Read Data ---------------------------------------------------------------



# Example for reading data from this repositories root folder:

# data <- read.csv("main/Data_sets/BASF.csv")
# write.csv(data, "main/Problem_Sets_Output/processed_data.csv")




df_STAN <- read.csv("main/Data_sets/STAN_ALL.csv")


vis_dat(df_STAN,
        warn_large_data = FALSE)





# Nr. 2 -------------------------------------------------------------------



df_small <- df_STAN[c("COUNTRY",
                      "YEAR",
                      "VALU",
                      "WAGE",
                      "INDUSTRY",
                      "ISICREV3")]

vis_dat(df_small)




# Nr. 3 -------------------------------------------------------------------

df_small <- df_small[order(df_small$COUNTRY, df_small$INDUSTRY, df_small$YEAR),]


### build a group index

df_small$idnr <- df_small %>% group_indices(COUNTRY, INDUSTRY)

### use the group index to get all informations nessacry in one line 

df_small <- df_small %>% group_by(idnr) %>% mutate(VALU_lag_1 = dplyr::lag(VALU, n = 1L, default = NA,))

### calcualte the growth rate of valu added 

df_small$VALU_gr <- (df_small$VALU - df_small$VALU_lag_1)/df_small$VALU_lag_1






















