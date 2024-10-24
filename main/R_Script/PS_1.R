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
library(ggplot2)
library(ggthemes)
library(stringr)




# Working Directory -------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory




# Read Data ---------------------------------------------------------------



# Example for reading data from this repositories root folder:

# data <- read.csv("main/Data_sets/BASF.csv")
# write.csv(data, "main/Problem_Sets_Output/processed_data.csv")




df_STAN <- read.csv("main/Data_sets/STAN_ALL.csv")


# vis_dat(df_STAN,
#         warn_large_data = FALSE)





# Nr. 2 -------------------------------------------------------------------



df_small <- df_STAN[c("COUNTRY",
                      "YEAR",
                      "VALU",
                      "WAGE",
                      "INDUSTRY",
                      "ISICREV3")]

vis_dat(df_small)




# Nr. 3 -------------------------------------------------------------------


# first order the dataframe first after country, then industry and then year

df_small <- df_small[order(df_small$COUNTRY, df_small$INDUSTRY, df_small$YEAR),]





# Use group_by() and group_indices create a unique industry id

df_small <- df_small %>% 
  group_by(COUNTRY, INDUSTRY) %>% 
  mutate(idnr = group_indices())



# create a variable that is the lag of VALU so everything for the growth rate is in one row

df_small <- df_small %>% 
  group_by(idnr) %>% 
  mutate(VALU_lag_1 = dplyr::lag(VALU, n = 1L, default = NA,))



# calcualte the growth rate of valu added 

df_small$VALU_gr <- (df_small$VALU - df_small$VALU_lag_1)/df_small$VALU_lag_1








# Nr. 4 -------------------------------------------------------------------



# create a variable that is the lag of wage so everything for the growth rate is in one row

df_small <- df_small %>% 
  group_by(idnr) %>% 
  mutate(WAGE_lag_1 = dplyr::lag(WAGE, n = 1L, default = NA,))


# calcualte the growth rate of wages 

df_small$WAGE_gr <- (df_small$WAGE - df_small$WAGE_lag_1)/df_small$WAGE_lag_1


# heatmapp

vis_dat(df_small)





# Nr. 5 -------------------------------------------------------------------


df_small2 <- df_small %>% 
  filter(!is.na(WAGE) | !is.na(VALU))



vis_dat(df_small2)


# Nr. 6 -------------------------------------------------------------------


# keep France

df_france_trailer <- df_small2[(df_small2$COUNTRY == "FRA"),]


# find out about the Semi-Trailer Industry

unique(df_france_trailer$INDUSTRY)

# can not find the Semi-Trailer Industry like this, so I use stringr package to find it:

find_trailer <- df_france_trailer %>%
  filter(str_detect(INDUSTRY, "TRAILER")) %>%
  select(idnr, INDUSTRY)

# => idnr 890 represents the Industry we are looking for

# keep France

df_france_trailer <- df_france_trailer[(df_france_trailer$idnr == 890),]

vis_dat(df_france_trailer)


# plot:


# Scatterplot
ggplot(df_france_trailer, aes(x = VALU_gr, y = WAGE_gr)) +
  geom_point(size = 4, alpha = 0.7) + 
  labs(
    title = "Motor Vehicles, Trailers and Semi-Trailers sector",
    subtitle = "Wage growth and Value growth",
    x = "Value added groth rates",
    y = "Wage growth rates",
    color = "Category"
  ) +
  theme_few(base_size = 10) +  # Minimal theme
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "top"
  )






# Nr. 7 -------------------------------------------------------------------



ggplot(df_france_trailer, aes(x = YEAR)) +
  geom_line(aes(y = VALU_gr, color = "Value Growth"), linewidth = 1.2) + 
  geom_line(aes(y = WAGE_gr, color = "Wage Growth"), linewidth = 1.2) + 
  labs(
    title = "Motor Vehicles, Trailers and Semi-Trailers sector",
    subtitle = "Wage growth and Value growth over time",
    x = "Year",
    y = "Growth Rates",
    color = "Lines:"
  ) +
  theme_few(base_size = 10) +  # Minimal theme
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "top"
  )



# Nr. 8 -------------------------------------------------------------------

mean_VALU_gr <- mean(df_france_trailer$VALU_gr,
                     na.rm = TRUE)

mean_WAGE_gr <- mean(df_france_trailer$WAGE_gr,
                     na.rm = TRUE)


# show result BIG
plot.new()

plot.window(xlim = c(0, 10), ylim = c(0, 10))

text(x = 5, y = 6, 
     labels = paste0("Mean VALU_gr: \n", round(mean_VALU_gr, 7), "\n \n \n \n"), 
     cex = 2, font = 2) 

text(x = 5, y = 4, 
     labels = paste0("Mean WAGE_gr: \n", round(mean_WAGE_gr, 7)), 
     cex = 2, font = 2)

