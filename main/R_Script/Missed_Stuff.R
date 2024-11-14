#load packages------------------------
library("haven")
library("Hmisc")
library("expss")
library("dplyr")
library("ggplot2")
library("tidyverse")
library("readr")
# install.packages("forecast")
library(forecast)
library(lubridate)
library(zoo)
library(xts)
library(gridExtra)
# install.packages("estimatr")
library(estimatr)
library(modelsummary)



# wd ----------------------------------------------------------------------



setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory





#load data sets----------------------------





air_usa <- read_csv("main/Data_sets/air-usa.csv")

is.Date(air_usa$time)

air_usa <- air_usa %>%
  mutate(year= str_sub(time,1, 4),
         month=str_sub(time, 6)
         )%>%
  mutate(date= as.Date(paste("01", month, year, sep = "-"),
                       format("%d-%m-%Y")))

is.Date(air_usa$date)

#is it time series
is.ts(air_usa)

#change to time series
air_usa_ts <- ts(air_usa$pas, start= c(1996,1), frequency = 12)

is.ts(air_usa_ts)


#plot ts--------------------------

plot(air_usa$date,air_usa$pas,typ="l")

#or

ts.plot(air_usa_ts)

#or

plot_raw <- ggplot(air_usa, aes(x=date, y=pas)) +
  geom_line() +
  labs(title="Number of Passengers",
       x="Date",
       y="Passengers"
       )

plot_raw

ggsave("pass.png")

#add trendline----------------------------

plot_tl <- ggplot(air_usa, aes(x=date, y=pas))+
  geom_line()+
  geom_smooth(
    aes(color="Trendline"),
    method = "lm_robust",
    se= FALSE,
    linewidth= 0.5
    )+
  labs(title="Number of Passengers",
       x="Date",
       y="Passengers"
       )

plot_tl


# other techniques-----------------------------

layout(1:2)
plot(aggregate(air_usa_ts))
boxplot(air_usa_ts ~ cycle(air_usa_ts))

#R-Decomposition-------------------------------

air_decom <- decompose(air_usa_ts, type = "multiplicative")

plot(air_decom)


# regression models ---------------------------------

# time index

air_usa <- air_usa%>%
  mutate(t=row_number())

# OLS regression -------------------------

reg_ols <- lm_robust(pas~t, data=air_usa)

summary(reg_ols)

modelsummary(reg_ols,
         stars= c("*"=0.1,"**"=0.05, "***"=0.01),
         fmt = "%.3f"
         )











