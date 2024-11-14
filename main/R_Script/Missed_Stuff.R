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




## predict

air_usa <- air_usa %>%
  mutate(pas_pre = predict(reg_ols,
                           newdata = air_usa)
                           )


# Generate Polynomials ----------------------------------------------------

air_usa <- air_usa %>%
  mutate(t_2 = t^2,
         t_3 = t^3,
         t_4 = t^4,
         t_5 = t^5
         )


# Polynomial Regression ---------------------------------------------------


models <- list()

models[["Poly 2"]] <- lm_robust(pas ~ t+  t_2, 
                                data = air_usa)
models[["Poly 3"]] <- lm_robust(pas ~ t+  t_2 + t_3, 
                                data = air_usa)
models[["Poly 4"]] <- lm_robust(pas ~ t+  t_2 + t_3 + t_4,
                                data = air_usa)
models[["Poly 5"]] <- lm_robust(pas ~ t+  t_2 + t_3 + t_4 + t_5,
                                data = air_usa)


# Modelsummary

modelsummary(models,
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             fmt = "%.3f")



poly_2 <- lm_robust(pas ~ t+  t_2, 
                    data = air_usa)
poly_3 <- lm_robust(pas ~ t+  t_2 + t_3,
                    data = air_usa)
poly_4 <- lm_robust(pas ~ t+  t_2 + t_3 + t_4,
                    data = air_usa)
poly_5 <- lm_robust(pas ~ t+  t_2 + t_3 + t_4 + t_5,
                    data = air_usa)

# Predictions -------------------------------------------------------------




air_usa <- air_usa %>%
  mutate(pas_pre_poly2 = predict(poly_2, newdata = air_usa),
         pas_pre_poly3 = predict(poly_3, newdata = air_usa),
         pas_pre_poly4 = predict(poly_4, newdata = air_usa),
         pas_pre_poly5 = predict(poly_5, newdata = air_usa)
         )



# Graphing ----------------------------------------------------------------

# make a nice graph using ggplot2 showing the predictions as well

plot_poly <- ggplot(air_usa, aes(x = t, y = pas)) +
  geom_point() +
  geom_line(aes(y = pas_pre), color = "red") +
  geom_line(aes(y = pas_pre_poly2), color = "blue") +
  geom_line(aes(y = pas_pre_poly3), color = "green") +
  geom_line(aes(y = pas_pre_poly4), color = "purple") +
  geom_line(aes(y = pas_pre_poly5), color = "orange") +
  labs(title = "Polynomial Regression Models",
       x = "Time",
       y = "Passengers") +
  theme_minimal()



plot_poly







plot_poly2 <- ggplot(air_usa, aes(x = t, y = pas)) +
  geom_line() +
  geom_line(aes(y = pas_pre), color = "red") +
  geom_line(aes(y = pas_pre_poly2), color = "blue") +
  geom_line(aes(y = pas_pre_poly3), color = "green") +
  geom_line(aes(y = pas_pre_poly4), color = "purple") +
  geom_line(aes(y = pas_pre_poly5), color = "orange") +
  labs(title = "Polynomial Regression Models",
       x = "Time",
       y = "Passengers") +
  theme_minimal()

plot_poly2









# Exponential Regression --------------------------------------------------

# To isolate the trend

# Start by taking log of number of passengers
air_usa <- air_usa %>%
  mutate(log_pas = log(pas))

library(ggthemes)

# plot time against log of passengers using ggplot2
plot_log <- ggplot(air_usa, aes(x = t, y = log_pas)) +
  geom_point() +
  labs(title = "Log of Passengers",
       x = "Time",
       y = "Log of Passengers") +
  theme_few()

plot_log

# run regressions using log and then predict them

exp_reg <- lm_robust(log_pas ~ t,
                     data = air_usa)

msummary(exp_reg,
         stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
         fmt = "%.3f")

# summrize prediction

summary(exp_reg$fitted.values)

# plot the predictions

air_usa <- air_usa %>%
  mutate(log_pas_pre = predict(exp_reg, newdata = air_usa),
         pas_pre_exp = exp(log_pas_pre))

plot_exp <- ggplot(air_usa, aes(x = t, y = pas)) +
  geom_point() +
  geom_line(aes(y = pas_pre_exp), color = "red") +
  labs(title = "Exponential Regression Model",
       x = "Time",
       y = "Passengers") +
  theme_stata()

plot_exp

# now also plot showing the decling growth of the log

plot_exp2 <- ggplot(air_usa, aes(x = t, y = log_pas)) +
  geom_point() +
  geom_line(aes(y = log_pas_pre), color = "red") +
  labs(title = "Exponential Regression Model",
       x = "Time",
       y = "Log of Passengers") +
  theme_stata()

plot_exp2




# Generate Residuals ------------------------------------------------------

# OLS residuals

air_usa <- air_usa %>%
  mutate(res = pas - pas_pre,
         res2 = pas - pas_pre_poly2,
         res3 = pas - pas_pre_poly3,
         res4 = pas - pas_pre_poly4,
         res5 = pas - pas_pre_poly5)


# Question by Abigail: Are the resiudals a mistake? -> No they are the unexplained part of the model!!! Thesy are the shocks that can not be accounted for!

# plot the residuals using ggplot2

plot_poly_res <- ggplot(air_usa, aes(x = t, y = pas)) +
  geom_line(aes(y = res), color = "red") +
  geom_line(aes(y = res2), color = "blue") +
  geom_line(aes(y = res3), color = "green") +
  geom_line(aes(y = res4), color = "purple") +
  geom_line(aes(y = res4), color = "orange") +
  labs(title = "Residuals of Polynomial Regression Models",
       x = "Time",
       y = "Residuals") +
  theme_stata()

plot_poly_res



# maybe better do it individually:

plot_poly_res <- ggplot(air_usa, aes(x = t, y = pas)) +
  geom_line(aes(y = res), color = "red") +
  theme_stata()

plot_poly_res

plot_poly_res2 <- ggplot(air_usa, aes(x = t, y = pas)) +
  geom_line(aes(y = res2), color = "blue") +
  theme_stata()

plot_poly_res2

plot_poly_res3 <- ggplot(air_usa, aes(x = t, y = pas)) +
  geom_line(aes(y = res3), color = "green") +
  theme_stata()

plot_poly_res3

plot_poly_res4 <- ggplot(air_usa, aes(x = t, y = pas)) +
  geom_line(aes(y = res4), color = "purple") +
  theme_stata()

plot_poly_res4


plot_poly_res5 <- ggplot(air_usa, aes(x = t, y = pas)) +
  geom_line(aes(y = res5), color = "orange") +
  theme_stata()

plot_poly_res5


library(gridExtra)

gridExtra::grid.arrange(plot_poly_res,
                        plot_poly_res2,
                        plot_poly_res3,
                        plot_poly_res4,
                        plot_poly_res5,
                        ncol = 2)

# be sure that u actually have a mean of 0:
summary(air_usa$res)


# Standardization ---------------------------------------------------------

# The residuals or coefficients of the different models can not be compared directly, if they are not standardized

# Lte's do standardization:

# Write a function to do it


st <- function(x){
  return(x - mean (x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


# standardize the residuals:

res_st <- st(air_usa$res)
res_st


# antother method using dplyr

air_usa <- air_usa %>%
  mutate(st_res = (res - mean(res, na.rm = TRUE)) / sd(res, na.rm = TRUE),
         st_res2 = (res2 - mean(res2, na.rm = TRUE)) / sd(res2, na.rm = TRUE),
         st_res3 = (res3 - mean(res3, na.rm = TRUE)) / sd(res3, na.rm = TRUE),
         st_res4 = (res4 - mean(res4, na.rm = TRUE)) / sd(res4, na.rm = TRUE),
         st_res5 = (res5 - mean(res5, na.rm = TRUE)) / sd(res5, na.rm = TRUE)
         ) 

# now the residuals can be compared


# Histogramm Plot ---------------------------------------------------------



# another way to asses if it is normally distributed is to use a histogram plot (ggplot2)

plot_hist <- ggplot(air_usa, aes(x = st_res)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Standardized Residuals",
       x = "Standardized Residuals",
       y = "Frequency") +
  theme_minimal()

plot_hist


# solution of abigail:

air_long <- air_usa %>%
  dplyr::select(starts_with("st_")) %>%
  tidyr::pivot_longer( # for long format
    cols = starts_with("st_"),
    names_to = "dist",
    values_to = "value"
  )

# ggplot

plot_hist2 <- ggplot(air_long, aes(x = value)) +
  geom_histogram(binwidth = 0.2,
                 fill = "blue",
                 color = "black",
                 aes(y = after_stat(density)),
                 bins = 30) +
  facet_wrap(~dist,
             scale = "free") +
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 1),
                aes(color = dist),
                linewidth = 1) +
  labs(title = "Histogram of Standardized Residuals",
       x = "Standardized Residuals",
       y = "Frequency") +
  theme_minimal()

plot_hist2
