# Information -------------------------------------------------------------

# Forecasting Methods WiSe 24/25
# Jannes Ehrhardt
# Exercice VIII

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


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script location
setwd("../..") # move up to the project root directory
getwd() # check if wd is the root directory


# Load Data ---------------------------------------------------------------



# Simulate Data: ----------------------------------------------------------

set.seed(1)

# Deterministic Trend -----------------------------------------------------

df <- tibble(
  t = 1:500,
  uuid = rnorm(500,       # error term
              mean = 0,
              sd = 10)
) %>%
  mutate(
    y = NA_real_,
    y = case_when(t == 1 ~ 1),
    dt = case_when(t > 1 ~ 0.9 + t + uuid) # deterministic trend
  )

# plot 
ggplot(df, aes(x = t)) +
  geom_line(aes(y = dt)) +
  theme_minimal() +
  labs(title = "Simulated Data",
       x = "Time",
       y = "Value") +
  theme_stata()



# Random Walk -------------------------------------------------------------

df <- df %>%
  mutate(
    rw = cumsum(uuid) # random walk
  )

# Random Walk with drift -------------------------------------------------------------

# doing it very baic here with base r
df$st <- df$y

for (i in 2:nrow(df)) {                 # second to nth row
  df$st[i] <- 0.9 + df$st[i-1] + df$uuid[i]   # 0.9 = constant term (bo) 
}


# Random Walk without Drift -----------------------------------------------


# doing it very baic here with base r
df$rw_nd <- df$y



for (i in 2:nrow(df)) {                 # second to nth row
  df$rw_nd[i] <- df$rw_nd[i-1] + df$uuid[i]   # 0.9 = constant term (bo) 
}



# Plot them togerther -----------------------------------------------------

par(mfrow=c(1,3))
plot(df$dt,
     type = "l",
     col = "darkblue",
     xlab = "Index",
     ylab = "Value",
     main = "Deterministic Trend")
plot(df$st,
     type = "l",
     col = "darkred",
     xlab = "Index",
     ylab = "Value",
     main = "Random Walk with Drift")
plot(df$rw_nd,
     type = "l",
     col = "darkgreen",
     xlab = "Index",
     ylab = "Value",
     main = "Random Walk without Drift")


# end the par

par(mfrow=c(1,1))






# stationarity test -------------------------------------------------------





# ADF test with adf.test --------------------------------------------------

# Ho: our ts data set has unit roots (non-stationary)
# Decision p_value should be < 0.05 to reject Ho

adf.test(df$st)

# p > 0.05, we fail to reject Ho, our ts data set has unit roots (non-stationary)

# Now run the same test for the random walk with drift
adf.test(df$rw_nd)

unitroot_kpss(df$st)
unitroot_kpss(df$rw_nd)




# Generate AR and MA Process ----------------------------------------------


## AR(1) ------------------------------------------------------------------

# y_t = c + \phi_1 y_{t-1} + \epsilon_t


df$ar1 <- df$y

for (i in 2:nrow(df)) {                 # second to nth row
  df$ar1[i] <- 0.9 + 0.6 * df$ar1[i-1] + df$uuid[i]   # 0.9 = constant term (c), 0.6 = slope (\phi_1)
}


# plot ar1 with ggplot2

ggplot(df, aes(x = t)) +
  geom_line(aes(y = ar1)) +
  theme_minimal() +
  labs(title = "AR(1) Process",
       x = "Time",
       y = "Value") +
  theme_stata()



# ACF and PACF plot for AR(1) ---------------------------------------------

Acf(df$ar1,
    plot = TRUE,
    na.action = na.pass)


par(mfrow = c(1 , 2))

Acf(df$ar1,
    plot = TRUE,
    na.action = na.pass)

Pacf(df$ar1,
    plot = TRUE,
    na.action = na.pass)







# MA(1) -------------------------------------------------------------------

df$ma1 <- df$y

df <- df %>%
  mutate(
    ma1 = 5 + uuid + lag(0.85 * uuid)
  )



# plot ma1 with ggplot2

ggplot(df, aes(x = t)) +
  geom_line(aes(y = ma1)) +
  theme_minimal() +
  labs(title = "MA(1) Process",
       x = "Time",
       y = "Value") +
  theme_stata()




par(mfrow = c(1 , 2))

Acf(df$ma1,
    plot = TRUE,
    na.action = na.pass)

Pacf(df$ma1,
     plot = TRUE,
     na.action = na.pass)



# Simulate MA (5) ---------------------------------------------------------
# easy and simple way in R

ma5_sim <- arima.sim(model = list(ma = c(0.55, 0.40, 0.35, 0.30, 0.25, 0.20)),
                     n = 500,
                     sd = 10)


# Plot ACF and PACF -------------------------------------------------------

Acf(ma5_sim, plot = TRUE)
Pacf(ma5_sim, plot = TRUE)












































































































































































































