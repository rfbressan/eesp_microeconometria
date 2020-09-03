#' Ferman2019 Simple Assessment
#' Author: Rafael F. Bressan
#' Date: August, 2020
#' 

#' Load libraries to make robust inference
library(dplyr)
library(microbenchmark)
library(fixest)

#' Loading the functions to perform the assessment
source("I/ferman_fun.R")

data(trade)
head(trade)
tail(trade)

#' Run a regression to fit data. Fixed effect of Year and clusters by Origin
reg <- lm(Euros ~ dist_km+as.factor(Year), data = trade)
lmtest::coeftest(reg, vcov = sandwich::vcovCL, cluster = ~Origin)

#' Run a cluster robust regression with felm
reg_rob <- lfe::felm(Euros ~ dist_km|Year|0|Origin, data = trade, cmethod = 'reghdfe')
summary(reg_rob)

#' Run regression with fixest
fix_reg <- fixest::feols(Euros ~ dist_km|Origin+Year, trade)
summary(fix_reg)

# Microbenchmark ----------------------------------------------------------

# WARNING: THE BENCHMARK MAY TAKE HOURS TO RUN!!
# bench <- microbenchmark(
#   sandwich = lm_ferman_ass(trade, "Euros ~ as.factor(Year)", "y_sim ~ dist_km+as.factor(Year)", assess_on = "dist_km", nsim = 1e3, cluster = "~Origin"),
#   fixest = fix_ferman_ass(trade, "Euros ~ as.factor(Year)|Origin", "y_sim ~ dist_km|Origin+Year", assess_on = "dist_km", nsim = 1e3, cluster = "Origin"),
#   lfe = fe_ferman_ass(trade, "Euros ~as.factor(Year)|0|0|Origin", "y_sim~dist_km|Year|0|Origin", assess_on = "dist_km", nsim = 1e3),
#   # tidy = tidy_ferman_ass(trade, "Euros ~ as.factor(Year)", "y_sim ~ dist_km+as.factor(Year)", assess_on = "dist_km", nsim = 1e3, cluster = "~Origin"),
#   times = 10)
# bench

# Unit: seconds
# expr       min        lq      mean      median        uq       max    neval
# sandwich 252.65642 255.79484 259.90028 258.33860 263.99196 273.61081  10
#   fixest  41.56166  42.11525  44.36001  43.91939  45.35654  49.00087  10
#      lfe 269.99256 272.14140 277.49892 276.21773 280.58591 291.94455  10

# Fixest is our preferred choice!

# ggplot2::autoplot(bench)
