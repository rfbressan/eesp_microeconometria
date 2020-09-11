#' Homework II - Microeconometrics I
#' Author: Rafael Felipe Bressan
#' 
#' Loading libraries
library(dplyr)
library(tidyr)
library(data.table)
library(tidyfast)
library(sandwich)
library(lmtest)

#' Reading in the dataset
data <- fread("I/input/cps_union_data.csv")

#' 1. Comparing average earning for unionized and not unionized workers
ear_na <- sum(is.na(data$earnings))
earnings_union <- data[union == 1 & !is.na(earnings), earnings]
earnings_not <- data[union == 0 & !is.na(earnings), earnings]
#' Difference in averages
avg_dif <- mean(earnings_union) - mean(earnings_not)
#' t-test for difference of means
test <- t.test(earnings_union, earnings_not, 
               alternative = "greater", var.equal = FALSE)

#' 2. Regression with covariates
#' a. Getting the covariates description from dictionary
dic <- fread("I/input/dictionary.csv", header = TRUE)
#' descriptions
desc <- dic[variable != "", .(variable, description)]
#' Select covariates
covar <- c("age", "female", "race", "marital_status", 
           "veteran", "education", "class_of_worker")
covariates <- desc[variable %in% covar]

#' b. Estimate the model
#' Cleaning and preparing data for regression
columns <- c("earnings", "union", covar)
data_cov <- data[!is.na(earnings), ..columns]
#' Missings in other variables
missings <- colSums(apply(data_cov, 2, is.na))
#' 83 missings in veteran. Let's input values to them

# Probit model to veteran -------------------------------------------------
vet_ratio <- table(data_cov$veteran)
#' Impute values to veteran
#' Do not separate data by veteran is NA or not right now
vet <- data[, -c("V1", "CPSID", "CPSIDP")]
#' Any other variable with NA?
colSums(apply(vet, 2, is.na))
#' 8636 NAs in public_housing, drop this column.
#' 19 NAs in earnings, drop only the rows
vet <- vet[, public_housing := NULL][!is.na(earnings)]
#' Make variables categorical. Numerical variables are:
num_cols <- c("age", "total_income_last_year", "wage_income_last_year", 
              "own_farm_income_last_year", "earnings")
cat_cols <- names(vet)[!(names(vet) %in% num_cols)]
vet[, (cat_cols) := lapply(.SD, as.factor), .SDcols = cat_cols]
#' All factors have two or more levels?
lapply(vet[, ..cat_cols], function(x) length(levels(x)))
#' employed has only one level. Conveys no information!
vet[, employed := NULL]
cat_cols <- cat_cols[!(cat_cols %in% "employed")]
#' All levels, for each variable, have more than one observation?
summary(vet[, ..cat_cols], maxsum = 16) # education has 16 levels
#' class_of_worker_last_year's level 29 has only one observation
#' Let's aggregate it with level 0. Level 29 is unpaid family worker and
#' best corresponds to level 0, did not work
vet[class_of_worker_last_year == 29, class_of_worker_last_year := "0"]
#' Now separate data by veteran NA or not
vet_na <- vet[is.na(veteran)]
vet_not <- vet[!is.na(veteran)]
#' Run a probit regression of veteran on all other
vet_prob <- glm(veteran~., family = binomial(link = "probit"), data = vet_not)
summary(vet_prob)
#' Predict values of veteran status to vet_na
vet_pred <- ifelse(
  predict(vet_prob, newdata = vet_na, type = "response") > vet_ratio[2]/vet_ratio[1],
  1, 0
)
#' All predictions are equal to zero, better set veteran status to zero directly
data_cov[is.na(veteran), veteran := 0]

# 2b Estimating the model -------------------------------------------------
#' Categorical variables: female, race, marital_status, veteran, education,
#' class_of_worker
num_cols <- c("earnings", "age", "union")
cat_cols <- names(data_cov)[!(names(data_cov) %in% num_cols)]
data_cov[, (cat_cols) := lapply(.SD, as.factor), .SDcols = cat_cols]
#' Check number of observations in levels of each variable
nobs_level <- data.frame(unclass(summary(data_cov[, ..cat_cols])), 
                         check.names = FALSE, stringsAsFactors = FALSE)
nobs_level[is.na(nobs_level)] <- ""
#' Estimate the model
model0 <- lm(earnings~union, data = data_cov)
model1 <- lm(earnings~., data = data_cov)
model1_coef_tbl <- coeftest(model1, vcov. = vcovHC, type = "HC0")
model0_coef_tbl <- coeftest(model0, vcov. = vcovHC, type = "HC0")
robust1.se <- model1_coef_tbl[, "Std. Error"]
robust1.pval <- model1_coef_tbl[, "Pr(>|t|)"]
robust0.se <- model0_coef_tbl[, "Std. Error"]
robust0.pval <- model0_coef_tbl[, "Pr(>|t|)"]

# 2c Weights --------------------------------------------------------------
#' Regression of union on all covariates
union_cov <- lm(union~.-earnings, data_cov)
resid_union_cov <- residuals(union_cov)
ssr_union_cov <- deviance(union_cov)
#' Compute the weight vector
union_vec <- data_cov[, union]
weight <- (union_vec + resid_union_cov*(1 - 2*union_vec))/ssr_union_cov
data_cov[, weight := weight] # column bind weight to data_cov
#' Summary statistics
w_summary <- data_cov[, 
                      lapply(.SD, function(x){
                        t <- summary(x)
                        n <- names(t)
                        return(list(data.frame(stat = n, value = as.numeric(t), 
                                               stringsAsFactors = FALSE)))
                      }), 
                      by = union, .SDcols = "weight"]

w_summary <- dt_unnest(w_summary, weight) %>% 
  dcast(stat~union, value.var = "value")
#' Do the weights sum to one in control?
data_cov[, sum(.SD), by = union, .SDcols = "weight"]
#' negative values?
data_cov[, min(.SD), by = union, .SDcols = "weight"]


save.image("I/input/homework_II.RData")
