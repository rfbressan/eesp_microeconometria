#' Homework II - Microeconometrics I
#' Author: Rafael Felipe Bressan
#' Matching packages available and useful companions:
#' MatchIt, Matching, twang, CBPS, ebal, designmatch, MatchThem, optmatch,
#' WeightIt, sbw
#' Loading libraries
library(dplyr)
library(tidyr)
library(broom)
library(data.table)
# library(dtplyr)
# library(tidyfast)
library(sandwich)
library(lmtest)
library(modelsummary)
library(Matching)
library(hdm)

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

#' First prepare a dataset with all variables
#' remove NAs on earnings, set veterans NAs to zero, remove unnecessary columns,
#' change marital status to married (1), not married (0)
#' change race to white (1), not white (0)
#' change to categorical variables
cat_cols_full <- c("class_of_worker", "class_of_worker_last_year")
data_full <- data[!is.na(earnings), ][
  is.na(veteran), veteran := 0][
    , c("V1", "CPSID", "CPSIDP", "public_housing", "employed") := NULL][
      , `:=`(marital_status = ifelse(marital_status %in% c(1, 2), 1, 0),
             race = ifelse(race == 1, 1, 0))][
      , (cat_cols_full) := lapply(.SD, factor), .SDcols = cat_cols_full]
#' Insert a column for age^2
data_full[, age_2 := age^2]
#' Gotta standardize variables total_income_last_year, wage_income_last_year and
#' own_farm_income_last_year
standardize <- function(x) {
  (x - mean(x)) / sd(x)
}
sd_cols <- c("total_income_last_year", "wage_income_last_year", 
             "own_farm_income_last_year")
data_full[, (sd_cols) := lapply(.SD, standardize), .SDcols = sd_cols]

#' Number of observations inside each level of a factor variable
nobs_level <- data.frame(unclass(summary(data_full[, ..cat_cols_full], maxsum = 10)), 
                         check.names = FALSE, stringsAsFactors = FALSE)
nobs_level[is.na(nobs_level)] <- ""
#' class_of_worker_last_year 29 has only one observation. Set it to level 0 and
#' drop level 29 from this factor
data_full[class_of_worker_last_year == 29, class_of_worker_last_year := "0"]
data_full[, class_of_worker_last_year := droplevels(class_of_worker_last_year)]

#' 2. Regression with covariates
#' a. Getting the covariates description from dictionary
dic <- fread("I/input/dictionary.csv", header = TRUE)
#' descriptions
desc <- dic[variable != "", .(variable, description)]
#' Select covariates
covar <- c("age", "age_2", "female", "race", "marital_status", 
           "veteran", "education", "class_of_worker")
covariates <- desc[variable %in% covar]

#' b. Estimate the model
#' Cleaning and preparing data for regression
columns <- c("earnings", "union", covar)
data_cov <- data_full[, ..columns]
#' Missings in other variables
missings <- colSums(apply(data, 2, is.na))
#' 83 missings in veteran. Let's input values to them

# Probit model to veteran -------------------------------------------------
vet_ratio <- table(data$veteran)
#' #' Impute values to veteran
#' #' Do not separate data by veteran is NA or not right now
#' vet <- data[, -c("V1", "CPSID", "CPSIDP")]
#' #' Any other variable with NA?
#' colSums(apply(vet, 2, is.na))
#' #' 8636 NAs in public_housing, drop this column.
#' #' 19 NAs in earnings, drop only the rows
#' vet <- vet[, public_housing := NULL][!is.na(earnings)]
#' #' Make variables categorical. Numerical variables are:
#' num_cols <- c("veteran", "age", "total_income_last_year", "wage_income_last_year", 
#'               "own_farm_income_last_year", "earnings")
#' cat_cols <- names(vet)[!(names(vet) %in% num_cols)]
#' vet[, (cat_cols) := lapply(.SD, as.factor), .SDcols = cat_cols]
#' #' All factors have two or more levels?
#' lapply(vet[, ..cat_cols], function(x) length(levels(x)))
#' #' employed has only one level. Conveys no information!
#' vet[, employed := NULL]
#' cat_cols <- cat_cols[!(cat_cols %in% "employed")]
#' #' All levels, for each variable, have more than one observation?
#' summary(vet[, ..cat_cols], maxsum = 16) # education has 16 levels
#' #' class_of_worker_last_year's level 29 has only one observation
#' #' Let's aggregate it with level 0. Level 29 is unpaid family worker and
#' #' best corresponds to level 0, did not work
#' vet[class_of_worker_last_year == 29, class_of_worker_last_year := "0"]
#' #' Now separate data by veteran NA or not
#' vet_na <- vet[is.na(veteran)]
#' vet_not <- vet[!is.na(veteran)]
#' #' Run a probit regression of veteran on all other
#' vet_prob <- glm(veteran~., family = binomial(link = "probit"), data = as.data.frame(vet_not))
#' summary(vet_prob)
#' #' Predict values of veteran status to vet_na
#' vet_pred <- ifelse(
#'   predict(vet_prob, newdata = vet_na, type = "response") > vet_ratio[2]/vet_ratio[1],
#'   1, 0
#' )
#' All predictions are equal to zero, better set veteran status to zero directly
# data_cov[is.na(veteran), veteran := 0]

# 2b Estimating the model -------------------------------------------------
#' Categorical variables: race, marital_status, class_of_worker
# num_cols <- c("earnings", "age", "age_2", "union", "female", "veteran", "education")
cat_cols <- cat_cols_full[cat_cols_full %in% names(data_cov)]
# data_cov[, (cat_cols) := lapply(.SD, as.factor), .SDcols = cat_cols]

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
weight <- (resid_union_cov*(2*union_vec - 1))/ssr_union_cov
data_cov[, weight := weight] # column bind weight to data_cov
#' Summary statistics
w_summary <- datasummary(Mean+Median+SD+Min+Max+sum ~ weight*factor(union), 
                         data = data_cov, 
                         fmt = "%.6f", 
                         output = 'data.frame')
#' Do the weights sum one in control?
data_cov[, sum(.SD), by = union, .SDcols = "weight"]
#' negative values?
data_cov[, min(.SD), by = union, .SDcols = "weight"]

# 4 Balance checks --------------------------------------------------------
#' First the categorical variables
cat_form <- paste0(
  paste0(cat_cols, collapse = "+"), "~factor(union)*((N=1)+Percent('col'))"
)
cat_balance <- datasummary(as.formula(cat_form),
                           data = data_cov[, c("union", ..cat_cols)],
                           output = 'data.frame')
#' Then numerical variables
num_bal <- data_cov[, -c("earnings", "weight", ..cat_cols)][
  , .(names = colnames(.SD), mean = colMeans(.SD), var = sapply(.SD, var)), 
  by = "union"] %>% 
  dcast(names~union, value.var = c("mean", "var"))
num_bal[, norm_diff := .(abs(mean_1 - mean_0)/sqrt((var_1 + var_0)/2))]

num_balance <- datasummary_balance(~union, 
                                   data = data_cov[, -c("earnings", "weight", ..cat_cols)],
                                   output = 'data.frame') %>% 
  mutate(norm_diff = format(num_bal$norm_diff, digits = 2))

# 5 Select covariates -----------------------------------------------------

# Stepwise model selection - Imbens and Rubin -----------------------------
# Imbens and Rubin's stepwise selection algorithm
# treatment: character variable for treatment indicator variable
# Xb: character vector with names of basic covariates: you may pass it as  c() if you do not want any basic covariate
# Xt: character vector with names for covariates to be tested for inclusion
# data: dataframe with variables
# Clinear: threshold, in terms of likelihood ratio statistics, for inclusion of linear terms
# Cquadratic: threshold, in terms of likelihood ratio statistics, for inclusion of quadratic/interaction terms
# Intercept: does model include intercept?
# Author: Luis Alvarez
# Modifications: Rafael F. Bressan
ir_stepwise <- function(treatment, Xb, Xt, data, Clinear = 1, Cquadratic = 2.71, intercept = TRUE)
{
  #Add or not intercept
  if (intercept)
    inter.add = "1" 
  else inter.add = "-1"
    
    
    #Formula for model
    if (length(Xb) == 0)
      formula = paste(treatment, inter.add, sep = " ~ ") else formula = paste(treatment, paste(c(inter.add,Xb), collapse = " + "), sep = " ~ ")
      
      continue = TRUE
      
      Xt_left = Xt
      # First order inclusion
      while (continue) {
        null.model = glm(as.formula(formula), data, family = "binomial")
        
        null.lkl = logLik(null.model)
        
        test.stats = c()
        for (covariate in Xt_left)
        {
          formula.test = paste(formula, covariate, sep = " + ")
          test.model = glm(as.formula(formula.test), data, family = "binomial")
          
          lkl.ratio = 2*(as.numeric(logLik(test.model)) - as.numeric(null.lkl))
          test.stats = c(test.stats, lkl.ratio)
        }
        
        if (max(test.stats,na.rm = TRUE) < Clinear)
          continue = FALSE else {
            
            add.coef = Xt_left[which.max(test.stats)]
            
            formula = paste(formula, add.coef, sep = " + ")
            
            Xt_left = Xt_left[-which.max(test.stats)]
          }
        
      }
      
      #Defining Xstar set. Set of first order included variables
      Xstar = c(Xb, Xt[!(Xt %in% Xt_left)])
      
      #Creating all combinations of Xstar interactions
      combinations = expand.grid(Xstar, Xstar)
      Xcomb = paste(combinations[,1],combinations[,2],sep = ":")
      
      continue = TRUE
      
      Xcomb_left = Xcomb
      
      while (continue) {
        null.model = glm(as.formula(formula), data, family = "binomial")
        
        null.lkl = logLik(null.model)
        
        test.stats = c()
        for (covariate in Xcomb_left)
        {
          formula.test = paste(formula, covariate, sep = " + ")
          test.model = glm(as.formula(formula.test), data, family = "binomial")
          
          lkl.ratio = 2*(as.numeric(logLik(test.model)) - as.numeric(null.lkl))
          test.stats = c(test.stats, lkl.ratio)
        }
        
        if (max(test.stats,na.rm = TRUE) < Cquadratic)
          continue = FALSE else {
            
            add.coef = Xcomb_left[which.max(test.stats)]
            
            formula = paste(formula, add.coef, sep = " + ")
            
            Xcomb_left = Xcomb_left[-which.max(test.stats)]
          }
        
      }
      
      return(list(formula = formula,
                  inc_x = Xstar))
}

#' own_farm_income_last_year is mostly zero and has an outlier. Better not use it
xt <- names(data_full)[! names(data_full) %in% 
                         c(covar, "earnings", "union", "own_farm_income_last_year")]
#' TEST ONLY: Select a random sample of full data for quick results
#' set.seed(1234)
#' s <- sample(nrow(data_full), 2000)
ir_form <- ir_stepwise("union", covar, xt, data = data_full)
ps_ir <- glm(as.formula(ir_form$formula), family = "binomial", 
             data = data_full[, -c("earnings")])
terms_ir <- attr(terms(ps_ir), "term.labels")

# Model selection via Lasso 
x_lasso <- names(data_full)[! names(data_full) %in% c("earnings", "union")]
#' using mlr package to create dummies for every factor variable
# dt_lasso <- mlr::createDummyFeatures(as.data.frame(data_full[, -c("earnings")]),
                                     # method = "reference")
ps_lasso <- rlassologit(union~(.)^2, 
                        data = data_full[, -c("earnings", "own_farm_income_last_year")])
summary(ps_lasso, all = FALSE)
terms_lasso <- names(coef(ps_lasso))[ps_lasso$index]

#' Elastic net regularization with package glmnet
# ps_net <- glmnet::glmnet(
#   Matrix::sparse.model.matrix(union~(.)^2, 
#                               data_full[, -c("earnings", "own_farm_income_last_year")]), 
#   data_full$union, 
#   family = "binomial"
# ) 

#' Model with full set of covariates!
ps_all <- glm(union~(.)^2, family = "binomial",
              data = data_full[, -c("earnings", "own_farm_income_last_year")])
terms_all <- attr(terms(ps_all), "term.labels")

#' data table with union and different latent indices based on estimation method
dt_ps <- data.table(model = c("Imbens-Rubin", "Lasso", "Full"),
                    union = list(data_full$union),
                    ps = list(predict(ps_ir, type = 'response'), 
                              as.vector(predict(ps_lasso, type = 'response')), 
                              predict(ps_all, type = 'response')),
                    li = list(predict(ps_ir),
                              as.vector(predict(ps_lasso, type = 'link')),
                              predict(ps_all)))
 
li_bal <- dt_ps[
  , .(union = unlist(union), 
      li = unlist(li),
      ps = unlist(ps))
  , by = model][
    , .(mean = sapply(.SD, mean), var = sapply(.SD, var))
    , by = .(union, model), .SDcols = "li"] %>% 
  dcast(model~union, value.var = c("mean", "var"))
li_bal[, norm_diff := .(abs(mean_1 - mean_0)/sqrt((var_1 + var_0)/2))]
setcolorder(li_bal, c("model", "mean_0", "var_0", "mean_1", "var_1", "norm_diff"))

# 6 Quality of PS ---------------------------------------------------------
#Function that subdivides a given propensity score vector in subblocks
#treat = vector with treatment assignments
#lin.psm = vector with linearized PSs
#K = how many covariates will we want to test/use in bias correction of estimates later on? 
#t.max = threshold for tstat in making a further subdivide 
#trim = should we discard extreme observations so there is overlap?
#' Author: Luis Alvarez
ps_blocks <- function(treat, lin.psm, K, t.max = 1.96,  trim = TRUE)
{
  if(trim){
    b0 = min(plogis(lin.psm[treat==1]))
    b1 = max(plogis(lin.psm[treat==0]))
  } else
  {
    b0 = 0
    b1 = 1
  }
  b_vec = c(b0,b1)
  while (TRUE)
  {
    J = length(b_vec)-1
    b_vec_new = do.call(c,lapply(1:J, function(j){
      sample = (b_vec[j] <= plogis(lin.psm)) & (plogis(lin.psm) < b_vec[j+1])
      
      ps.treat = lin.psm[sample&treat==1]
      ps.control = lin.psm[sample&treat==0]
      
      #print(length(ps.control))
      #print(length(ps.treat))
      
      t.test.pass = tryCatch({abs(t.test(ps.control, ps.treat)$statistic) > t.max}, error = function(e){return(F)})
      
      med.val = median(c(ps.treat, ps.control))
      
      Nt.below = sum(ps.treat < med.val)
      Nt.above = sum(ps.treat >= med.val)
      Nc.below = sum(ps.control < med.val)
      Nc.above = sum(ps.control >= med.val)
      
      sample.crit = min(Nt.below, Nt.above, Nc.below, Nc.above) >= max(3, K+2)
      
      if(t.test.pass&sample.crit)
        return(c(b_vec[j], plogis(med.val), b_vec[j+1])) else return(c(b_vec[j], b_vec[j+1]))
      
    }))
    b_vec_new = unique(b_vec_new)
    
    #print(length(b_vec_new))
    if(length(b_vec_new)==length(b_vec))
      break else b_vec = b_vec_new
  }
  
  #Constructing blocking variable now
  block_var = rep(NA, length(treat))
  
  for (j in 1:(length(b_vec) - 1))
    block_var[(b_vec[j] <= plogis(lin.psm)) & (plogis(lin.psm) < b_vec[j+1])] = j
  #' Propensity scores lower than b0 will be in block -2 and PS higher than or 
  #' equal to b1 will be block -1
  # block_var[plogis(lin.psm) < b0] <- -2
  # block_var[plogis(lin.psm) >= b1] <- -1
  
  return(block_var)
}

#' Appending latent indices to data_full, then computing blocks
k_ir <- length(terms_ir)
k_lasso <- length(terms_lasso)
k_all <- length(terms_all)
cols_full <- names(data_full)[
  ! names(data_full) %in% c("earnings", "own_farm_income_last_year", "li_ir",
                            "li_lasso", "li_all", "block_ir", "block_lasso", 
                            "block_all")]
num_cols_full <- cols_full[! cols_full %in% cat_cols_full]

data_full[, `:=`(li_ir = predict(ps_ir, type = 'link'),
                 li_lasso = as.vector(predict(ps_lasso, type = 'link')),
                 li_all = predict(ps_all, type = 'link'))][
                   , `:=`(block_ir = ps_blocks(union, li_ir, k_ir),
                          block_lasso = ps_blocks(union, li_lasso, k_lasso),
                          block_all = ps_blocks(union, li_all, k_all))]

#' Assessing balance of covariates using the three methods by Imbens-Rubin
#' balance table: t-test and normalized difference -------------------------
table.test <- function(data, covariates, treat_var, col_ret = "all")
{
  treat_vec = data[, treat_var]
  table = c()
  
  for (lab in covariates)
  {
    cov_vec = data[,lab]
    control = cov_vec[treat_vec == 0 & !is.na(treat_vec)]
    treatment = cov_vec[treat_vec == 1 & !is.na(treat_vec)]
    
    normalized_diff = (mean(treatment) - mean(control))/sqrt((var(treatment) + var(control))/2)
    
    table.line = cbind( "meanc" = mean(control), 
                        "meant" = mean(treatment), 
                        "tstat" = tryCatch({t.test(control, x = treatment)$statistic}, 
                                           error = function(e){NaN}), 
                        "norm.diff" = normalized_diff)
    
    table = rbind(table, table.line)
  }
  
  table <- as.data.frame(table)
  rownames(table) <- NULL
  
  if (col_ret[1] == "all") 
    return(cbind(covar = covariates, table))
  else  
    return(cbind(covar = covariates, table[, col_ret, drop = FALSE]))
}
#' Third approach, single covariate, single stratum at a time
#' First we need to expand factor variables into dummies
tbl_covar <- num_cols_full[num_cols_full != "union"]

summary_block_ir <- 
  model.matrix(~.-1, data = data_full[, c(..cols_full, "block_ir")]) %>% 
  na.omit() %>% 
  as_tibble() %>% 
#' Using table.test by blocks and only on numerical variables
  group_by(block_ir) %>% 
  summarise(t_stat = list(table.test(cur_data(), names(cur_data()), "union", "tstat"))) %>% 
  unnest(t_stat) %>% 
  filter(covar != "union") %>% 
  pivot_wider(covar, names_from = block_ir, values_from = tstat)

summary_block_lasso <- 
  model.matrix(~.-1, data = data_full[, c(..cols_full, "block_lasso")]) %>% 
  na.omit() %>% 
  as_tibble() %>% 
  group_by(block_lasso) %>% 
  summarise(t_stat = list(table.test(cur_data(), names(cur_data()), "union", "tstat"))) %>% 
  unnest(t_stat) %>% 
  filter(covar != "union") %>% 
  pivot_wider(covar, names_from = block_lasso, values_from = tstat)

summary_block_all <- 
  model.matrix(~.-1, data = data_full[, c(..cols_full, "block_all")]) %>% 
  na.omit() %>% 
  as_tibble() %>% 
  group_by(block_all) %>% 
  summarise(t_stat = list(table.test(cur_data(), names(cur_data()), "union", "tstat"))) %>% 
  unnest(t_stat) %>% 
  filter(covar != "union") %>% 
  pivot_wider(covar, names_from = block_all, values_from = tstat)

#' Many of class_of_worker has problems in the t-stat. Probably due to lack of 
#' observations
obs_block_ir <- model.matrix(~.-1, data = data_full[, c(..cols_full, "block_ir")]) %>% 
  na.omit() %>% 
  as_tibble() %>% 
  dplyr::select(matches("class_of_worker"), "block_ir") %>% 
  group_by(block_ir) %>% 
  summarise(across(everything(), sum))
#' Indeed many strata of class_of_worker type of covariate do not have any
#' observation for a given block, thus the statistics can't be computed.
#' But this is mainly due to the attribution for the NA block, which we are
#' removing from balance assessment.  
#' 
#' Re-blocking!! To assess number of treated and control by blocks with trim
#' option set to false. This is the blocking config we will pass to the forthcomming
#' subclassification procedure.
data_full[, `:=`(block_ir = ps_blocks(union, li_ir, k_ir, trim = FALSE),
                 block_lasso = ps_blocks(union, li_lasso, k_lasso, trim = FALSE),
                 block_all = ps_blocks(union, li_all, k_all, trim = FALSE))]

# 7 Trim the sample -------------------------------------------------------
#' Author: Luis Alvarez
#' Adapted by: Rafael Bressan
trimming.imbens2 <- function(lin.ps)
{
  inv.vec = 1/(plogis(lin.ps)*(1 - plogis(lin.ps)))
  
  if (max(inv.vec) <= 2*mean(inv.vec))
  {
    print("No trimming")
    return(rep(TRUE, length(lin.ps))) 
  }else {
    # value function
    value_fun <- function(gamma) {
      2*sum(inv.vec[inv.vec <= gamma])  - gamma*sum(inv.vec <= gamma)
    }
    # root finding. g is a list with root
    g <- uniroot(value_fun, c(min(inv.vec), max(inv.vec)))
    
    alpha.trim <- 1/2 - sqrt(1/4 - 1/g$root)
    print(paste("Trimming threshold alpha is ",alpha.trim))
    return(plogis(lin.ps) <= 1 - alpha.trim &  plogis(lin.ps) >= alpha.trim)
  }
}

trim_idx_ir <- trimming.imbens2(data_full$li_ir)
trim_idx_lasso <- trimming.imbens2(data_full$li_lasso)
trim_idx_all <- trimming.imbens2(data_full$li_all)

#' Add trimming indexes to dt_ps
dt_ps[, trim_idx := list(trim_idx_ir, trim_idx_lasso, trim_idx_all)]

trim_bal_ir <- table.test(as.data.frame(data_full[trim_idx_ir]),
                          c(num_cols_full, "li_ir"), "union") %>% 
  filter(covar != "union")
trim_bal_lasso <- table.test(as.data.frame(data_full[trim_idx_lasso]),
                          num_cols_full, "union") %>% 
  filter(covar != "union")
trim_bal_all <- table.test(as.data.frame(data_full[trim_idx_all]),
                          num_cols_full, "union") %>% 
  filter(covar != "union")

#' Table for balance of latent indices before and after trimming
no_trim_li <- dt_ps %>% 
  as_tibble() %>% 
  dplyr::select(-c(ps, trim_idx)) %>% 
  unnest(c(union, li)) %>% 
  group_by(model, union) %>% 
  summarise(avg = mean(li), vari = var(li)) %>% 
  mutate(diff = avg - lag(avg), 
         sqrt = sqrt((vari + lag(vari))/2),
         norm_diff = diff / sqrt) %>% 
  na.omit()
trim_li <- dt_ps %>% 
  as_tibble() %>% 
  dplyr::select(-ps) %>% 
  unnest(c(union, li)) %>% 
  filter(c(trim_idx_ir, trim_idx_lasso, trim_idx_all)) %>% 
  group_by(model, union) %>% 
  summarise(avg = mean(li), vari = var(li)) %>% 
  mutate(diff = avg - lag(avg), 
         sqrt = sqrt((vari + lag(vari))/2),
         norm_diff = diff / sqrt) %>% 
  na.omit()
trim_bal_li <- no_trim_li %>% 
  left_join(trim_li, by = "model", suffix = c("_no_trim", "_trim")) %>% 
  dplyr::select(matches("norm_diff")) %>% 
  arrange(model)

# 8 Estimating effects by subclassification -----------------------------------
#' 
#' @param dt data.table
#' @param outcome character string, name of outcome variable
#' @param treat character string, name of treatment variable
#' @param block character string, name of block variable
#' @param controls Optional. Vector of character strings, name of control 
#' variables
#' @return list containing a data.table called blocks with all weights, standard
#' deviations and effects by blocks, and a data.table called effects with ATE
#' and ATT (and their robust standard errors).
#' @author Rafael Bressan
subclassification <- function(dt, outcome, treat, block, controls = NULL) {
  stopifnot(is.data.table(dt))
  
  K <- length(controls)
  #' regression formula
  if (K == 0)
    form <- paste(outcome, treat, sep = " ~ ") 
  else 
    form <- paste(outcome, paste(c(treat, controls), collapse = "+"), 
                  sep = " ~ ")
  #' Number of observations
  nobs <- nrow(dt)
  nb <- dt[, .(nb = .N), by = c(block)]
  nt <- dt[get(treat) == 1, .(nt = .N), by = c(block)]
  nc <- dt[get(treat) == 0, .(nc = .N), by = c(block)]
  dt_n <- nb[nt, on = c(block)][nc, on = c(block)][
    , `:=`(w_ate = nb/nobs,
           w_att = nt/sum(nt))]
  #' DT to hold results by block
  reg_block <- dt[, .(reg = list(lm(as.formula(form), data = .SD)))
                  , by = c(block)][
                    , `:=`(tau_b = coef(reg[[1]])[treat],
                           rob_se = sqrt(sandwich::vcovHC(reg[[1]])[treat, treat]))
                    , by = c(block)]
  
  dt_out <- dt_n[reg_block[, c(..block, "tau_b", "rob_se")], on = c(block)]
  setnames(dt_out, block, "block")
  ate <- sum(dt_out$w_ate * dt_out$tau_b)
  ate_se <- sqrt(sum((dt_out$w_ate * dt_out$rob_se)^2))
  att <- sum(dt_out$w_att * dt_out$tau_b)
  att_se <- sqrt(sum((dt_out$w_att * dt_out$rob_se)^2))
  dt_eff <- data.table(stat = c("mean", "se"), 
                       ate = c(ate, ate_se), 
                       att = c(att, att_se))
  
  return(list(blocks = dt_out, effects = dt_eff))
}

#' Subclassification without any controlling covariate
sub_nc_ir <- subclassification(data_full[trim_idx_ir],
                               "earnings", "union", "block_ir")
sub_nc_lasso <- subclassification(data_full[trim_idx_lasso],
                                  "earnings", "union", "block_lasso")
sub_nc_all <- subclassification(data_full[trim_idx_all],
                                "earnings", "union", "block_all")
#' Subclassification with imbalanced controls: age, education and 
#' private_health_insurance
sub_ir <- subclassification(
  data_full[trim_idx_ir],
  "earnings", "union", "block_ir",
  controls = c("age", "education", "private_health_insurance"))
sub_lasso <- subclassification(
  data_full[trim_idx_lasso],
  "earnings", "union", "block_lasso",
  controls = c("age", "education", "private_health_insurance"))
sub_all <- subclassification(
  data_full[trim_idx_all],
  "earnings", "union", "block_all",
  controls = c("age", "education", "private_health_insurance"))
#' Table with number of treated and control by block and model
ntc_ir <- sub_nc_ir$blocks[, c("block", "nt", "nc")]
ntc_lasso <- sub_nc_lasso$blocks[, c("block", "nt", "nc")]
ntc_all <- sub_nc_all$blocks[, c("block", "nt", "nc")]
#' Merge tables
ntc_block <- 
  merge(ntc_ir, ntc_lasso, by = "block", all = TRUE) %>% 
  merge(ntc_all, by = "block", all = TRUE)
#' ATT and ATE for three models without controlling covariates
sub_effect_nc <- 
  merge(sub_nc_ir$effects, sub_nc_lasso$effects, by = "stat") %>% 
  merge(sub_nc_all$effects, by = "stat")
#' ATT and ATE for three models with controlling covariates
sub_effect <- 
  merge(sub_ir$effects, sub_lasso$effects, by = "stat") %>% 
  merge(sub_all$effects, by = "stat")

# 9 Estimate effects with Matching ----------------------------------------
#' Compute estimates by matching PS
match_nc_effect <- dt_ps %>% 
  as_tibble() %>% 
  unnest(effect) %>% 
  rowwise() %>% 
  mutate(match_nc = list(Match(Y = data_full[trim_idx, earnings],
                               Tr = data_full[trim_idx, union],
                               X = li[trim_idx],
                               estimand = effect,
                               M = 1, replace = TRUE)),
         coef_nc = list(tibble(estimate = match_nc["est"], 
                               se = match_nc["se"],
                               orig.nobs = match_nc["orig.nobs"],
                               orig.treated.nobs = match_nc["orig.treated.nobs"],
                               match.obs = match_nc["orig.wnobs"]))) %>% 
  dplyr::select(model, effect, coef_nc)

match_nc <- match_nc_effect %>% 
  unnest(coef_nc) %>% 
  pivot_wider(id_cols = model, 
              names_from = effect, 
              values_from = c(estimate:match.obs)) %>% 
  dplyr::select(model, matches("_ATE$"), matches("_ATT$"))

# 10 IPW doubly-robust estimation -----------------------------------------
#' Overall probability of treatment. For ATT estimation
p_treat <- mean(data_full$union)
#' Add column "effect"
dt_ps[, effect := list(c("ATE", "ATT"))]

ipw_weights <- dt_ps %>% 
  as_tibble() %>% 
  dplyr::select(-li) %>% 
  unnest(effect) %>% 
  rowwise() %>% 
  mutate(
    weight = list(case_when(
      effect == "ATE" ~ ifelse(union == 1, 1/ps, 1/(1 - ps)),
      effect == "ATT" ~ (1/p_treat)*ifelse(union == 1, 1, ps/(1 - ps)),
      TRUE ~ 1.0 # no weighting
    )))

#' regressions
ipw_reg <- ipw_weights %>% 
  mutate(lm_fit = list(lm(earnings~union, 
                          data = data_full[trim_idx], 
                          weights = weight[trim_idx])),
         coef_rob = list(coeftest(lm_fit)),
         est = list(tidy(coef_rob))) %>% 
  dplyr::select(model, effect, est) %>% 
  unnest(est) %>% 
  filter(term == "union") %>% 
  pivot_wider(id_cols = model, 
              names_from = effect, 
              values_from = c(estimate:p.value)) %>% 
  dplyr::select(model, estimate_ATE, std.error_ATE, estimate_ATT, std.error_ATT)




save.image("I/input/homework_II.RData")
