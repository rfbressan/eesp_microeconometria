#' Homework I - Microeconometrics II
#' Author: Rafael Felipe Bressan
#' 
#' Loading libraries
library(tidyverse)
# library(broom)
library(sandwich)
library(lmtest)
library(modelsummary)
library(fixest)
library(AER)
library(dtplyr)
library(rdrobust)
library(rddensity)
library(rdd)
library(foreach)
library(doParallel)


#' Part 1 Instrumental variables
#' Load data
lg_census2010 <- read_csv("II/input/censo_data_family_size.csv")
#' Exploratory analysis
summary(lg_census2010)
#' First child years of education has 16228 NAs. This is our outcome.
lg_census2010 <- lg_census2010 %>% 
  filter(!is.na(first_child_years_of_education))

#' Question 1
#' b) Table with average years of schooling by number of children and frequency
#' of number of children. Use sample weight (person_weight)
educ_tab <- lg_census2010 %>% 
  group_by(family_number_children) %>% 
  summarise(frequency = sum(person_weight),
            avg_school = sum(first_child_years_of_education*person_weight)/frequency) %>% 
  select(family_number_children, avg_school, frequency)
educ_tab
#' Total population represented
sum(educ_tab$frequency)
#'
#' c) ATE lower bound
#' 
#' ManskiPepper2000 table 1
manski_tbl1 <- lg_census2010 %>% 
  select(c(first_child_years_of_education, family_number_children, 
           person_weight)) %>% 
  group_by(family_number_children) %>% 
  summarise(average = weighted.mean(first_child_years_of_education, person_weight), 
            prob = sum(person_weight)/sum(lg_census2010$person_weight),
            size = sum(person_weight)) %>% 
  arrange(family_number_children)
#' Manski bounds with MTR and MTS
ate_ub <- function(data, outcome, treatment, treat_levels, weight) {
  treat_levels <- sort(treat_levels)
  treat_vec <- data %>% 
    distinct({{treatment}}) %>% 
    arrange({{treatment}}) %>% 
    pull()
  if (!all(treat_levels %in% treat_vec))
    stop("All treatment levels must be in the data.")
  # Conditional expectations and probabilities. 
  # data must be the manski_tbl1 format
  sum_weights <- data %>% select({{weight}}) %>% pull() %>% sum() 
  exp_probs <- data %>% 
    select(c({{outcome}}, {{treatment}}, {{weight}})) %>% 
    group_by({{treatment}}) %>% 
    summarise(average = weighted.mean({{outcome}}, {{weight}}), 
              prob = sum({{weight}})/sum_weights,
              size = sum({{weight}})) %>% 
    arrange({{treatment}})
  
  ub <- vector("numeric", length = length(treat_levels))
  n_treats <- nrow(exp_probs)

  for (i in seq_along(treat_levels)) {
    t <- treat_levels[i]
    t_idx <- which(treat_vec == t)
    s <- treat_vec[t_idx - 1]
    sum_t_plus <- exp_probs %>% 
      filter({{treatment}} > t) %>% 
      summarise(sum(average*prob)) %>% 
      pull()
    sum_s_less <- exp_probs %>% 
      filter({{treatment}} < s) %>% 
      summarise(sum(average*prob)) %>% 
      pull()
    exp_y_t <- exp_probs %>% 
      filter({{treatment}} == t) %>% 
      pull(average)
    exp_y_s <- exp_probs %>% 
      filter({{treatment}} == s) %>% 
      pull(average)
    prob_t_leq <- exp_probs %>% 
      filter({{treatment}} <= t) %>% 
      summarise(sum(prob)) %>% 
      pull()
    prob_s_geq <- exp_probs %>% 
      filter({{treatment}} >= s) %>% 
      summarise(sum(prob)) %>% 
      pull()
    upper_bound <- sum_t_plus + exp_y_t * prob_t_leq - (sum_s_less + exp_y_s * prob_s_geq)
    
    ub[i] <- upper_bound
  }
  
  return(data.frame(treat_levels = treat_levels, upper_bound = ub))
}
#' Estimate of LB
#' Wrong sign. Just checking
ate_lb_est <- ate_ub(lg_census2010, first_child_years_of_education, 
                     family_number_children, (2:16), person_weight) %>% 
  rename(lb_est = upper_bound)
#' Changing sign They are the same!! Compute upper bound and say it is lower 
#' is the same as changing signs, compute UB then change sign back again to 
#' lower
#' 
#' Bootstrapping for Lower Bound confidence
#' Results in unique households and their weights

lg_household <- lg_census2010 %>% 
  select(id_household, household_weight) %>% 
  group_by(id_household) %>% 
  summarise(household_weight = first(household_weight))

nrep <- 200
n_sample <- nrow(lg_household)
#' Using foreach
#' Register doParallel backend for parallel computation of bootstrap
cl <- makePSOCKcluster(3)
registerDoParallel(cl)

ate_lb_df <- foreach(i = 1:nrep, .combine = "rbind", .packages = "dplyr") %dopar% {
  boot <- sample(n_sample, replace = TRUE, 
                 # prob = lg_household$household_weight
  )
  
  lg_hh_boot <- lg_household[boot, "id_household"] %>% 
    left_join(lg_census2010 %>% 
                select(id_household, first_child_years_of_education, 
                       family_number_children, person_weight), 
              by = "id_household")
  
  lg_hh_boot %>% 
    ate_ub(first_child_years_of_education, family_number_children, (2:6), 
           person_weight)
}
stopCluster(cl)
gc() # Garbage collector to clean up memory

#' Faceted plot of lower bounds histograms
lb_hist <- ggplot(ate_lb_df, aes(upper_bound, group = treat_levels)) +
  geom_histogram(bins = 20) +
  facet_wrap(~treat_levels, scales = "free") +
  labs(x = "Lower bound") +
  theme_classic()

manski_tbl2 <- ate_lb_df %>% 
  group_by(treat_levels) %>% 
  summarise(quant05 = quantile(upper_bound, probs = 0.05)) %>% 
  mutate(s_treat = treat_levels - 1) %>% 
  left_join(ate_lb_est, by = "treat_levels") %>% 
  select(s_treat, treat_levels, lb_est, quant05)

#' Question 2
#' a) Multiple births in second birth (second child are twins)
lg_two_plus_births <- lg_census2010 %>% 
  filter(family_number_births >= 2)
n_two_plus_births <- nrow(lg_two_plus_births)
n_second_multiple <- sum(lg_two_plus_births$second_birth_ismultiplebirth, 
                         na.rm = TRUE)
#' b) IV Estimation
iv_est0 <- ivreg(
  first_child_years_of_education~family_number_children
  |second_birth_ismultiplebirth,
  weights = person_weight,
  data = lg_two_plus_births)
cl_vcov0 <- vcovCL(iv_est0, cluster = ~id_household)
iv_est1 <- ivreg(
  first_child_years_of_education~family_number_children+first_child_age+
    first_child_is_girl+first_birth_ismultiplebirth
  |second_birth_ismultiplebirth+first_child_age+first_child_is_girl+
    first_birth_ismultiplebirth,
  weights = person_weight,
  data = lg_two_plus_births)
cl_vcov1 <- vcovCL(iv_est1, cluster = ~id_household)
#' F-test for instrument relevance
f_test0 <- summary(iv_est0, diagnostics = TRUE, 
                   vcov. = vcovCL, cluster = id_household)
f_test0 <- f_test0$diagnostics['Weak instruments', 'statistic']
f_test1 <- summary(iv_est1, diagnostics = TRUE, 
                   vcov. = vcovCL, cluster = id_household)
f_test1 <- f_test1$diagnostics['Weak instruments', 'statistic']
#' c) Weak instrument test
#Code to implement the Olea Pflueger correction:
#FS is
#x= Z\pi +  R \gamma + epsilon

#Arguments are 
# edndogenous = character variable with name of dependent variable
# instruments = character vector with name of instruments
# vcov = variance matrix to be used in computing first stage, e.g. vcovCL for cluster robust
# data = data.frame with data
# controls = vector with controls to be included in refression. c() if no controls.
# intercept = shoudl an intercept be included in formulas? Variable named intercept will be included among
#             controls
# weights = vector of weights, if weighted regression is desired
# cluster = if vcov = vcovCL, the name of the cluster variable (defaults to NULL)
# ... = additional arguments, to be passed to vcov function, e.g. degree of freedom correction
olea_pflueger_f <- function(endogenous, instruments, vcov, data, controls = c(), 
                            intercept = TRUE, weights = NULL, cluster = NULL, ...)
{
  # Early chechk for weights and cluster
  if (!is.null(weights))
    weights <- as.data.frame(data)[, weights]
  if (!is.null(cluster))
    cluster <- as.data.frame(data)[, cluster]
  
  if (length(controls) > 0)
    data.kept = data[,c(endogenous,instruments, controls)] 
  else 
    data.kept = data[,c(endogenous,instruments)]
  
  keep_ind = complete.cases(data.kept)
  
  data.kept = data.kept[keep_ind,]
  
  Nobs = nrow(data.kept)
  
  if (intercept)
  {
    data.kept = cbind(data.kept, "intercept" = 1)
    controls = c(controls, "intercept")
  }
  
  if (length(controls) > 0)
  {
    # y = as.vector(residuals(lm(as.formula(paste(endogenous, "~ -1 + ", paste(controls,collapse = "+"),sep="")), data.kept)))
    
    Z = c()
    
    for (instrument in instruments)
    {
      if(is.null(weights))
        z =  as.vector(residuals(lm(as.formula(paste(instrument, "~ -1 + ", paste(controls,collapse = "+"),sep="")), data.kept))) 
      else
        z =  as.vector(residuals(lm(as.formula(paste(instrument, "~ -1 + ", paste(controls,collapse = "+"),sep="")), data.kept, weights = weights[keep_ind])) )
      
      Z = cbind(Z, z)
    }
    
  } else {
    # y = as.vector(data.kept[,endogenous])
    
    Z = as.matrix(data[,instruments])
    
  }
  
  formula.fs = paste(endogenous,"~ -1 +",paste(c(instruments,controls),collapse = " + "))
  
  if(is.null(weights))
    fs.reg = lm(as.formula(formula.fs), data.kept) 
  else 
    fs.reg = lm(as.formula(formula.fs), data.kept, 
                weights = weights[keep_ind])
  
  # if(is.null(weights))
  #   fs.reg = lm(y~Z-1) else fs.reg = lm(y~Z-1, weights = weights[keep_ind])
  
  coefs = fs.reg$coefficients[names(fs.reg$coefficients)%in%instruments]
  
  if(!is.null(cluster))
    vcov_mat = vcov(fs.reg, cluster = cluster[keep_ind], ...) 
  else 
    vcov_mat = vcov(fs.reg, ...)
  
  #Restricting to only instruments
  vcov_mat = vcov_mat[names(fs.reg$coefficients)%in%instruments,names(fs.reg$coefficients)%in%instruments]
  
  if (is.null(weights))
    Q_Z_norm = (t(Z) %*% Z)/Nobs 
  else {
    # Q_Z_norm = t(Z)%*%diag(weights[keep_ind])%*%Z/Nobs
    Z_w <- sapply(1:ncol(Z), function(j) weights[keep_ind]*Z[, j])
    Q_Z_norm <- (t(Z_w) %*% Z_w)/Nobs
  }
  F_eff = t(coefs)%*%Q_Z_norm%*%coefs/sum(diag(vcov_mat%*%Q_Z_norm))
  
  
  return(list("Nobs" = Nobs, "eff_F" = F_eff))
}


weak_iv_test <- olea_pflueger_f("family_number_children", 
                                "second_birth_ismultiplebirth",
                                vcovCL, data = lg_two_plus_births, 
                                controls = c("first_child_age",
                                             "first_child_is_girl",
                                             "first_birth_ismultiplebirth"),
                                weights = "person_weight",
                                cluster = "id_household")
#' d) Anderson-Rubin confidence interval
#' 
#AR CI

#Arguments are 
# outcome = character variable with name of outcome variable of interest
# edndogenous = character variable with name of endogenous variable
# instruments = character vector with name of instruments
# vcov = variance matrix to be used in pooled model.
# data = data.frame with data
# grid_beta = grid over which to perform grid search. Beta is the parameter of
#             interest in the IV regression
# confidence = confidence level for CI (defaults to 0.95)
# controls = vector with controls to be included in refression. c() if no controls.
# intercept = shoudl an intercept be included in formulas? Variable named intercept will be included among
#             controls
# weights = column name in data giving the desired observations weight
# cluster = if vcov = vcovCL, the name of the cluster variable (defaults to NULL)
# ... = additional arguments, to be passed to vcov function, e.g. degree of freedom correction

anderson_rubin_ci <- function(outcome, endogenous, instruments, vcov, data, 
                              grid_beta, confidence = 0.95, controls = c(), 
                              intercept = TRUE, weights = NULL, 
                              cluster = NULL, ...)
{
  # Early chechk for weights and cluster
  if (!is.null(weights))
    weights <- as.data.frame(data)[, weights]
  if (!is.null(cluster))
    cluster <- as.data.frame(data)[, cluster]
  
  if (length(controls) > 0)
    data.kept = data[,c(outcome, endogenous,instruments, controls)] 
  else 
    data.kept = data[,c(outcome, endogenous,instruments)]
  
  keep_ind = complete.cases(data.kept)
  
  data.kept = data.kept[keep_ind,]
  
  Nobs = nrow(data.kept)
  
  if (intercept){
    data.kept = cbind(data.kept, "intercept" = 1)
    controls = c(controls, "intercept")
  }
  
  #We will pool outcome and endogenous now
  data.pooled = rbind(data.kept, data.kept)
  
  data.pooled$pool_variable = c(data.kept[,outcome], data.kept[,endogenous])
  data.pooled$variable_indicator = as.factor(c(rep("reduced_form",    Nobs),rep("first_stage",Nobs)))
  
  
  
  #Constructing the formula for regression
  if(length(controls)>0)
    formula = paste("pool_variable ~ -1 + ", paste(paste("variable_indicator", instruments, sep = ":"),collapse = "+"), "+", paste(paste("variable_indicator", controls, sep = ":"),collapse = "+")) 
  else  
    formula = paste("pool_variable ~ -1 +", paste(paste("variable_indicator", instruments, sep = ":"),collapse = "+")) 
  
  
  if(is.null(weights))
    pool.model = lm(formula, data.pooled) 
  else  {
    pool_weights <- rep(weights[keep_ind], 2)
    pool.model = lm(as.formula(formula), data = data.pooled, 
                    weights = pool_weights) 
  }
  coefs = pool.model$coefficients
  
  if(!is.null(cluster))
    vcov_model = vcov(pool.model, cluster = rep(cluster[keep_ind],2), ...) 
  else 
    vcov_model = vcov(pool.model, ...)
  
  p1 = grepl(paste("reduced_form", instruments, sep = ":"), names(coefs))
  p2 = grepl(paste("first_stage", instruments, sep = ":"), names(coefs))
  
  acc_vec = c() 
  
  #Looping over grid
  for(beta in grid_beta)
  {
    
    lin_vec = p1 - beta*p2
    #constructing test statistic
    val = (coefs%*%lin_vec)
    vcov_lin = t(lin_vec)%*%vcov_model%*%lin_vec
    
    ar = val%*%solve(vcov_lin)%*%val
    
    pvalue = pchisq(ar, 1)
    
    if(pvalue<= confidence)
      acc_vec = c(acc_vec, T) else acc_vec = c(acc_vec, F)
    
  }
  
  if(sum(acc_vec) == 0)
    return("Confidence set is empty!") else {
      
      vec_region_start = c()
      vec_region_end = c()
      if(acc_vec[1] == TRUE)
      {
        warning("Lower boundary point was accepted! Perhaps decrease grid lower bound to see what happens?")
        vec_region_start = grid_beta[1]
      }
      
      if(acc_vec[length(acc_vec)] == TRUE)
      {
        warning("Upper boundary point was accepted! Perhaps increase grid upper bound to see what happens?")
        vec_region_end = grid_beta[length(acc_vec)]
      }
      
      vec_region_start = c(vec_region_start, grid_beta[c(FALSE,diff(acc_vec)==1)]  )
      vec_region_end = c(grid_beta[c(diff(acc_vec) ==-1, FALSE)],vec_region_end)
      
      # CI.text = paste(paste("[",vec_region_start, ",", vec_region_end, "]"),collapse = " U ")
      
      return(c(vec_region_start, vec_region_end))
    }
}
#' beta is estimated to be around -0.4, thus a grid surrounding this value.
grid_beta <- seq(-10, 10, 0.1)
ar_ci <- anderson_rubin_ci("first_child_years_of_education", 
                           "family_number_children", 
                           "second_birth_ismultiplebirth", 
                           vcovCL, lg_two_plus_births, grid_beta,
                           controls = c("first_child_age",
                                        "first_child_is_girl",
                                        "first_birth_ismultiplebirth"),
                           weights = "person_weight",
                           cluster = "id_household"
                           )
ar_ci_txt <- paste0("[", format(ar_ci[1], digits = 4), ", ",
                    format(ar_ci[2], digits = 4), "]")
iv_ci <- coefci(iv_est1)['family_number_children',]
iv_ci_txt <- paste0("[", format(iv_ci[1], digits = 4), ", ",
                    format(iv_ci[2], digits = 4), "]")

#' PART II RDD
#' 
#' Load data and drop unities with NA for treat or running
panes <- data.table::fread("II/input/amarante2016.csv", encoding = "Latin-1",
                           na.strings = "")[!is.na(treat) & !is.na(running)]
# lg_panes <- data.table::fread("II/input/amarante2016.csv", encoding = "Latin-1")

#' 2) Plots of treat versus running and bajo2500 versus running
rdplot(panes$treat, panes$running, binselect = 'es', ci = 95)

#' 3) Local linear regression
#' Using the rdd package
# rdd_model <- RDestimate(bajo2500~running+treat, data = panes, model = TRUE)
#' Using rdrobust package
tri_model <- rdrobust(panes$bajo2500, panes$running, 
                      fuzzy = panes$treat, kernel = "tri", all = TRUE)
uni_model <- rdrobust(panes$bajo2500, panes$running, 
                      fuzzy = panes$treat, kernel = "uni", all = TRUE)
#' First stage using the bandwidth from fuzzy model
tri_bw <- tri_model$bws["h", ]
tri_first <- rdrobust(panes$treat, panes$running, kernel = "tri", all = TRUE)

# Extract Data Values
table1 <- data.frame(uni_model$coef, uni_model$se, uni_model$pv)
table2 <- data.frame(tri_model$coef, tri_model$se, tri_model$pv)
rdd_met <- rep(rownames(table1), 2)
est_tab <- cbind(rdd_met, rbind(table1, table2))
rownames(est_tab) <- NULL
colnames(est_tab) <- c("Method", "Estimate", "Std.Error", "P-value")

#' 4) Placebo tests
#' Gestational length in weeks: semgest
#' Week first prenatal visit: semprim
#' Number of previous pregnancies: numemban
placebo_m1 <- rdrobust(panes$semgest, panes$running, 
                           fuzzy = panes$treat, kernel = "tri", all = TRUE)
# placebo_m2 <- rdrobust(panes$semprim, panes$running, 
#                            fuzzy = panes$treat, kernel = "tri", all = TRUE)
# placebo_m3 <- rdrobust(panes$numemban, panes$running, 
#                            fuzzy = panes$treat, kernel = "tri", all = TRUE)
#' 5) Manipulation test with rddensity
manipulation <- rddensity(panes$running)
manip_plot <- rdplotdensity(manipulation, panes$running)


#' Do not save large dataframes that start with "lg_"
save(list = ls()[!grepl("^lg_.*", ls())], file = "II/input/homework_I.RData")
