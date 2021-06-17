#' ## homework_II.R
#' Homework II - Microeconometrics II
#' Author: Rafael Felipe Bressan
#' 
#' Paper: Regional Effects of Trade Reform: What Is the Correct Measure of 
#' Liberalization? by Brian Kovak - AER2013
#' Loading libraries
library(tidyverse)
library(fixest)
library(sandwich)
library(dtplyr)
library(data.table)
library(ShiftShareSE)

#' Process 1991 census and create matrices of share weights to use with
#' Adao et. al. inference
# source("II/homework_II_census.R")

#' Part II
#' Load data
folder <- "II/input/Kovak2013/AER-2011-0545_data/"

dlnwmmc_mincer <- haven::read_dta("II/input/dlnwmmc_mincer.dta")
dlnwmmc_mincer_nt <- haven::read_dta("II/input/dlnwmmc_mincer_nt.dta")
rtc <- haven::read_dta("II/input/rtc.dta")
microreg_to_mmc <- haven::read_dta(paste0(folder, "microreg_to_mmc.dta")) %>% 
  as.data.table()
#' Load DATASUS data on unemployment in 1991
unemp <- fread("II/input/desemprego1991.csv", sep = ";", encoding = "Latin-1",
               dec = ",")
unemp[, `:=`(microreg = as.numeric(str_extract(unemp$`Microrregião IBGE`, "\\d{5}")))]
unemp <- microreg_to_mmc[unemp, on = "microreg"]
unemp <- unemp[, .(avg_unemp91 = mean(`Taxa_de_desemprego_16a_e+`, na.rm = TRUE)), 
               by = "mmc"]
#' Processed data for SS assessment and Adao et. al inference
load("II/input/homework_II_Adao.RData")

W_main <- weight_main[, -1]
W_notheta <- weight_notheta[, -1]
W_nt <- weight_nt[, -1]

#' Join data, create state fixed effect and drop Manaus
df <- dlnwmmc_mincer %>% 
  left_join(dlnwmmc_mincer_nt, by = "mmc") %>% 
  left_join(rtc, by = "mmc") %>% 
  left_join(as_tibble(unemp), by = "mmc") %>% 
  mutate(state = factor(floor(mmc/1000))) %>% 
  filter(mmc != 13007)
# Embed the weights on the data frame
df <- df %>% 
  mutate(weights = dlnwmmc_mincerse^-2,
         weights_nt = dlnwmmc_mincerse_nt^-2)
#' Setup fixest
setFixest_se(no_FE = "hetero")
setFixest_dof(dof(fixef.K = "full")) # get Stata's reg results

# Replication of Table 1 --------------------------------------------------


#' Main specifications - columns (1) and (2)
reg_main <- feols(dlnwmmc_mincer~rtc_main, 
                  weights = ~weights,
                  data = df)
reg_main_fe <- feols(dlnwmmc_mincer~rtc_main|state, 
                     weights = ~weights,
                     data = df)
#' No labor share adjustment - columns (3) and (4)
reg_nolab <- feols(dlnwmmc_mincer~rtc_notheta, 
                   weights = ~weights,
                   data = df)
reg_nolab_fe <- feols(dlnwmmc_mincer~rtc_notheta|state, 
                      weights = ~weights,
                      data = df)
#' Nontraded price change set to zero - columns (5) and (6)
reg_notrad <- feols(dlnwmmc_mincer~rtc_nt, 
                    weights = ~weights,
                    data = df)
reg_notrad_fe <- feols(dlnwmmc_mincer~rtc_nt|state, 
                       weights = ~weights,
                       data = df)

#' Nontraded sector workers' wages - columns (7) and (8)
reg_workers <- feols(dlnwmmc_mincer_nt~rtc_main, 
                     weights = ~weights_nt,
                     data = df)
reg_workers_fe <- feols(dlnwmmc_mincer_nt~rtc_main|state, 
                        weights = ~weights_nt,
                        data = df)

# Ferman assessment -------------------------------------------------------
#' Performs the inference assessment provided in Ferman (2019).
#'
#' @param df Your database
#' @param model A character string in R's formula style defining the fixest model.
#' @param assess_on The variable the assessment will be taken on.
#' @param W share matrix
#' @param H0 Coefficient value under the null hypothesis, by default 0.0
#' @param nsim Number of simulations to run, by default 1000
#' @param alpha Significance level, by default 0.05
#' @param weights Weights variable name
#' @param cluster Cluster variable name. Package fixest must be loaded in order
#' to use this argument. It must agree with the cluster provided in the model
#'
#' @return Assessment value for the given level of significance.
#' @export
#'
#' @examples
#' # NOT RUN
#' ferman_assessment(iris, "Sepal.Lenght~Sepal.Width+Petal.Width", "Petal.Width")
# Shift-share Ferman assessment -------------------------------------------

ss_ferman_assessment <- function(data, model, assess_on, W, H0 = 0.0, nsim = 1000, 
                                 alpha = 0.05, weights = NULL, cluster = NULL) {
  # Coercing df to data.frame ONLY (no tibble or data.table)
  df <- as.data.frame(df)
  # No spaces allowed in model formula
  model <- gsub("\\s+", "", model)
  depvar <- sub("~.+", "", model)
  # Simulations sequence
  sim <- seq_len(nsim)
  # Rejections vector (of 0s and 1s)
  rejections <- c()
  # Regression weights
  weight <- df[, weights]
  # Holder for artificial data
  df_artificial <- df
  # Iterate nsim simulations
  for (i in sim) {
    # Placebo SS regressor
    df_artificial[, assess_on] <- W %*% rnorm(ncol(W))
    # Estimate model with placebo SS
    placebo_fit <- fixest::feols(as.formula(model), data = df_artificial, 
                                 warn = FALSE, weights = weight)
    # Reject at specified significance?
    if (is.null(cluster)) {
      beta <- summary(placebo_fit)$coeftable[assess_on, 1]
      se_beta <- summary(placebo_fit)$coeftable[assess_on, 2]
      tstat <- abs((beta - H0)/se_beta)
    }
    else {
      # fixest must be loaded in order to clusters work!
      beta <- summary(placebo_fit, cluster = cluster)$coeftable[assess_on, 1]
      se_beta <- summary(placebo_fit, cluster = cluster)$coeftable[assess_on, 2]
      tstat <- abs((beta - H0)/se_beta)
    }
    # Test whether pvals < alpha and store in rejections
    rejections[i] <- ifelse(tstat > qnorm(1 - alpha/2), 1, 0)
  }
  # Return the mean of rejections
  return(mean(rejections))
}


#' Assessment on main model without fixed effects
base_df <- tibble(name = c("Main", "Main FE", "No labor", "No labor FE",
                           "Nontraded", "Nontraded FE",
                           "Workers", "Workers FE"),
                  assess_on = c(rep("rtc_main", 2), rep("rtc_notheta", 2),
                                rep("rtc_nt", 2), rep("rtc_main", 2)),
                  weights = c(rep("weights", 6), rep("weights_nt", 2)),
                  cluster = "state")
assessment_df <- base_df %>% 
  mutate(model = c("dlnwmmc_mincer~rtc_main",
                   "dlnwmmc_mincer~rtc_main|state",
                   "dlnwmmc_mincer~rtc_notheta",
                   "dlnwmmc_mincer~rtc_notheta|state",
                   "dlnwmmc_mincer~rtc_nt",
                   "dlnwmmc_mincer~rtc_nt|state",
                   "dlnwmmc_mincer_nt~rtc_main",
                   "dlnwmmc_mincer_nt~rtc_main|state"),
         W_mat = list(W_main, W_main, W_notheta, W_notheta, W_nt, W_nt,
                      W_main, W_main),
         alpha = list(c(0.05, 0.10))) %>% 
  unnest(cols = alpha) %>% 
  rowwise() %>%
  mutate(assessment = ss_ferman_assessment(df, model, assess_on, W_mat,
                                           cluster = cluster, weights = weights,
                                           alpha = alpha))

assessment_tbl <- assessment_df %>% 
  select(name, alpha, assessment) %>% 
  pivot_wider(id_cols = name, names_from = alpha, values_from = assessment)

# Wild Bootstrap ----------------------------------------------------------
#Function that computes wild BS. Takes as arguments:
#formula: a model to test
#coef.to.test: character. name of the variable in formula whose coefficient we will test
#cluster.var: character. name of the cluster indicator variable
#data: dataframe where estimation will be conducted
#b: value of coefficient under the null. Defaults to 0
#S: Number of replications of step 2 in algorithm. Defaults to 1000
#dataset with variables
wild.bs <- function(data, formula, coef.to.test, cluster.var, weight.var = NULL,
                    b = 0, S = 1000)
{
  stopifnot(is_tibble(data))
  # No spaces allowed in model formula
  formula <- gsub("\\s+", "", formula)
  depvar <- sub("~.+", "", formula)
  # Weighted regression
  if (!is.null(weight.var)) 
    weight.vec <- data %>% pull(weight.var)
  else
    weight.vec <- NULL
  #Imposing the null in formula
  formula.null <- gsub(coef.to.test, 
                       glue::glue("offset(b*{coef.to.test})"),
                       formula)
  
  modelo.nulo <- lm(as.formula(formula.null), weights = weight.vec, data = data)
  
  cluster.data <- data[, cluster.var]
  
  cluster.indexes <- unique(cluster.data)
  
  C <- nrow(cluster.indexes)
  
  vec_unstud <- c()
  vec_stud <- c()
  
  data.artificial <- data
  
  for (s in 1:S)
  {
    e_s = 1 - 2*rbinom(C, 1, 0.5)
    
    vals.cluster <- cbind(cluster.indexes, "e_s" = e_s)
    cluster.matched <- merge(cluster.data, vals.cluster, by = cluster.var)
    
    #Creating artificial data
    data.artificial[, depvar] <- modelo.nulo$fitted.values + 
      cluster.matched$e_s*modelo.nulo$residuals
    
    modelo.s <- lm(as.formula(formula), weights = weight.vec, 
                   data = data.artificial) 
    
    coef.s <- modelo.s$coefficients[coef.to.test]
    
    vec_unstud <- c(vec_unstud, coef.s - b)
    
    se.s <- sqrt(
      diag(sandwich::vcovCL(modelo.s, cluster = cluster.data[,1])))[coef.to.test]
    
    vec_stud <- c(vec_stud,  (coef.s - b)/se.s)
  }
  
  #Compute estimates from the data now
  modelo.data <- lm(as.formula(formula), weights = weight.vec, data = data)
  
  coef.data <- modelo.data$coefficients[coef.to.test]
  
  p.val.unstud <- 1 - mean(abs(coef.data - b) > abs(vec_unstud))
  se.data <- sqrt(
    diag(sandwich::vcovCL(modelo.data, cluster = cluster.data[,1])))[coef.to.test]
  
  p.val.stud <- 1 - mean(abs((coef.data - b)/se.data) > abs(vec_stud))
  
  return(data.frame("Unstudentized p-value" = p.val.unstud, 
                    "Studentized p-value" = p.val.stud))
}

wild_df <- base_df %>% 
  mutate(model = c("dlnwmmc_mincer~rtc_main",
                   "dlnwmmc_mincer~rtc_main+factor(state)",
                   "dlnwmmc_mincer~rtc_notheta",
                   "dlnwmmc_mincer~rtc_notheta+factor(state)",
                   "dlnwmmc_mincer~rtc_nt",
                   "dlnwmmc_mincer~rtc_nt+factor(state)",
                   "dlnwmmc_mincer_nt~rtc_main",
                   "dlnwmmc_mincer_nt~rtc_main+factor(state)")) %>% 
  rowwise() %>% 
  mutate(wild = list(wild.bs(df, model, assess_on, cluster, weights))) %>% 
  unnest(cols = wild)


# Permutation test - Randomization Inference ------------------------------
rand_inference <- function(df, model, assess_on, H0 = 0.0, nsim = 1000, 
                           cluster = NULL, weights = NULL) {
  stopifnot(is_tibble(df))
  # No spaces allowed in model formula
  model <- gsub("\\s+", "", model)
  depvar <- sub("~.+", "", model)
  # Weights vector
  if (!is.null(weights))
    weights_vec <- df %>% pull(weights)

  # Regression with original data
  orig_model <- fixest::feols(as.formula(model), weights = weights_vec, data = df)
  orig_coef <- orig_model$coefficients[assess_on]
  if (!is.null(cluster))
    orig_se <- summary(orig_model, cluster = cluster)$se[assess_on]
  else
    orig_se <- summary(orig_model)$se[assess_on]
  # Original regression test statistic. Studentized
  orig_test <- abs(orig_coef - H0) / orig_se
  
  # Permutations
  # Clusterized permutations: number of clusters
  if (!is.null(cluster)) {
    n_cl <- nrow(unique(df[cluster]))
  }
  # Artificial data
  df_art <- df
  vec_sim <- vector(mode = "double", length = nsim)
  for (s in seq_len(nsim)) {
    if (!is.null(cluster)) {
      # Sample clusters without replacement
      cl_shuffle <- sample(n_cl)
      # Reorder only cluster and treatment
      cl_sample <- unique(df[cluster])[cl_shuffle, ] %>% 
        right_join(df[c(cluster, assess_on)], by = cluster)
      # Replace cluster and treatment without reordering all else!
      df_art[c(cluster, assess_on)] <- cl_sample
      # Regression with artificial data
      art_model <- fixest::feols(as.formula(model), weights = weights_vec, 
                                 data = df_art)
      art_coef <- art_model$coefficients[assess_on]
      art_se <- summary(art_model, cluster = df_art[cluster])$se[assess_on]
    } # clusterized permutation
    else {
      shuffle <- sample(nrow(df))
      # Reorder treatment
      tr_sample <- df[shuffle, assess_on]
      df_art[assess_on] <- tr_sample
      # Regression with artificial data
      art_model <- fixest::feols(as.formula(model), weights = weights_vec, 
                                 data = df_art)
      art_coef <- art_model$coefficients[assess_on]
      art_se <- summary(art_model)$se[assess_on]
    }
    # Artificial regression test statistic. Studentized
    art_test <- abs(art_coef - H0) / art_se
    vec_sim[s] <- art_test
  } # end of for loop
  
  p_val <- 1 - mean(orig_test > vec_sim)
  return(c(exact_p_val = p_val))
  
}
ri_df <- base_df %>% 
  mutate(model = c("dlnwmmc_mincer~rtc_main",
                   "dlnwmmc_mincer~rtc_main|state",
                   "dlnwmmc_mincer~rtc_notheta",
                   "dlnwmmc_mincer~rtc_notheta|state",
                   "dlnwmmc_mincer~rtc_nt",
                   "dlnwmmc_mincer~rtc_nt|state",
                   "dlnwmmc_mincer_nt~rtc_main",
                   "dlnwmmc_mincer_nt~rtc_main|state")) %>% 
  rowwise() %>% 
  mutate(ri_p_val = rand_inference(df, model, assess_on, cluster = cluster,
                                   weights = weights))

# Adao confidence interval ----------------------------------------------------
adao1 <- reg_ss("dlnwmmc_mincer~1", X = rtc_main , W = W_main, 
                weights = weights,
                region_cvar = state,
                method = "all", data = df)[["p"]]
adao2 <- reg_ss("dlnwmmc_mincer~state", X = rtc_main , W = W_main, 
                weights = weights,
                region_cvar = state,
                method = "all", data = df)[["p"]]
adao3 <- reg_ss("dlnwmmc_mincer~1", X = rtc_notheta , W = W_notheta, 
                weights = weights,
                region_cvar = state,
                method = "all", data = df)[["p"]]
adao4 <- reg_ss("dlnwmmc_mincer~state", X = rtc_notheta , W = W_notheta, 
                weights = weights,
                region_cvar = state,
                method = "all", data = df)[["p"]]
adao5 <- reg_ss("dlnwmmc_mincer~1", X = rtc_nt , W = W_nt, 
                weights = weights,
                region_cvar = state,
                method = "all", data = df)[["p"]]
adao6 <- reg_ss("dlnwmmc_mincer~state", X = rtc_nt , W = W_nt, 
                weights = weights,
                region_cvar = state,
                method = "all", data = df)[["p"]]
adao7 <- reg_ss("dlnwmmc_mincer_nt~1", X = rtc_main , W = W_main, 
                weights = weights_nt,
                region_cvar = state,
                method = "all", data = df)[["p"]]
adao8 <- reg_ss("dlnwmmc_mincer_nt~state", X = rtc_main , W = W_main, 
                weights = weights_nt,
                region_cvar = state,
                method = "all", data = df)[["p"]]
adao_df <- base_df %>% 
  mutate(adao_pval = list(adao1, adao2, adao3, adao4, 
                          adao5, adao6, adao7, adao8)) %>% 
  rowwise() %>% 
  mutate(methods = list(names(adao_pval))) %>%  
  unnest(cols = c(adao_pval, methods)) %>% 
  select(name, adao_pval, methods) %>% 
  pivot_wider(names_from = methods, values_from = adao_pval) %>% 
  rename(Homo = Homoscedastic)

# Borusyak Hull Jaravel inference -----------------------------------------
source("II/BorusyakHullJaravel.R")

bhj_ivreg_ss("dlnwmmc_mincer~state|rtc_main", X = rtc_main , W = W_main, 
             weights = weights,
             region_cvar = state,
             method = "all", data = df)

# Robustness specs --------------------------------------------------------
unemp_main <- reg_ss("dlnwmmc_mincer~avg_unemp91", X = rtc_main , W = W_main, 
                     weights = weights,
                     region_cvar = state,
                     method = "all", data = df)
unemp_main_fe <- reg_ss("dlnwmmc_mincer~avg_unemp91+state", X = rtc_main , 
                        W = W_main, 
                        weights = weights,
                        region_cvar = state,
                        method = "all", data = df)
unemp_nolab <- reg_ss("dlnwmmc_mincer~avg_unemp91", X = rtc_notheta , W = W_notheta, 
                      weights = weights,
                      region_cvar = state,
                      method = "all", data = df)
unemp_nolab_fe <- reg_ss("dlnwmmc_mincer~avg_unemp91+state", X = rtc_notheta , W = W_notheta, 
                         weights = weights,
                         region_cvar = state,
                         method = "all", data = df)
unemp_notrad <- reg_ss("dlnwmmc_mincer~avg_unemp91", X = rtc_nt , W = W_nt, 
                       weights = weights,
                       region_cvar = state,
                       method = "all", data = df)
unemp_notrad_fe <- reg_ss("dlnwmmc_mincer~avg_unemp91+state", X = rtc_nt , W = W_nt, 
                          weights = weights,
                          region_cvar = state,
                          method = "all", data = df)
unemp_workers <- reg_ss("dlnwmmc_mincer_nt~avg_unemp91", X = rtc_main , W = W_main, 
                        weights = weights_nt,
                        region_cvar = state,
                        method = "all", data = df)
unemp_workers_fe <- reg_ss("dlnwmmc_mincer_nt~avg_unemp91+state", X = rtc_main , W = W_main, 
                           weights = weights_nt,
                           region_cvar = state,
                           method = "all", data = df)
robust_df <- tibble(description = c("Regional tariff change", names(unemp_main$se)),
                    main = c(unemp_main$beta, unemp_main$se),
                    main_fe = c(unemp_main_fe$beta, unemp_main_fe$se),
                    nolab = c(unemp_nolab$beta, unemp_nolab$se),
                    nolab_fe = c(unemp_nolab_fe$beta, unemp_nolab_fe$se),
                    notrad = c(unemp_notrad$beta, unemp_notrad$se),
                    notrad_fe = c(unemp_notrad_fe$beta, unemp_notrad_fe$se),
                    workers = c(unemp_workers$beta, unemp_workers$se),
                    workers_fe = c(unemp_workers_fe$beta, unemp_workers_fe$se))


inferences_df <- wild_df %>% 
  select(name, contains("p.value")) %>% 
  left_join(ri_df[c("name", "ri_p_val")], by = "name") %>% 
  left_join(adao_df, by = "name")
#' save image
save(list = ls(), file = "II/input/homework_II.RData")
