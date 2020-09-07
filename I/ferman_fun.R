#' Function to make the assessment. Input arguments are the data frame, 
#' unconstrained formula to regress, variable name to make the assessment, 
#' coefficient under H0, number of simulations to run, significance level, an optional 
#' argument to set whether the residuals from model fit should be sampled with 
#' replacement to carry out all simulations. If there are clusters in the 
#' model they may be passed on through "cluster".
#' 
# ferman_ass function ---------------------------------------
# ferman_ass <- function(data, H0form, Uncform, assess_on, nsim = 1000, alpha = 0.05, res.samp = FALSE) {
#   # Simulations sequence
#   sim <- seq_len(nsim)
#   
#   # Step 1: estimate H0 model
#   H0fit <- lm(H0form, data = data)
#   # Step 2: store the predicted values
#   y_pred <- predict(H0fit)
#   
#   # Step 3: nsim iterations
#   # Step 3.1: draw simulation errors and generate y_sim
#   if (res.samp) {
#     ob_res <- residuals(H0fit)
#     sim_errors <- lapply(sim, 
#                          function(x) {
#                            sample(ob_res, size = length(ob_res), replace = TRUE)
#                          } 
#     ) # lapply
#   } else {# Draw errors fron standard normal
#     sim_errors <- lapply(sim, function(x) rnorm(length(residuals(H0fit)))) 
#   }
#   
#   y_sim <- lapply(sim_errors, `+`, y_pred) # Sum y_pred to errors for each sim
#  
#   # Step 3.2: Estimate the unrestricted model for each sim
#   sim_regs <- lapply(y_sim,
#                      function(x){
#                        aug_df <- cbind(y_sim = x, data)
#                        lm(Uncform, data = aug_df)
#                      })
#   # Step 3.3: Test the null hypothesis for each sim
#   # Extract all p-values from desired coefficient
#   pvals <- lapply(sim_regs, 
#                   function(x){
#                     summary(x)$coefficients[assess_on, 4]
#                   })
#   # Test whether pvals < alpha and return the proportion
#   prop <- mean(ifelse(pvals < alpha, 1, 0))
#   return(prop)
# }

# lm_ferman_ass function ---------------------------------------
lm_ferman_ass <- function(data, H0form, Uncform, assess_on, nsim = 1000, alpha = 0.05, res.samp = FALSE, cluster = NULL) {
  # Simulations sequence
  sim <- seq_len(nsim)
  # Rejections vector (of 0s and 1s)
  rejections <- c()
  
  # Step 1: estimate H0 model
  H0fit <- lm(as.formula(H0form), data = data)
  # Step 2: store the predicted values
  y_pred <- predict(H0fit)
  res <- residuals(H0fit)
  
  # Step 3: nsim iterations
  # Step 3.1: draw simulation errors and generate y_sim
  for (i in sim) {
    if (res.samp)
      sim_error <- sample(res, size = length(res), replace = TRUE)
    else
      sim_error <- rnorm(length(res))
    
    y_sim <- y_pred + sim_error
    aug_df <- cbind(y_sim = y_sim, data)
    
    # Step 3.2: Estimate the unrestricted model for each sim
    sim_reg <- lm(as.formula(Uncform), data = aug_df)
    
    # Step 3.3: Test the null hypothesis for each sim
    # Extract all p-values from desired coefficient
    coefi <- lmtest::coeftest(sim_reg, vcov = sandwich::vcovCL, 
                              cluster = as.formula(cluster))
    pval <- coefi[assess_on, 4]
    # Test whether pvals < alpha and store in rejections
    rejections[i] <- ifelse(pval < alpha, 1, 0)
  }
  
  # Return the mean of rejections
  return(mean(rejections))
}

# tidy_ferman_ass function ---------------------------------------
# tidy_ferman_ass <- function(data, H0form, Uncform, assess_on, nsim = 1000, alpha = 0.05, res.samp = FALSE, cluster = NULL) {
#   # Step 1: estimate H0 model
#   H0fit <- lm(as.formula(H0form), data = data)
#   # Step 2: store the predicted values
#   y_pred <- predict(H0fit)
#   res <- residuals(H0fit)
#   # Simulations sequence
#   df <- tibble::tibble(sim = seq_len(nsim)) %>% 
#     dplyr::rowwise()
#   
# 
#   # Step 3: nsim iterations
#   # Step 3.1: draw simulation errors and generate y_sim
#   if (res.samp) {
#       df <- df %>% 
#         dplyr::mutate(sim_errors = list(sample(res, replace = TRUE)))
#   } else {# Draw errors from standard normal
#     df <- df %>% 
#       dplyr::mutate(sim_errors = list(rnorm(length(res))))
#     }
#   
#   df <- df %>% 
#     dplyr::mutate(y_sim = list(sim_errors + y_pred))
#   
#   
#   # Step 3.2: Estimate the unrestricted model for each sim
#   df_regs <- df %>% 
#     dplyr::mutate(sim_reg = list(lm(as.formula(Uncform), 
#                                     data = cbind(y_sim = y_sim, data))),
#                   # Step 3.3: Test the null hypothesis for each sim
#                   # Extract all p-values from desired coefficient
#                   coefi = list(lmtest::coeftest(sim_reg, vcov = sandwich::vcovCL, 
#                                                 cluster = as.formula(cluster))),
#                   pvals = coefi[assess_on, 4],
#                   reject = ifelse(pvals < alpha, 1, 0)
#     )
# 
#   # Return the mean of rejections
#   prop <- mean(df_regs$reject)
#   return(prop)
# }

# speed_ferman_ass function ---------------------------------------
# speed_ferman_ass <- function(data, H0form, Uncform, assess_on, nsim = 1000, alpha = 0.05, res.samp = FALSE) {
#   # Simulations sequence
#   sim <- seq_len(nsim)
#   # Rejections vector (of 0s and 1s)
#   rejections <- c()
#   
#   # Step 1: estimate H0 model
#   H0fit <- speedglm::speedlm(H0form, data = data, y = TRUE, fitted = TRUE)
#   # Step 2: store the predicted values
#   y_pred <- predict(H0fit)
#   res <- H0fit$y - y_pred
#   
#   # Step 3: nsim iterations
#   # Step 3.1: draw simulation errors and generate y_sim
#   for (i in sim) {
#     if (res.samp)
#       sim_error <- sample(res, size = length(res), replace = TRUE)
#     else
#       sim_error <- rnorm(length(res))
#     
#     y_sim <- y_pred + sim_error
#     aug_df <- cbind(y_sim = y_sim, data)
#     
#     # Step 3.2: Estimate the unrestricted model for each sim
#     sim_reg <- speedglm::speedlm(Uncform, data = aug_df)
#     
#     # Step 3.3: Test the null hypothesis for each sim
#     # Extract all p-values from desired coefficient
#     pval <- summary(sim_reg)$coefficients[assess_on, 4]
#     # Test whether pvals < alpha and store in rejections
#     rejections[i] <- ifelse(pval < alpha, 1, 0)
#   }
#   
#   # Return the mean of rejections
#   return(mean(rejections))
# }

# fix_ferman_ass function ---------------------------------------
fix_ferman_ass <- function(data, H0form, Uncform, assess_on, nsim = 1000, alpha = 0.05, res.samp = FALSE, cluster = NULL) {
  # Thus to replicate the results from reghdfe you need to provide the argument 
  # dof = dof(cluster.df = "min", t.df = "min"). From fixest "On standard-errors"
  # https://cran.r-project.org/web/packages/fixest/vignettes/standard_errors.html
  fixest::setFixest_dof(dof(cluster.df = "min", t.df = "min"))
  fixest::setFixest_se(no_FE = "white")
  # Simulations sequence
  sim <- seq_len(nsim)
  # Rejections vector (of 0s and 1s)
  rejections <- c()
  
  # Step 1: estimate H0 model
  H0fit <- fixest::feols(as.formula(H0form), data = data)
  # Step 2: store the predicted values
  y_pred <- predict(H0fit)
  res <- residuals(H0fit)
  
  # Step 3: nsim iterations
  # Step 3.1: draw simulation errors and generate y_sim
  for (i in sim) {
    if (res.samp)
      sim_error <- sample(res, size = length(res), replace = TRUE)
    else
      sim_error <- rnorm(length(res))
    
    y_sim <- y_pred + sim_error
    aug_df <- cbind(y_sim = y_sim, data)
    
    # Step 3.2: Estimate the unrestricted model for each sim
    sim_reg <- fixest::feols(as.formula(Uncform), data = aug_df)
    
    # Step 3.3: Test the null hypothesis for each sim
    # Extract all p-values from desired coefficient
    pval <- summary(sim_reg, cluster = cluster)$coeftable[assess_on, 4]
    # Test whether pvals < alpha and store in rejections
    rejections[i] <- ifelse(pval < alpha, 1, 0)
  }
  
  # Return the mean of rejections
  return(mean(rejections))
}

# fe_ferman_ass function ---------------------------------------
fe_ferman_ass <- function(data, H0form, Uncform, assess_on, nsim = 1000, alpha = 0.05, res.samp = FALSE) {
  # Simulations sequence
  sim <- seq_len(nsim)
  # Rejections vector (of 0s and 1s)
  rejections <- c()
  
  # Step 1: estimate H0 model
  H0fit <- lfe::felm(as.formula(H0form), data = data, cmethod = "reghdfe")
  # Step 2: store the predicted values
  y_pred <- H0fit$fitted.values
  res <- H0fit$residuals
  
  # Step 3: nsim iterations
  # Step 3.1: draw simulation errors and generate y_sim
  for (i in sim) {
    if (res.samp)
      sim_error <- sample(res, size = length(res), replace = TRUE)
    else
      sim_error <- rnorm(length(res))
    
    y_sim <- y_pred + sim_error
    aug_df <- cbind(y_sim = y_sim, data)
    
    # Step 3.2: Estimate the unrestricted model for each sim
    sim_reg <- lfe::felm(as.formula(Uncform), data = aug_df, cmethod = "reghdfe")
    
    # Step 3.3: Test the null hypothesis for each sim
    # Extract all p-values from desired coefficient
    pval <- summary(sim_reg)$coefficients[assess_on, 4]
    # Test whether pvals < alpha and store in rejections
    rejections[i] <- ifelse(pval < alpha, 1, 0)
  }
  
  # Return the mean of rejections
  return(mean(rejections))
}



#Function that computes MDE for bilateral test -----------------------------
#alpha: significance level
#se: estimate of coefficient standard error
#kappa: rejection probability
#step.grid: grid step for for computing power
compute.mde.bilateral <- function(alpha, se, kappa, step.grid = 1e-3)
{
  #Grid for evaluating formula
  grid = seq(0, qnorm(1 - alpha/2) + 5*se, by = step.grid*se)
  
  power = sapply(grid, function(b){
    1 - pnorm(qnorm(1 - alpha/2) - b/se ) + pnorm(qnorm(alpha/2) - b/se)
    })
  
  mde = grid[which.min(abs(power - kappa))]
  
  print(paste("MDE at ",100*kappa, "% of a ", 
              alpha*100,"% test is absolute value of effect >= ", mde, sep = ""))
  return(mde)
}

#Function that computes wild BS. Takes as arguments: ---------------------------
#formula: amodel to test
#coef.to.test: character. name of the variable in formula whose coefficient we will test
#cluster.var: character. name of the cluster indicator variable
#data: dataframe where estimation will be conducted
#b: value of coefficient under the null. Defaults to 0
#S: Number of replications of step 2 in algorithm. Defaults to 1000
#dataset with variables
wild.bs <- function(formula, coef.to.test, cluster.var, data, b = 0, S = 1000)
{
  formula.text = as.character(formula)
  
  #Imposing the null in formula
  formula.null = paste(
    formula.text[2], "~", gsub(coef.to.test, paste("offset(b*",coef.to.test,")", sep = ""),
                               formula.text[3]))
  
  modelo.nulo = lm(as.formula(formula.null), data = data)
  
  cluster.data = cbind("Cluster" = data[,cluster.var])
  
  cluster.indexes = unique(cluster.data[,1])
  
  C = length(cluster.indexes)
  
  vec_unstud = c()
  vec_stud = c()
  
  data.artificial  = data
  
  for (s in 1:S)
  {
    e_s = 1 - 2*rbinom(C, 1, 0.5)
    
    vals.cluster = cbind("Cluster" = cluster.indexes, "e_s" = e_s)
    cluster.matched = merge(cluster.data, vals.cluster, by = "Cluster")
    
    #Creating artificial data
    
    data.artificial[,formula.text[2]] = modelo.nulo$fitted.values + 
      cluster.matched$e_s*modelo.nulo$residuals
    
    modelo.s = lm(formula, data = data.artificial) 
    
    coef.s = modelo.s$coefficients[coef.to.test]
    
    vec_unstud = c(vec_unstud, coef.s)
    
    se.s = sqrt(diag(vcovCL(modelo.s, cluster = cluster.data[,1])))[coef.to.test]
    
    vec_stud = c(vec_stud,  (coef.s - b)/se.s)
  }
  
  #Compute estimates from the data now
  modelo.data = lm(formula, data = data)
  
  coef.data = modelo.data$coefficients[coef.to.test]
  
  p.val.unstud = 1 - mean(abs(coef.data) > abs(vec_unstud))
  se.data =  sqrt(diag(vcovCL(modelo.data, cluster = cluster.data[,1])))[coef.to.test]
  
  p.val.stud = 1 - mean(abs((coef.data - b)/se.data) > abs(vec_stud))
  
  return(list("Unstudentized p-value" = p.val.unstud, 
              "Studentized p-value" = p.val.stud))
}


#' Performs the inference assessment provided in Ferman (2019).
#'
#' @param df Your database
#' @param model A character string in R's formula style defining the unconstrained model.
#' @param assess_on The variable the assessment will be taken on.
#' @param H0 Coefficient value under the null hypothesis, by default 0.0
#' @param nsim Number of simulations to run, by default 1000
#' @param alpha Significance level, by default 0.05
#' @param res.samp  Logical describing if residuals from null regression should 
#' be sampled with replacement to construct simulated residuals, by default False
#' @param cluster Cluster variable name. Not yet implemented, by default None
#'
#' @return Assessment value for the given level of significance.
#' @export
#'
#' @examples
#' # NOT RUN
#' ferman_assessment(iris, "Sepal.Lenght~Sepal.Width+Petal.Width", "Petal.Width")
ferman_assessment <- function(df, model, assess_on, H0 = 0.0, nsim = 1000, alpha = 0.05, res.samp = FALSE, cluster = NULL, weights = NULL) {
  # Coercing df to data.frame ONLY (no tibble or data.table)
  df <- as.data.frame(df)
  # Thus to replicate the results from reghdfe you need to provide the argument 
  # dof = dof(cluster.df = "min", t.df = "min"). From fixest "On standard-errors"
  # https://cran.r-project.org/web/packages/fixest/vignettes/standard_errors.html
  fixest::setFixest_dof(fixest::dof(cluster.df = "min", t.df = "min"))
  fixest::setFixest_se(no_FE = "white")
  # No spaces allowed in model formula
  model <- gsub("\\s+", "", model)
  depvar <- sub("~.+", "", model)
  # Simulations sequence
  sim <- seq_len(nsim)
  # Rejections vector (of 0s and 1s)
  rejections <- c()
  
  # Step 1: estimate H0 model
  pattern <- paste0(assess_on, "\\+")
  null_text <- gsub(pattern, "", model)
  off <- H0*df[, assess_on]
  weight <- df[, weights]
  H0fit <- fixest::feols(as.formula(null_text), data = df, offset = off, 
                         warn = FALSE, weights = weight)
  # H0fit <- lm(as.formula(null_text), data = df)
  # Step 2: store the predicted values
  y_pred <- predict(H0fit)
  res <- residuals(H0fit)
  
  # Step 3: nsim iterations
  # Step 3.1: draw simulation errors and generate y_sim
  for (i in sim) {
    if (res.samp)
      sim_error <- sample(res, replace = TRUE)
    else
      sim_error <- rnorm(length(res))
    
    y_sim <- y_pred + sim_error
    df[, depvar] <- y_sim
    
    # Step 3.2: Estimate the unrestricted model for each sim
    sim_reg <- fixest::feols(as.formula(model), data = df, weights = weight,
                             warn = FALSE)
    
    # Step 3.3: Test the null hypothesis for each sim
    # Extract all p-values from desired coefficient
    if (is.null(cluster)) {
      beta <- summary(sim_reg)$coeftable[assess_on, 1]
      se_beta <- summary(sim_reg)$coeftable[assess_on, 2]
      tstat <- abs((beta - H0)/se_beta)
    }
    else {
      beta <- summary(sim_reg, cluster = cluster)$coeftable[assess_on, 1]
      se_beta <- summary(sim_reg, cluster = cluster)$coeftable[assess_on, 2]
      tstat <- abs((beta - H0)/se_beta)
    }
    # Test whether pvals < alpha and store in rejections
    rejections[i] <- ifelse(tstat > qnorm(1 - alpha/2), 1, 0)
  }
  
  # Return the mean of rejections
  return(mean(rejections))
}