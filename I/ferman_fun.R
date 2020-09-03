#' Function to make the assessment. Input arguments are the data frame, H0
#' formula, unconstrained model formula to regress, variable name to make the 
#' assessment, number of simulations to run, significance level, an optional 
#' argument to set whether the residuals from model fit should be sampled with 
#' replacement to carry out all simulations. If clusters are presented in the 
#' model they may be passed along through "cluster", and the way you pass it is
#' dependent on the function. See the section Microbenchmark to understand it.
#' 
#' Uncform MUST have the dependent variable named as y_sim. Thus the string
#' passed onto it must be something like "y_sim ~ X1+X2+as.factor(fe1)" for lm and
#' fix functions or "y_sim ~ X1+X2|fe1|0|C" for fe.
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

