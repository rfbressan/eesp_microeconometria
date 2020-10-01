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


# Propensity score blocking -----------------------------------------------
#Function that subdivides a given propensity score vector in subblocks
#treat = vector with treatment assignments
#lin.psm = vector with linearized PSs
#K = how many covariates will we want to test/use in bias correction of estimates later on? 
#t.max = threshold for tstat in making a further subdivide 
#trim = should we discard extreme observations so there is overlap?
ps_blocks <- function(treat,lin.psm, K, t.max = 1.96,  trim =T)
{
  if(trim){
    b0 = min(plogis(lin.psm[treat==1]))
    b1 = max(plogis(lin.psm[treat==0]))
  } else
  {
    b0 = 0
    b1 = 1
  }
  b_vec =c(b0,b1)
  while(TRUE)
  {
    J = length(b_vec)-1
    b_vec_new = do.call(c,lapply(1:J, function(j){
      sample = (b_vec[j] <= plogis(lin.psm)) & (plogis(lin.psm) < b_vec[j+1])
      
      ps.treat = lin.psm[sample&treat==1]
      ps.control = lin.psm[sample&treat==0]
      
      #print(length(ps.control))
      #print(length(ps.treat))
      
      t.test.pass = tryCatch({abs(t.test(ps.control, ps.treat)$statistic) > t.max}, 
                             error = function(e){return(FALSE)})
      
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
  
  for(j in 1:(length(b_vec)-1))
    block_var[(b_vec[j] <= plogis(lin.psm)) & (plogis(lin.psm) < b_vec[j+1])] = j

  return(block_var)
}

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

# balance table: t-test and normalized difference -------------------------
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

# 7 Trim the sample -------------------------------------------------------
#' Author: Luis Alvarez
trimming.imbens <- function(lin.ps, step.gamma.grid = 1e-3)
{
  inv.vec = 1/(plogis(lin.ps)*(1 - plogis(lin.ps)))
  
  if (max(inv.vec) <= 2*mean(inv.vec))
  {
    print("No trimming")
    return(rep(TRUE, length(lin.ps))) 
  }else {
    gamma.grid <- seq(min(inv.vec), max(inv.vec), by = step.gamma.grid)
    
    values = sapply(gamma.grid, function(gamma){
      (2*sum(inv.vec[inv.vec <= gamma])  - gamma*sum(inv.vec <= gamma))
    })
    
    values[values < 0] = Inf
    
    gamma = gamma.grid[which.min(values)]
    
    alpha.trim = 1/2 - sqrt(1/4 - 1/gamma)
    print(paste("Trimming threshold alpha is ",alpha.trim))
    return(plogis(lin.ps) <= 1 - alpha.trim &  plogis(lin.ps) >= alpha.trim)
  }
}

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