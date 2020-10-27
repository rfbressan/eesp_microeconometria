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


# Propensity Score --------------------------------------------------------

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

# Weak Instruments - IV ---------------------------------------------------

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
      if (is.null(weights))
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
  
  
  return(list("Nobs" = Nobs, "Effective F" = F_eff))
}

#AR test

#Arguments are 
# outcome = character variable with name of outcome variable of interest
# edndogenous = character variable with name of endogenous variable
# instruments = character vector with name of instruments
# vcov = variance matrix to be used in pooled model.
# data = data.frame with data
# beta_0 = value under the null (defaults to 0)
# controls = vector with controls to be included in refression. c() if no controls.
# intercept = shoudl an intercept be included in formulas? Variable named intercept will be included among
#             controls
# weights = vector of weights, if weighted regression is desired
# cluster = if vcov = vcovCL, the name of the cluster variable (defaults to NULL)
# ... = additional arguments, to be passed to vcov function, e.g. degree of freedom correction

anderson_rubin_test <- function(outcome, endogenous, instruments, vcov, data, beta_0=0, controls = c(), intercept = T, weights = NULL, cluster = NULL, ...)
{
  # Early chechk for weights and cluster
  if (!is.null(weights))
    weights <- as.data.frame(data)[, weights]
  if (!is.null(cluster))
    cluster <- as.data.frame(data)[, cluster]
  
  if(length(controls)>0)
    data.kept = data[,c(outcome, endogenous,instruments, controls)] else data.kept = data[,c(outcome, endogenous,instruments)]
    
    keep_ind = complete.cases(data.kept)
    
    data.kept = data.kept[keep_ind,]
    
    Nobs = nrow(data.kept)
    
    if(intercept)
    {
      data.kept = cbind(data.kept, "intercept" = 1)
      controls = c(controls, "intercept")
    }
    
    #We will pool outcome and endogenous now
    data.pooled = rbind(data.kept, data.kept)
    
    data.pooled$pool_variable = c(data.kept[,outcome], data.kept[,endogenous])
    data.pooled$variable_indicator = as.factor(c(rep("reduced_form",    Nobs),rep("first_stage",Nobs)))
    
    
    
    #Constructing the formula for regression
    if(length(controls)>0)
      formula = paste("pool_variable ~ -1 + ", paste(paste("variable_indicator", instruments, sep = ":"),collapse = "+"), "+", paste(paste("variable_indicator", controls, sep = ":"))) else  formula = paste("pool_variable ~ -1 +", paste(paste("variable_indicator", instruments, sep = ":"),collapse = "+")) 
    
    
    if(is.null(weights))
      pool.model = lm(formula, data.pooled) else  pool.model = lm(formula, data.pooled, weights = weights[rep(keep_ind,2)]) 
    
    coefs = pool.model$coefficients
    
    if(!is.null(cluster))
      vcov_model = vcov(pool.model,cluster = rep(data[keep_ind, cluster],2), ...) else vcov_model = vcov(pool.model, ...)
    
    
    lin_vec = 1*grepl(paste("reduced_form", instruments, sep = ":"), names(coefs)) - 
      beta_0*grepl(paste("first_stage", instruments, sep = ":"), names(coefs))
    
    #constructing test statistic
    val = (coefs%*%lin_vec)
    vcov_lin = t(lin_vec)%*%vcov_model%*%lin_vec
    
    ar = val%*%solve(vcov_lin)%*%val
    
    pvalue =1 - pchisq(ar, 1)
    
    return(list("AR test statistic" = ar, "P-value" = pvalue, "Nobs" = Nobs))
}

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

#' Manski bounds with MTR and MTS
#' Computes the ATE's upper bound based on the MTS assumption.
#' $t\geq s \implies E[y(w)|z=t]\geq E[y(w)|z=s]$.
#' @param data Data Frame
#' @param outcome Name of outcome variable
#' @param treatment Name of treatment variable
#' @param treat_levels Vector with levels of treatment where ATE's UB will be 
#' computed.
#' 
#' All variable's names must be **unquoted**.
#' 
#' @return A data frame with treatment levels and their upper bound on ATE.
ate_ub <- function(data, outcome, treatment, treat_levels) {
  treat_levels <- sort(treat_levels)
  treat_vec <- data %>% 
    distinct({{treatment}}) %>% 
    arrange({{treatment}}) %>% 
    pull()
  if (!all(treat_levels %in% treat_vec))
    stop("All treatment levels must be in the data.")
  # Conditional expectations and probabilities
  exp_probs <- data %>% 
    select(c({{outcome}}, {{treatment}})) %>% 
    group_by({{treatment}}) %>% 
    summarise(average = mean({{outcome}}), 
              probability = n()/nrow(.),
              size = n()) %>% 
    mutate(avg_prob = average * probability) %>% 
    arrange({{treatment}})
  
  ub <- vector("numeric", length = length(treat_levels))
  n_treats <- nrow(exp_probs)
  
  for (i in seq_along(treat_levels)) {
    t <- treat_levels[i]
    t_idx <- which(treat_vec == t)
    s <- treat_vec[t_idx - 1]
    sum_t_plus <- exp_probs %>% 
      filter({{treatment}} > t) %>% 
      summarise(sum(avg_prob)) %>% 
      pull()
    sum_s_less <- exp_probs %>% 
      filter({{treatment}} < s) %>% 
      summarise(sum(avg_prob)) %>% 
      pull()
    exp_y_t <- exp_probs %>% 
      filter({{treatment}} == t) %>% 
      pull(average)
    exp_y_s <- exp_probs %>% 
      filter({{treatment}} == s) %>% 
      pull(average)
    prob_t_leq <- exp_probs %>% 
      filter({{treatment}} <= t) %>% 
      summarise(sum(probability)) %>% 
      pull()
    prob_s_geq <- exp_probs %>% 
      filter({{treatment}} >= s) %>% 
      summarise(sum(probability)) %>% 
      pull()
    upper_bound <- sum_t_plus + exp_y_t * prob_t_leq - (sum_s_less + exp_y_s * prob_s_geq)
    
    ub[i] <- upper_bound
  }
  
  return(data.frame(treat_levels = treat_levels, upper_bound = ub))
}