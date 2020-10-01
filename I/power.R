# Power calculation example.

#Loading package that calculates cluster robust SEs
# library(sandwich)
library(microbenchmark)
library(dplyr)
library(paramtest)
# Load data
data <- read.csv("I/input/escolas.csv")
#Sets seed to allow for replication
set.seed(1234)

#We'll consider a fine grid of treatment effects
grid = seq(0, 0.5, 0.05)

luis_power <- function(data, grid, nrep) {

#We'll consider three specifications.
#Spec 1 Regression of Outcome on treatment indicator
#Sepc 2 Includes sex as control
#Spec 3 Includes both sex and age as controls
spec1.mat = c()
spec2.mat = c()
spec3.mat = c()


#Creating list of schools to sample from
list.schools = unique(data$escola)

for (eff in grid)
{
  rej.col.1 = c()
  rej.col.2 = c()
  rej.col.3 = c()
  
  
  for (replications in 1:nrep)
  {
    #Drawing a sample, with replacement, of schools from our data. The idea here is that our sample well approximates the population distribution.
    #and that the sampling scheme is close to random sampling from the population of interest. 
    sample.draw = sample(list.schools, replace = T)
    
    #Construct the artificial data from a draw. Note that repeated schools should be treated as distinct so as to mimic random sampling.
    list.new.data = lapply(1:length(sample.draw), function(x){
      extract = data[data$escola == sample.draw[x],]
      #extract$escola = x
      return(extract)
    })
    
    #Concatenating data on lists
    # data.artificial ends up with lower number of observations that original 
    # data
    data.artificial = do.call(rbind, list.new.data)
    
    #Next, we select that half of the schools will be treated
    treat.draw = sample(unique(data.artificial$escola),size = length(unique(data.artificial$escola))/2, replace = F)
    
    data.artificial$treat = 1*(data.artificial$escola %in% treat.draw)
    
    #Create outcome
    data.artificial$y = data.artificial$logico + data.artificial$treat*eff
    
    #Running models and storing whether we reject the null that effect is 0
    model1 = lm(y~treat, data = data.artificial)
    
    se = sqrt(diag(vcovCL(model1, cluster = data.artificial$escola)))
    
    rej.col.1 = c(rej.col.1, 1*(abs(model1$coefficients["treat"]/se["treat"]) > qnorm(0.975) ))
    
    
    model2 = lm(y~treat+mulher, data = data.artificial)
    
    se = sqrt(diag(vcovCL(model2, cluster = data.artificial$escola)))
    
    rej.col.2 = c(rej.col.2, 1*(abs(model2$coefficients["treat"]/se["treat"]) > qnorm(0.975) ))
    
    
    model3 = lm(y~treat+mulher+idade, data = data.artificial)
    
    se = sqrt(diag(vcovCL(model3, cluster = data.artificial$escola)))
    
    rej.col.3 = c(rej.col.3, 1*(abs(model3$coefficients["treat"]/se["treat"]) > qnorm(0.975) ))
    
  }
  
  spec1.mat = cbind(spec1.mat, rej.col.1)
  spec2.mat = cbind(spec2.mat, rej.col.2)
  spec3.mat = cbind(spec3.mat, rej.col.3)
  
}


tabela = cbind("Effect" =  grid, "Spec 1" = colMeans(spec1.mat), "Spec 2" = colMeans(spec2.mat), "Spec 3" = colMeans(spec3.mat))
rownames(tabela) = rep("",nrow(tabela))

return(tabela)
}

bressan_power <- function(data, eff, nrep) {
  
  #We'll consider three specifications.
  #Spec 1 Regression of Outcome on treatment indicator
  #Sepc 2 Includes sex as control
  #Spec 3 Includes both sex and age as controls
  spec1.mat = matrix(NA_real_, nrow = nrep, ncol = length(grid))
  spec2.mat = matrix(NA_real_, nrow = nrep, ncol = length(grid))
  spec3.mat = matrix(NA_real_, nrow = nrep, ncol = length(grid))
  
  
  #Creating list of schools to sample from
  list.schools = unique(data$escola)
  
  for (i in seq_along(eff))
  {
    rej.col.1 = vector(mode = "numeric", length = nrep)
    rej.col.2 = vector(mode = "numeric", length = nrep)
    rej.col.3 = vector(mode = "numeric", length = nrep)
    
    
    for (r in 1:nrep)
    {
      #Drawing a sample, with replacement, of schools from our data. The idea here is that our sample well approximates the population distribution.
      #and that the sampling scheme is close to random sampling from the population of interest. 
      sample.draw = sample(list.schools, replace = T)
      
      #Construct the artificial data from a draw. Note that repeated schools should be treated as distinct so as to mimic random sampling.
      list.new.data = lapply(1:length(sample.draw), function(x){
        extract = data[data$escola == sample.draw[x],]
        #extract$escola = x
        return(extract)
      })
      
      #Concatenating data on lists
      # data.artificial ends up with lower number of observations that original 
      # data
      data.artificial = do.call(rbind, list.new.data)
      
      #Next, we select that half of the schools will be treated
      treat.draw = sample(unique(data.artificial$escola),size = length(unique(data.artificial$escola))/2, replace = F)
      
      data.artificial$treat = 1*(data.artificial$escola %in% treat.draw)
      
      #Create outcome
      data.artificial$y = data.artificial$logico + data.artificial$treat*eff[i]
      
      #Running models and storing whether we reject the null that effect is 0
      model1 = lm(y~treat, data = data.artificial)
      
      se = sqrt(diag(vcovCL(model1, cluster = data.artificial$escola)))
      
      rej.col.1[r] = 1*(abs(model1$coefficients["treat"]/se["treat"]) > qnorm(0.975) )
      
      
      model2 = lm(y~treat+mulher, data = data.artificial)
      
      se = sqrt(diag(vcovCL(model2, cluster = data.artificial$escola)))
      
      rej.col.2[r] = 1*(abs(model2$coefficients["treat"]/se["treat"]) > qnorm(0.975) )
      
      
      model3 = lm(y~treat+mulher+idade, data = data.artificial)
      
      se = sqrt(diag(vcovCL(model3, cluster = data.artificial$escola)))
      
      rej.col.3[r] = 1*(abs(model3$coefficients["treat"]/se["treat"]) > qnorm(0.975) )
      
    }
    
    spec1.mat[, i] = rej.col.1
    spec2.mat[, i] = rej.col.2
    spec3.mat[, i] = rej.col.3
    
  }
  
  
  tabela = cbind("Effect" =  grid, "Spec 1" = colMeans(spec1.mat), "Spec 2" = colMeans(spec2.mat), "Spec 3" = colMeans(spec3.mat))
  rownames(tabela) = rep("",nrow(tabela))
  
  return(tabela)
}

pt_func <- function(simNum, eff) {
  #Drawing a sample, with replacement, of schools from our data. The idea here is that our sample well approximates the population distribution.
  #and that the sampling scheme is close to random sampling from the population of interest. 
  sample.draw = sample(list.schools, replace = T)
  
  #Construct the artificial data from a draw. Note that repeated schools should be treated as distinct so as to mimic random sampling.
  list.new.data = lapply(1:length(sample.draw), function(x){
    extract = data[data$escola == sample.draw[x],]
    #extract$escola = x
    return(extract)
  })
  
  #Concatenating data on lists
  # data.artificial ends up with lower number of observations that original 
  # data
  data.artificial = do.call(rbind, list.new.data)
  
  #Next, we select that half of the schools will be treated
  treat.draw = sample(unique(data.artificial$escola),size = length(unique(data.artificial$escola))/2, replace = F)
  data.artificial$treat = 1*(data.artificial$escola %in% treat.draw)
  
  #Create outcome
  data.artificial$y = data.artificial$logico + data.artificial$treat*eff
  
  #Running models and storing whether we reject the null that effect is 0
  model1 = lm(y~treat, data = data.artificial)
  se = sqrt(diag(sandwich::vcovCL(model1, cluster = data.artificial$escola)))
  rej.col.1 = 1*(abs(model1$coefficients["treat"]/se["treat"]) > qnorm(0.975) )
  
  model2 = lm(y~treat+mulher, data = data.artificial)
  se = sqrt(diag(sandwich::vcovCL(model2, cluster = data.artificial$escola)))
  rej.col.2 = 1*(abs(model2$coefficients["treat"]/se["treat"]) > qnorm(0.975) )
  
  model3 = lm(y~treat+mulher+idade, data = data.artificial)
  se = sqrt(diag(sandwich::vcovCL(model3, cluster = data.artificial$escola)))
  rej.col.3 = 1*(abs(model3$coefficients["treat"]/se["treat"]) > qnorm(0.975) )
  
  return(c(M1 = rej.col.1, M2 = rej.col.2, M3 = rej.col.3))
}

#' Passing data frame along
pt_func2 <- function(simNum, df, eff) {
  list.schools <- unique(df$escola)
  #Drawing a sample, with replacement, of schools from our data. The idea here is that our sample well approximates the population distribution.
  #and that the sampling scheme is close to random sampling from the population of interest. 
  sample.draw = sample(list.schools, replace = T)
  
  #Construct the artificial data from a draw. Note that repeated schools should be treated as distinct so as to mimic random sampling.
  list.new.data = lapply(1:length(sample.draw), function(x){
    extract = df[df$escola == sample.draw[x],]
    #extract$escola = x
    return(extract)
  })
  
  #Concatenating data on lists
  # data.artificial ends up with lower number of observations that original 
  # data
  data.artificial = do.call(rbind, list.new.data)
  
  #Next, we select that half of the schools will be treated
  treat.draw = sample(unique(data.artificial$escola),size = length(unique(data.artificial$escola))/2, replace = F)
  data.artificial$treat = 1*(data.artificial$escola %in% treat.draw)
  
  #Create outcome
  data.artificial$y = data.artificial$logico + data.artificial$treat*eff
  
  #Running models and storing whether we reject the null that effect is 0
  model1 = lm(y~treat, data = data.artificial)
  se = sqrt(diag(sandwich::vcovCL(model1, cluster = data.artificial$escola)))
  rej.col.1 = 1*(abs(model1$coefficients["treat"]/se["treat"]) > qnorm(0.975) )
  
  model2 = lm(y~treat+mulher, data = data.artificial)
  se = sqrt(diag(sandwich::vcovCL(model2, cluster = data.artificial$escola)))
  rej.col.2 = 1*(abs(model2$coefficients["treat"]/se["treat"]) > qnorm(0.975) )
  
  model3 = lm(y~treat+mulher+idade, data = data.artificial)
  se = sqrt(diag(sandwich::vcovCL(model3, cluster = data.artificial$escola)))
  rej.col.3 = 1*(abs(model3$coefficients["treat"]/se["treat"]) > qnorm(0.975) )
  
  return(c(M1 = rej.col.1, M2 = rej.col.2, M3 = rej.col.3))
}


#' WARNING: Benchmarking may take a long time to run depending on your number of
#' simulations and number of replication in the benchmark (parameter "times")
#' 
#' Benchmarking loops without memory pre-allocation (luis_power) and with pre-allocation (bressan_power)
#' Little gain from pre-allocation
#' 
# bench <- microbenchmark(
#   luis = luis_power(data, grid, 1000),
#   bressan = bressan_power(data, grid, 1000),
#   times = 2
# )
# bench
# 
# ggplot2::autoplot(bench)

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# luis 35.95879 38.16900 40.37969 41.06376 42.87047 43.67753    10
# bressan 35.36512 36.44161 38.56642 37.39070 41.39571 44.42894    10

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# luis 433.3701 433.3701 434.7878 434.7878 436.2056 436.2056     2
# bressan 415.5078 415.5078 420.2004 420.2004 424.8929 424.8929     2


#' Testing paramtest
#' First let as global variables: data and list.schools
#' 
list.schools <- unique(data$escola)
pt_power <- run_test(pt_func, n.iter = 1000, output = "data.frame",
                                eff = 0.5)
#' Test power results
colMeans(results(pt_power))[2:4]

#' Now test passing data to pt_func2
pt_power2 <- run_test(pt_func2, n.iter = 1000, output = "data.frame",
                                 df = data, eff = 0.5)
#' Test power results
colMeans(results(pt_power2))[2:4]
#' Benchmarking. Looks like there is no harm in passing the data.frame to the
#' function. This is preferable since the code is more concise. 
# pt_bench <- microbenchmark(
#   pt1 = run_test(pt_func, n.iter = 1000, output = "data.frame",
#                             eff = 0.5),
#   pt2 = run_test(pt_func2, n.iter = 1000, output = "data.frame",
#                             df = data, eff = 0.5),
#   times = 2
# )
# pt_bench

#' Now let's vary the eff argument, to capture the Minimum Detectable Effect - MDE
#' 
pt_pwr2 <- grid_search(pt_func2, n.iter = 1000, output = "data.frame",
                                  df = data, params = list(eff = grid))
pwr2_df <- results(pt_pwr2) %>% 
  group_by(eff.test) %>% 
  summarise(across(contains("treat"), mean))
pwr2_df

#' Is it worth to use parallelization? Let's check it.
#' 
pt_par <- grid_search(pt_func2, n.iter = 1000, output = "data.frame",
                                 df = data, params = list(eff = grid),
                                 parallel = "multicore", ncpus = 4)
par_df <- results(pt_par) %>% 
  group_by(eff.test) %>% 
  summarise(across(contains("treat"), mean))
par_df

#' Benchmark it! Parallelized simulation is approximately twice as fast.
# par_bench <- microbenchmark(
#   no_par = grid_search(pt_func2, n.iter = 1000, output = "data.frame",
#                        df = data, params = list(eff = grid)),
#   par = grid_search(pt_func2, n.iter = 1000, output = "data.frame",
#                     df = data, params = list(eff = grid),
#                     parallel = "multicore", ncpus = 4),
#   times = 2
# )
# par_bench

# Unit: seconds
# expr       min        lq      mean    median        uq       max neval
# no_par 171.47772 171.47772 171.68298 171.68298 171.88824 171.88824     2
# par  87.04231  87.04231  92.20065  92.20065  97.35899  97.35899     2

