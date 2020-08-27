# Power calculation example.

#Loading package that calculates cluster robust SEs
library(sandwich)
library(microbenchmark)

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

bench <- microbenchmark(
  luis = luis_power(data, grid, 1000),
  bressan = bressan_power(data, grid, 1000),
  times = 2
)
bench

ggplot2::autoplot(bench)

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# luis 35.95879 38.16900 40.37969 41.06376 42.87047 43.67753    10
# bressan 35.36512 36.44161 38.56642 37.39070 41.39571 44.42894    10

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# luis 433.3701 433.3701 434.7878 434.7878 436.2056 436.2056     2
# bressan 415.5078 415.5078 420.2004 420.2004 424.8929 424.8929     2

