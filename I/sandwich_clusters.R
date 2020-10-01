# Sandwich test

data("InstInnovation", package = "sandwich")

sand_test <- function(data, form, cluster = NULL) {
  h_innov <- pscl::hurdle(form, data = data, dist = "negbin")
  
  vc <- list("standard" = vcov(h_innov),
             "basic" = sandwich::sandwich(h_innov),
             "CL-1" = sandwich::vcovCL(h_innov, cluster = as.formula(cluster))
  )
  
  se <- function(vcovar) sapply(vcovar, function(x) sqrt(diag(x)))
  return(se(vc))
}

sand_test(InstInnovation, 
          cites ~ institutions + log(capital/employment) + log(sales),
          "~company")
