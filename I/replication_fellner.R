#' Load data from Fellner-etall
#' 
data_final <- haven::read_dta("I/input/data_final.dta")

# They use only robust se. No clusters
# package ri2 to perform randomization inference
