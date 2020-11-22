#' ## homework_II_census.R
#' Homework II - Microeconometrics II
#' Author: Rafael Felipe Bressan
#' 
#' Paper: Regional Effects of Trade Reform: What Is the Correct Measure of 
#' Liberalization? by Brian Kovak - AER2013
#' Loading libraries
library(tidyverse)
library(dtplyr)
library(data.table)
library(haven)

#' Part II
#' Load data
# load("input/homework_II.RData")
folder <- "II/input/Kovak2013/AER-2011-0545_data/"
# census_sample -----------------------------------------------------------
cnaedom_to_indmatch <- read_dta(paste0(folder, "cnaedom_to_indmatch.dta")) %>% 
  as.data.table()
pnad_to_indmatch <- read_dta(paste0(folder, "pnad_to_indmatch.dta")) %>% 
  as.data.table()
microreg_to_mmc <- read_dta(paste0(folder, "microreg_to_mmc.dta")) %>% 
  as.data.table()
ibge_wagebill <- read_dta(paste0(folder, "ibge_wagebill.dta")) %>% 
  as.data.table()
ibge_value_added <- read_dta(paste0(folder, "ibge_value_added.dta")) 
niv50_to_indmatch <- read_dta(paste0(folder, "niv50_to_indmatch.dta")) 

#' Reading census
# ibge_census_2000 <- read_dta(paste0(folder, "ibge_census_2000.dta")) %>% 
#   as.data.table()
# gc()
# ibge_census_2000 <- 
#   ibge_census_2000[age %in% 18:55 & ymain > 0 & !is.na(ymain) & inschool == 0]
# gc()
#' 5517219 row after filtering
# print(object.size(ibge_census_2000), units = "GB")
ibge_census_1991 <- read_dta(paste0(folder, "ibge_census_1991.dta")) %>% 
  as.data.table()
gc()
ibge_census_1991 <- 
  ibge_census_1991[age %in% 18:55 & ymain > 0 & !is.na(ymain) & inschool == 0]
gc()
#' 4739171 rows after filtering
print(object.size(ibge_census_1991), units = "GB")
#' calculate log real wage and generate remaining wage regression variables
#' real wages 2000 base year
ibge_census_1991[, `:=`(wagemain = (ymain/4.33)/hmain,
                        agesq = age^2 / 1000,
                        city = (urbanrural == 1))]
ibge_census_1991[, rwagemain := (wagemain/2750000)/(0.000067244146018/0.890629059684618)]
ibge_census_1991[, lnrwmain := log(rwagemain)]
gc()
#' recode industries to IndMatch
#' 1991 column atividade matches to pnad
ibge_census_1991 <- merge(ibge_census_1991, pnad_to_indmatch, by = "atividade",
                          all.x = TRUE)
#' 2000 column cnae matches to cnaedom
# ibge_census_2000 <- merge(ibge_census_2000, cnaedom_to_indmatch, 
#                           by.x = "cnae", by.y = "cnaedom", all.x = TRUE)
# setnames(ibge_census_2000, "cnae", "atividade")
gc()

#' Bind the two censuses 
# ibge_census <- rbind(ibge_census_1991, ibge_census_2000)
# rm(ibge_census_1991, ibge_census_2000)
# gc()

#' merge in consistent microregion codes
ibge_census_1991 <- merge(ibge_census_1991, microreg_to_mmc, by = "microreg",
                          all.x = TRUE)

#' Census sample
census_sample <- ibge_census_1991
rm(ibge_census_1991)

# figure_1_b1_b2_b5 -------------------------------------------------------
tariff_chg <- read_dta(paste0(folder, "kume_etal_tariff.dta")) %>% 
  left_join(niv50_to_indmatch, by = "niv50") %>% 
  filter(indmatch != 99) %>% 
  left_join(ibge_value_added, by = "niv50") %>% 
  group_by(indmatch, indname) %>% 
  summarise(across(tariff1987:tariff1998, ~weighted.mean(.x, w = va))) %>% 
  mutate(dlnonetariff9095 = log(1 + (tariff1995/100)) - log(1 + (tariff1990/100))) %>% 
  select(indmatch, indname, dlnonetariff9095)

# figure_2_b3 -------------------------------------------------------------


# figure_3_4_b4 -----------------------------------------------------------
#' Using lambda and theta from Kovak's Stata
# lambda_kovak <- haven::read_dta("II/input/lambda.dta")
# theta_kovak <- haven::read_dta("II/input/theta.dta")
# 
# lambda <- lambda_kovak %>% 
#   pivot_longer(cols = lambda1:lambda99, names_to = "indmatch", 
#                values_to = "lambda") %>% 
#   mutate(indmatch = as.numeric(str_extract(indmatch, "\\d+")))
# theta <- theta_kovak
#' With lambda and theta taken directly from Kovak the results are THE SAME!!
#' More evidence my computations of share weights are correct

#' calculate fixed factor share of input cost (theta)
theta <- ibge_wagebill %>% 
  as_tibble() %>% 
  left_join(niv50_to_indmatch, by = "niv50") %>% 
  group_by(indmatch, indname) %>% 
  summarise(across(wagebill:factorcost, sum)) %>% 
  mutate(theta = 1 - (wagebill/factorcost))

#' calculate industry weights for each region
lambda <- census_sample %>% 
  lazy_dt() %>% 
  filter(!is.na(indmatch)) %>% 
  select(mmc, xweighti, indmatch) %>% 
  group_by(mmc, indmatch) %>% 
  summarise(lambda = sum(xweighti)) %>% 
  mutate(lambda = lambda / sum(lambda)) %>% 
  as_tibble() 
  # pivot_wider(names_from = indmatch, values_from = lambda, names_sort = TRUE)

#' weights omitting nontraded sector, with and without theta adjustment
weights_ss_notheta <- lambda %>% 
  left_join(theta, by = "indmatch") %>% 
  filter(indmatch != 99) %>%  # omit nontraded sector and colinear 16
  mutate(lambdaovertheta = lambda / theta) %>% 
  group_by(mmc) %>% 
  mutate(sumlambdaovertheta = sum(lambdaovertheta),
         sumlambda = sum(lambda),
         weight_main = lambdaovertheta / sumlambdaovertheta,
         weight_notheta = lambda / sumlambda)
#' weights including nontraded sector and theta adjustment
weights_ss_nt <- lambda %>% 
  left_join(theta, by = "indmatch") %>% 
  mutate(lambdaovertheta = lambda / theta) %>% 
  group_by(mmc) %>% 
  mutate(sumlambdaovertheta = sum(lambdaovertheta),
         weight_nt = lambdaovertheta / sumlambdaovertheta)

#' generate RTC. weighted averages of tariff changes 
rtc_notheta <- weights_ss_notheta %>% 
  select(mmc, indmatch, weight_main, weight_notheta) %>% 
  left_join(tariff_chg, by = "indmatch") %>% 
  group_by(mmc) %>% 
  summarise(rtc_main = sum(weight_main * dlnonetariff9095),
            rtc_notheta = sum(weight_notheta * dlnonetariff9095))

rtc_nt <- weights_ss_nt %>% 
  select(mmc, indmatch, weight_nt) %>% 
  left_join(tariff_chg, by = "indmatch") %>% 
  mutate(dlnonetariff9095 = if_else(indmatch == 99, 0, dlnonetariff9095)) %>% 
  group_by(mmc) %>% 
  summarise(rtc_nt = sum(weight_nt * dlnonetariff9095))

rtc_bressan <- rtc_notheta %>% 
  left_join(rtc_nt, by = "mmc")
#' Load Kovak's rtc and compare
rtc_kovak <- read_dta("II/input/rtc.dta")
cat("Maximum difference in rtc_main from Kovak's\n",
    max(abs(rtc_bressan$rtc_main - rtc_kovak$rtc_main)))

#' Preparing weight matrices to Adao Shift Share confidence intervals
weight_main <- weights_ss_notheta %>% 
  filter(mmc != 13007) %>%  # drop Manaus
  select(mmc, indmatch, weight_main) %>% 
  pivot_wider(names_from = indmatch, values_from = weight_main, 
              names_sort = TRUE, values_fill = 0.0) %>% 
  as.matrix()

weight_notheta <- weights_ss_notheta %>% 
  filter(mmc != 13007) %>%  # drop Manaus
  select(mmc, indmatch, weight_notheta) %>% 
  pivot_wider(names_from = indmatch, values_from = weight_notheta, 
              names_sort = TRUE, values_fill = 0.0) %>% 
  as.matrix()

weight_nt <- weights_ss_nt %>% 
  filter(mmc != 13007) %>%  # drop Manaus
  select(mmc, indmatch, weight_nt) %>% 
  pivot_wider(names_from = indmatch, values_from = weight_nt, 
              names_sort = TRUE, values_fill = 0.0) %>% 
  as.matrix()

#' Do not save large dataframes that start with "lg_"
save(weight_main, weight_notheta, weight_nt, rtc_bressan, 
     file = "II/input/homework_II_Adao.RData")
#' Clean up environment
rm(list = ls())
gc()
