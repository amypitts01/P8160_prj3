# Things I need for report 
library(ggplot2)
library(tidyverse)
library(data.table)
library(maps)
library(gtsummary)

dt = read.csv("hurrican703.csv")
dt1 <- dt

load("df_rm.RData")

load("mcmc_chain_1.RData")
load("mcmc_chain_2.RData")
load("mcmc_chain_3.RData")
load("mcmc_chain_4.RData")
load("mcmc_chain_5.RData")
load("mcmc_chain_6.RData")

# saving all data
theta_chain = rbind(theta_chain_1, theta_chain_2, theta_chain_3, 
                    theta_chain_4, theta_chain_5, theta_chain_6)



L = 1.5
d = 5  ## dimension of beta_i
m = 681  ## number of hurricanes
n_beta = m*d  ## total number of beta
n_theta = n_beta+d+1+15  ## total number of parameters
mu_ind = (n_beta+1):(n_beta+d) ## index of mu
sigma_ind = n_beta+d+1 ## index of sigma
cov_m_ind = (n_beta+d+2):n_theta ## index of elements of inverse covariance matrix

## take average over 5501-10000 as estimates
bs_estimates = unname(apply(theta_chain[5501:10000,], 2, mean))


inv_cov_m = matrix(0,5,5)
inv_cov_m[lower.tri(inv_cov_m, diag = TRUE)] = bs_estimates[cov_m_ind]
inv_cov_m[upper.tri(inv_cov_m)] = t(inv_cov_m)[upper.tri(inv_cov_m)]


cov_m = solve(inv_cov_m)

## estimates
sigma = bs_estimates[3411]
mu = bs_estimates[3406:3410]
beta = matrix(bs_estimates[1:n_beta], ncol = d)


load("df_rm.RData")
## create variables of interest
dt = df_rm %>% 
  janitor::clean_names() %>% 
  group_by(id) %>% 
  mutate(wind_lag = lag(wind_kt,1), ##previous wind speed
         lat_shift = lag(latitude, 1) - lag(latitude, 2), ## change of lat
         long_shift = lag(longitude, 1) - lag(longitude, 2), ## change of long
         wind_shift = lag(wind_kt, 1) - lag(wind_kt, 2)) %>%  ##change of wind speed
  drop_na()
n_obs = nrow(dt) ## number of

dt_res = dt %>% 
  dplyr::select(id, nature, season, month, wind_kt,wind_lag,lat_shift, long_shift, wind_shift) %>%
  nest(data = nature:wind_shift) %>% 
  ungroup() %>% 
  mutate(beta_0 = beta[,1],
         beta_1 = beta[,2],
         beta_2 = beta[,3],
         beta_3 = beta[,4],
         beta_4 = beta[,5]) %>% 
  unnest(data) %>% 
  mutate(wind_kt_pred = beta_0+beta_1*wind_lag+beta_2*lat_shift+beta_3*long_shift+beta_4*wind_shift) %>%
  group_by(id) %>% 
  mutate(r_square = (1-sum((wind_kt_pred-wind_kt)^2)/(sum((wind_kt-mean(wind_kt))^2))),
         n_obs = n(),
         adj_r_square = 1-(1-r_square)*(n_obs-1)/(n_obs-5),
         p_value = 1-pchisq(sum((wind_kt_pred-wind_kt)^2)/sigma, n_obs))
  
