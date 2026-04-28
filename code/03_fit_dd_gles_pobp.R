#-------------------------------------------------------------------------------
#
# Opting-in but not donating: Fit partially observed bivariate probit model 
#
# Author: Sina Chen
# Date: 2026-04-28
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

{
  library(tidyverse)
  library(rstan)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  library(cmdstanr)
  library(shinystan)
  library(bayesplot)
}

set.seed(2025)
setwd("XXX") # set your working directory here


# Data --------------------------------------------------------------------

# polling data
data <- readRDS("data/data_analysis_final.rds")


# Preparation -------------------------------------------------------------


#### Stan Input ####

# design matrices for independent variables
pred_consent <-  model.matrix(~ age_sc + edu + gender + split_google + party_vote, 
                              data = data) 
pred_finish <- model.matrix(~ age_sc + edu + gender + split_google + party_vote, 
                            data = data %>% 
                              filter(consent2 == 1)) 

# Stan input data
stan_data <- list(
  N = nrow(data),
  N_obs2 = length(which(data$consent2 == 1)),
  
  P1 = ncol(pred_consent),
  P2 = ncol(pred_finish),
  
  X1 = pred_consent,
  X2 = pred_finish,
  
  Y1 = data$consent2,
  Y2 = data$finished2[which(data$consent2 == 1)]
)

# check Stan input data
sapply(stan_data, length)
sapply(stan_data, range)



# Fit model ---------------------------------------------------------------

# compile model
mod <- cmdstan_model("code/stan_ml/pobp_v4.stan")

# fit model
fit <- mod$sample(
  data = stan_data,
  chains = 4,
  iter_sampling = 10,
  iter_warmup = 10,
  refresh = 1,
  adapt_delta = 0.95,
  max_treedepth = 12
)

# check results
print(fit$summary(), n = Inf)

# transform results to data frame
posterior_df <- posterior::as_draws_df(fit$draws())

# save results
saveRDS(posterior_df, "code/posterior_df_pobp.rds")
