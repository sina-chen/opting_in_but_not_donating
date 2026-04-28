#-------------------------------------------------------------------------------
#
# Opting-in but not donating: Prepare results partially observed bivariate probit model 
#
# Author: Sina Chen
# Date: 2026-04-28
#
#-------------------------------------------------------------------------------


# Libraries ---------------------------------------------------------------

{
  library(tidyverse)
  library(bayesplot)
  library(posterior)
  library(purrr)
  library(mvtnorm)
}

set.seed(2025)



# Data --------------------------------------------------------------------

# posterior draws
posterior_df <- readRDS("code/posterior_df_pobp.rds") 

# survey data
data <- readRDS("data/data_analysis_final.rds")



# Functions ---------------------------------------------------------------

# predicted probabilities effect for continuous and binary predictors
pp_cont <- function(new_value, var_name, predictors, posterior_draws) {
  
  new_data <- predictors %>% 
    as.data.frame()
  new_data[var_name] <- new_value
  
  probs <- apply(posterior_draws, 1, function(post_row) {
    post_row <- post_row %>% 
      t() %>% 
      as.data.frame()
    
    person_probs <- apply(new_data, 1, function(data_row) {
      data_row <- data_row %>% 
        t() %>% 
        as.data.frame()
      xb1 <- post_row$`beta[1]` +  # Intercept
        post_row$`beta[2]` * data_row$age_sc +
        post_row$`beta[3]` * data_row$eduIntermediate +
        post_row$`beta[4]` * data_row$eduLower +
        post_row$`beta[5]` * data_row$gender +
        post_row$`beta[6]` * data_row$split_google +
        post_row$`beta[7]` * data_row$party_voteAfD +
        post_row$`beta[8]` * data_row$party_voteSPD +
        post_row$`beta[9]` * data_row$party_voteGreens +
        post_row$`beta[10]` * data_row$party_voteLINKE +
        post_row$`beta[11]` * data_row$party_voteBSW +
        post_row$`beta[12]` * data_row$party_voteFDP +
        post_row$`beta[13]` * data_row$party_voteother +
        post_row$`beta[14]` * data_row$`party_voteNo answer/not voted`
      
      xb2 <- post_row$`gamma[1]` +  # Intercept
        post_row$`gamma[2]` * data_row$age_sc +
        post_row$`gamma[3]` * data_row$eduIntermediate +
        post_row$`gamma[4]` * data_row$eduLower +
        post_row$`gamma[5]` * data_row$gender +
        post_row$`gamma[6]` * data_row$split_google +
        post_row$`gamma[7]` * data_row$party_voteAfD +
        post_row$`gamma[8]` * data_row$party_voteSPD +
        post_row$`gamma[9]` * data_row$party_voteGreens +
        post_row$`gamma[10]` * data_row$party_voteLINKE +
        post_row$`gamma[11]` * data_row$party_voteBSW +
        post_row$`gamma[12]` * data_row$party_voteFDP +
        post_row$`gamma[13]` * data_row$party_voteother +
        post_row$`gamma[14]` * data_row$`party_voteNo answer/not voted`
      
      p1 <- pnorm(xb1)
      p2 <- pnorm(xb2)
      
      mu <- c(xb1, xb2)
      Sigma <- matrix(c(1, post_row$rho, post_row$rho, 1), 2, 2)
      p_joint <- pmvnorm(lower = c(0, 0), upper = c(Inf, Inf), mean = mu, sigma = Sigma)[1]
      
      return(c(p1 = p1, 
               p2 = p2, 
               p_joint = p_joint))
    })
    
    p1_mean <- mean(person_probs[1,])
    p2_mean <- mean(person_probs[2,])
    p_joint_mean <- mean(person_probs[3,])
    
    return(c(p1_mean = p1_mean,
             p2_mean = p2_mean, 
             p_joint_mean = p_joint_mean))
  })
  
  res <-  tibble(
    new_value = new_value,
    mean_p1 = mean(probs[1,]),
    lower_p1 = quantile(probs[1,], 0.05, na.rm = T),
    upper_p1 = quantile(probs[1,], 0.95, na.rm = T),
    mean_p2 = mean(probs[2,]),
    lower_p2 = quantile(probs[2,], 0.05, na.rm = T),
    upper_p2 = quantile(probs[2,], 0.95, na.rm = T),
    mean_p_joint = mean(probs[3,]),
    lower_p_joint = quantile(probs[3,], 0.05, na.rm = T),
    upper_p_joint = quantile(probs[3,], 0.95, na.rm = T)
  )
  return(res)
}

# average marginal effect for categorical predictors
pp_cat <- function(new_values, var_names, predictors, posterior_draws, 
                    cat_name) {
  
  new_data <- predictors %>% 
    as.data.frame()
  
  for(i in 1:length(new_values)){
    new_data[var_names[i]] <- new_values[i]
  }
  
  probs <- apply(posterior_draws, 1, function(post_row) {
    post_row <- post_row %>% 
      t() %>% 
      as.data.frame()
    
    person_probs <- apply(new_data, 1, function(data_row) {
      data_row <- data_row %>% 
        t() %>% 
        as.data.frame()
      xb1 <- post_row$`beta[1]` +  # Intercept
        post_row$`beta[2]` * data_row$age_sc +
        post_row$`beta[3]` * data_row$eduIntermediate +
        post_row$`beta[4]` * data_row$eduLower +
        post_row$`beta[5]` * data_row$gender +
        post_row$`beta[6]` * data_row$split_google +
        post_row$`beta[7]` * data_row$party_voteAfD +
        post_row$`beta[8]` * data_row$party_voteSPD +
        post_row$`beta[9]` * data_row$party_voteGreens +
        post_row$`beta[10]` * data_row$party_voteLINKE +
        post_row$`beta[11]` * data_row$party_voteBSW +
        post_row$`beta[12]` * data_row$party_voteFDP +
        post_row$`beta[13]` * data_row$party_voteother +
        post_row$`beta[14]` * data_row$`party_voteNo answer/not voted`
      
      xb2 <- post_row$`gamma[1]` +  # Intercept
        post_row$`gamma[2]` * data_row$age_sc +
        post_row$`gamma[3]` * data_row$eduIntermediate +
        post_row$`gamma[4]` * data_row$eduLower +
        post_row$`gamma[5]` * data_row$gender +
        post_row$`gamma[6]` * data_row$split_google +
        post_row$`gamma[7]` * data_row$party_voteAfD +
        post_row$`gamma[8]` * data_row$party_voteSPD +
        post_row$`gamma[9]` * data_row$party_voteGreens +
        post_row$`gamma[10]` * data_row$party_voteLINKE +
        post_row$`gamma[11]` * data_row$party_voteBSW +
        post_row$`gamma[12]` * data_row$party_voteFDP +
        post_row$`gamma[13]` * data_row$party_voteother +
        post_row$`gamma[14]` * data_row$`party_voteNo answer/not voted`
      
      p1 <- pnorm(xb1)
      p2 <- pnorm(xb2)
      
      mu <- c(xb1, xb2)
      Sigma <- matrix(c(1, post_row$rho, post_row$rho, 1), 2, 2)
      p_joint <- pmvnorm(lower = c(0, 0), upper = c(Inf, Inf), mean = mu, 
                         sigma = Sigma)[1]
      
      return(c(p1 = p1, 
               p2 = p2, 
               p_joint = p_joint))
    })
    
    p1_mean <- mean(person_probs[1,])
    p2_mean <- mean(person_probs[2,])
    p_joint_mean <- mean(person_probs[3,])
    
    return(c(p1_mean = p1_mean,
             p2_mean = p2_mean, 
             p_joint_mean = p_joint_mean))
  })
  
  res <-  tibble(
    new_value = cat_name,
    mean_p1 = mean(probs[1,]),
    lower_p1 = quantile(probs[1,], 0.05, na.rm = T),
    upper_p1 = quantile(probs[1,], 0.95, na.rm = T),
    mean_p2 = mean(probs[2,]),
    lower_p2 = quantile(probs[2,], 0.05, na.rm = T),
    upper_p2 = quantile(probs[2,], 0.95, na.rm = T),
    mean_p_joint = mean(probs[3,]),
    lower_p_joint = quantile(probs[3,], 0.05, na.rm = T),
    upper_p_joint = quantile(probs[3,], 0.95, na.rm = T)
  )
  return(res)
}



# Preprocess data ---------------------------------------------------------

# design matrices
pred_consent <-  model.matrix(~ age_sc + edu + gender + split_google + party_vote, 
                              data = data) 
pred_finish <- model.matrix(~ age_sc + edu + gender + split_google + party_vote, 
                            data = data %>% 
                              filter(consent2 == 1)) 



# Prepare simulation results ----------------------------------------------

#### Age ####

# scaled age sequence
age_seq <- seq(-2.1, 2.1, length.out = 43)

# compute ame over age sequence
age_pp <- lapply(age_seq, function(x) 
  pp_cont(new_value = x, 
           var_name = "age_sc", 
           predictors = pred_consent, 
           posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ]))

# combine in single df
age_pp_df <- bind_rows(age_pp)

# rescale age
mean_age <- mean(data$age)
sd_age <- sd(data$age)
age_pp_df <- rename(age_pp_df, age_sc = new_value)
age_pp_df<- age_pp_df %>% 
  mutate(age = age_sc * sd_age + mean_age)

# save
saveRDS(age_pp_df, "code/age_pp.rds")


#### Gender ####

# compute pp female
gender_female_pp <- pp_cont(new_value = 0, 
                            var_name = "gender", 
                            predictors = pred_consent, 
                            posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ])
gender_female_pp <- gender_female_pp %>% 
  rename("gender" = "new_value")

# compute pp male
gender_male_pp <- pp_cont(new_value = 1, 
                          var_name = "gender", 
                          predictors = pred_consent, 
                          posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ])

gender_male_pp <- gender_male_pp %>% 
  rename("gender" = "new_value")

gender_pp_df <- rbind(gender_female_pp, gender_male_pp)
saveRDS(gender_pp_df, "code/gender_pp.rds")


#### Education ####

# compute pp education higher
edu_higher_pp <- pp_cat(new_values = c(0,0), 
                        var_names = c("eduIntermediate", "eduLower"), 
                        predictors = pred_consent, 
                        posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ],
                        cat_name = "higher")

# compute pp education intermediate
edu_intermediate_pp <- pp_cat(new_values = c(1,0), 
                               var_names = c("eduIntermediate", "eduLower"), 
                               predictors = pred_consent, 
                               posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ],
                               cat_name = "intermediate")

# compute pp education lower
edu_lower_pp <- pp_cat(new_values = c(0,1), 
                       var_names = c("eduIntermediate", "eduLower"), 
                       predictors = pred_consent, 
                       posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ],
                       cat_name = "lower")

# combine in data df
edu_pp_df <- rbind(edu_higher_pp,
                   edu_intermediate_pp,
                   edu_lower_pp)

saveRDS(edu_pp_df, "code/edu_pp.rds")


#### Party vote ####

# CDU/CSU
vote_cdu_pp <- pp_cat(new_values = c(0,0,0,0,0,0,0,0), 
                      var_names = c("party_voteAfD", "party_voteSPD", 
                                    "party_voteGreens", "party_voteLINKE",
                                    "party_voteBSW", "party_voteFDP",
                                    "party_voteother", "party_voteNo answer/not voted"), 
                      predictors = pred_consent, 
                      posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ],
                      cat_name = "CDU/CSU")

# AfD
vote_afd_pp <- pp_cat(new_values = c(1,0,0,0,0,0,0,0), 
                       var_names = c("party_voteAfD", "party_voteSPD", 
                                     "party_voteGreens", "party_voteLINKE",
                                     "party_voteBSW", "party_voteFDP",
                                     "party_voteother", "party_voteNo answer/not voted"), 
                       predictors = pred_consent, 
                       posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ],
                       cat_name = "AfD")

# SPD
vote_spd_pp <- pp_cat(new_values = c(0,1,0,0,0,0,0,0), 
                       var_names = c("party_voteAfD", "party_voteSPD", 
                                     "party_voteGreens", "party_voteLINKE",
                                     "party_voteBSW", "party_voteFDP",
                                     "party_voteother", "party_voteNo answer/not voted"), 
                       predictors = pred_consent, 
                       posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ],
                       cat_name = "SPD")

# Greens
vote_greens_pp <- pp_cat(new_values = c(0,0,1,0,0,0,0,0), 
                          var_names = c("party_voteAfD", "party_voteSPD", 
                                        "party_voteGreens", "party_voteLINKE",
                                        "party_voteBSW", "party_voteFDP",
                                        "party_voteother", "party_voteNo answer/not voted"), 
                          predictors = pred_consent, 
                          posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ],
                          cat_name = "Greens")

# LINKE
vote_linke_pp <- pp_cat(new_values = c(0,0,0,1,0,0,0,0), 
                          var_names = c("party_voteAfD", "party_voteSPD", 
                                        "party_voteGreens", "party_voteLINKE",
                                        "party_voteBSW", "party_voteFDP",
                                        "party_voteother", "party_voteNo answer/not voted"), 
                          predictors = pred_consent, 
                          posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ],
                          cat_name = "LINKE")

# BSW
vote_bsw_pp <- pp_cat(new_values = c(0,0,0,0,1,0,0,0), 
                        var_names = c("party_voteAfD", "party_voteSPD", 
                                      "party_voteGreens", "party_voteLINKE",
                                      "party_voteBSW", "party_voteFDP",
                                      "party_voteother", "party_voteNo answer/not voted"), 
                        predictors = pred_consent, 
                        posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ],
                        cat_name = "BSW")

# FDP
vote_fdp_pp <- pp_cat(new_values = c(0,0,0,0,0,1,0,0), 
                        var_names = c("party_voteAfD", "party_voteSPD", 
                                      "party_voteGreens", "party_voteLINKE",
                                      "party_voteBSW", "party_voteFDP",
                                      "party_voteother", "party_voteNo answer/not voted"), 
                        predictors = pred_consent, 
                        posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ],
                        cat_name = "FDP")

# other
vote_oth_pp <- pp_cat(new_values = c(0,0,0,0,0,0,1,0), 
                        var_names = c("party_voteAfD", "party_voteSPD", 
                                      "party_voteGreens", "party_voteLINKE",
                                      "party_voteBSW", "party_voteFDP",
                                      "party_voteother", "party_voteNo answer/not voted"), 
                        predictors = pred_consent, 
                        posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ],
                        cat_name = "other")

# not voted/na
vote_non_pp <- pp_cat(new_values = c(0,0,0,0,0,0,0,1), 
                        var_names = c("party_voteAfD", "party_voteSPD", 
                                      "party_voteGreens", "party_voteLINKE",
                                      "party_voteBSW", "party_voteFDP",
                                      "party_voteother", "party_voteNo answer/not voted"), 
                        predictors = pred_consent, 
                        posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ],
                        cat_name = "NA/not voted")

# combine in one df
vote_pp_df <- rbind(vote_cdu_pp, vote_afd_pp, vote_spd_pp, vote_greens_pp,
                    vote_linke_pp, vote_bsw_pp, vote_fdp_pp, vote_oth_pp,
                    vote_non_pp)

# save
saveRDS(vote_pp_df, "code/vote_pp.rds")



#### Split ####

# compute pp webservices
split_webservice_pp <- pp_cont(new_value = 0, 
                              var_name = "split_google", 
                              predictors = pred_consent, 
                              posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ])
split_webservice_pp <- split_webservice_pp %>% 
  rename("split_google" = "new_value")

# compute pp google
split_google_pp <- pp_cont(new_value = 1, 
                            var_name = "split_google", 
                            predictors = pred_consent, 
                            posterior_draws = posterior_df[sample(1:nrow(posterior_df), 100), ])
split_google_pp <- split_google_pp %>% 
  rename("split_google" = "new_value")

split_pp_df <- rbind(split_webservice_pp, split_google_pp)
saveRDS(split_pp_df, "code/split_pp.rds")
