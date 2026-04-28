#-------------------------------------------------------------------------------
#
# Opting-in but not donating: Plot results partially observed bivariate probit model 
#
# Author: Sina Chen
# Date: 2026-04-28
#
#-------------------------------------------------------------------------------


setwd("XXX") # set your working directory here



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

# predicted probability: age
age_pp_df <- readRDS("code/age_pp.rds")

# predicted probability: gender
gender_pp_df <- readRDS("code/gender_pp.rds")

# predicted probability: education
edu_pp_df <- readRDS("code/edu_pp.rds")

# predicted probability: party vote
vote_pp_df <- readRDS("code/vote_pp.rds")

# predicted probability: split
split_pp_df <- readRDS("code/split_pp.rds")



# Preparation -------------------------------------------------------------


# input data
data_prep <- data %>% 
  filter(labelled::unlabelled(t5) != -99) %>% 
  mutate(consent2 = case_when(t1087_1 == 1|t1087_2 == 1|t1087_3 == 1|
                                (t1087_1 == 0 & Datenspende == "finished") ~ 1, 
                              .default = 0),
         finished2 = if_else(donation_status %in% c("no_google_takeout_found",
                                                    "no_search_data",
                                                    "valid_data"), 1, 0),
         age = 2025-t2_cont,
         age_sc = scale(age),
         edu = case_when(t303h == 1 | t303i == 1 | t303j == 1 ~ "Higher", # ISCED 5-8
                         
                         (t3 %in% c(4,5) & if_any(c(t303a, t303b, t303e, t303m, 
                                                    t303l), ~ . == 1)) | # ISCED 3: Fachhochschulreife/ Abitur ohne weitere abgeschlossene Ausbildung
                           (t3 %in% c(4,5) & if_all(c(t303a, t303b, t303c, 
                                                      t303d, t303e, t303f, 
                                                      t303g, t303h, t303i, 
                                                      t303j, t303k, t303l, 
                                                      t303m), ~ . == 0))|
                           t303c == 1 | t303d == 1 | # ISCED 3
                           t303f == 1 | t303g == 1 | # ISCED 4
                           (t303k == 1 & if_all(c(t303h, t303i, t303j), ~ . == 0)) |
                           (t3 == 6 & if_all(c(t303a, t303b, t303c, t303d, 
                                               t303e, t303f, t303g, t303h,
                                               t303i, t303j, t303k, t303l, 
                                               t303m), ~ . == 0)) |
                           t3 == 9 ~ "Intermediate",
                         
                         t3 <= 3 & if_any(c(t303a, t303b, t303e, t303m, # ISCED 2
                                            t303l), ~ . == 1) |
                           (t3 <= 3 & if_all(c(t303a, t303b, t303c, t303d, 
                                               t303e, t303f, t303g, t303h,
                                               t303i, t303j, t303k, t303l, 
                                               t303m), ~ . == 0)) ~ "Lower") %>% 
           factor(levels = c("Higher",
                             "Intermediate",
                             "Lower")),
         gender = if_else(labelled::unlabelled(t1) == "maennlich", 1, 0),
         pol_int = case_when(t5 == 1 ~ "Very",
                             t5 == 2 ~ "Somewhat",
                             t5 >= 3 ~ "<= Moderate") %>% 
           factor(levels = c("<= Moderate", 
                             "Somewhat", 
                             "Very")),
         party_vote = case_when(t1069bb == 1 ~ "CDU/CSU",
                                t1069bb == 4 ~ "SPD",
                                t1069bb == 5 ~ "FDP",
                                t1069bb == 6 ~ "Greens",
                                t1069bb == 7 ~ "LINKE",
                                t1069bb == 322 ~ "AfD",
                                t1069bb == 392 ~ "BSW",
                                t1069bb == 800 ~ "other",
                                t1069bb < 0 ~ "No answer/not voted") %>% 
           factor(levels = c("CDU/CSU",
                             "AfD",
                             "SPD",
                             "Greens",
                             "LINKE",
                             "BSW",
                             "FDP",
                             "other",
                             "No answer/not voted")),
         split_google = case_when(t1087_1 %in% c(0,1) ~ 0,
                                  t1087_2 %in% c(0,1) ~ 1,
                                  t1087_3 %in% c(0,1) ~ 1)
  ) %>% 
  filter(!is.na(split_google))

# prepare data plot
data_plot <- posterior_df %>% 
  select(-c("lp__", "rho_raw")) %>% 
  summary() %>% 
  mutate(model = case_when(str_detect(variable, "beta") ~ "Willingness",
                           str_detect(variable, "gamma") ~ "Completion",
                           str_detect(variable, "rho") ~ "Correlation") %>% 
           factor(levels = c("Willingness", "Completion", "Correlation")),
         variable_named = case_when(variable == "beta[1]" ~ "Intercept - Willingness",
                                    variable == "beta[2]" ~ "beta[age (scaled)]",
                                    variable == "beta[3]" ~ "beta[education:intermediate]",
                                    variable == "beta[4]" ~ "beta[education:lower]",
                                    variable == "beta[5]" ~ "beta[gender:male]",
                                    variable == "beta[6]" ~ "beta[split:Google]",
                                    variable == "beta[7]" ~ "beta[vote:AfD]",
                                    variable == "beta[8]" ~ "beta[vote:SPD]",
                                    variable == "beta[9]" ~ "beta[vote:Greens]",
                                    variable == "beta[10]" ~ "beta[vote:LINKE]",
                                    variable == "beta[11]" ~ "beta[vote:BSW]",
                                    variable == "beta[12]" ~ "beta[vote:FDP]",
                                    variable == "beta[13]" ~ "beta[vote:other]",
                                    variable == "beta[14]" ~ "beta[vote:na/not voted]",
                                    variable == "gamma[1]" ~ "Intercept - Completion",
                                    variable == "gamma[2]" ~ "gamma[age (scaled)]",
                                    variable == "gamma[3]" ~ "gamma[education:intermediate]",
                                    variable == "gamma[4]" ~ "gamma[education:lower]",
                                    variable == "gamma[5]" ~ "gamma[gender:male]",
                                    variable == "gamma[6]" ~ "gamma[split:Google]",
                                    variable == "gamma[7]" ~ "gamma[vote:AfD]",
                                    variable == "gamma[8]" ~ "gamma[vote:SPD]",
                                    variable == "gamma[9]" ~ "gamma[vote:Greens]",
                                    variable == "gamma[10]" ~ "gamma[vote:LINKE]",
                                    variable == "gamma[11]" ~ "gamma[vote:BSW]",
                                    variable == "gamma[12]" ~ "gamma[vote:FDP]",
                                    variable == "gamma[13]" ~ "gamma[vote:other]",
                                    variable == "gamma[14]" ~ "gamma[vote:na/not voted]") %>% 
           factor(levels = c("Intercept - Willingness",
                             "beta[age (scaled)]",
                             "beta[education:intermediate]",
                             "beta[education:lower]",
                             "beta[gender:male]",
                             "beta[vote:AfD]",
                             "beta[vote:SPD]",
                             "beta[vote:Greens]",
                             "beta[vote:LINKE]",
                             "beta[vote:BSW]",
                             "beta[vote:FDP]",
                             "beta[vote:other]",
                             "beta[vote:na/not voted]",
                             "beta[split:Google]",
                             "Intercept - Completion",
                             "gamma[age (scaled)]",
                             "gamma[education:intermediate]",
                             "gamma[education:lower]",
                             "gamma[gender:male]",
                             "gamma[vote:AfD]",
                             "gamma[vote:SPD]",
                             "gamma[vote:Greens]",
                             "gamma[vote:LINKE]",
                             "gamma[vote:BSW]",
                             "gamma[vote:FDP]",
                             "gamma[vote:other]",
                             "gamma[vote:na/not voted]",
                             "gamma[split:Google]")
           ),
         model_version = "Initial model"
  )



# Plots -------------------------------------------------------------------


#### Raw Parameter Estimates ####

raw_estimates_plot <- ggplot(data = data_plot %>% 
                               filter(variable != "rho"),
                             aes(x = mean, y =  variable_named)) +
  geom_point(position = position_dodge(0.7))  +
  geom_errorbar(aes(xmin = q5, xmax = q95), width = 0.1, position = position_dodge(0.7)) +
  facet_wrap(~model, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  scale_y_discrete(limits=rev) +
  theme_bw() +
  labs(x = "Posterior estimates (probit scale) with 95% credible intervals", 
       y = "",
       color = "") +
  theme(text = element_text(size = 16)) 

ggsave(filename = 'code/plots/raw_estimates.png', 
       plot = raw_estimates_plot, 
       width = 14, height = 8, bg='#ffffff')


#### Predicted probability: Age ####

# reshape to longer 
age_pp_longer <- rbind(
  age_pp_df %>% 
    select(age, age_sc, mean_p1, lower_p1, upper_p1) %>% 
    rename("mean_p" = "mean_p1",
           "lower_p" = "lower_p1",
           "upper_p" = "upper_p1") %>% 
    mutate(prob = "Willingness"),
  age_pp_df %>% 
    select(age, age_sc, mean_p2, lower_p2, upper_p2) %>% 
    rename("mean_p" = "mean_p2",
           "lower_p" = "lower_p2",
           "upper_p" = "upper_p2") %>% 
    mutate(prob = "Completion"),
  age_pp_df %>% 
    select(age, age_sc, mean_p_joint, lower_p_joint, upper_p_joint) %>% 
    rename("mean_p" = "mean_p_joint",
           "lower_p" = "lower_p_joint",
           "upper_p" = "upper_p_joint") %>% 
    mutate(prob = "Joint")
)  %>% 
  mutate(prob = prob %>% 
           factor(levels = c("Willingness", "Completion", "Joint")))

# plot
pp_age_plot <- ggplot(age_pp_longer) +
  geom_line(aes(x=age, y = mean_p, color = prob)) +
  geom_ribbon(aes(ymin = lower_p, ymax = upper_p, x = age, fill = prob),
              alpha = 0.2) +
  theme_bw() +
  labs(x = "Age (in years)", y = "Predicted probability", color = "", fill = "") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_manual(values = c("#4DAF4A", "#377EB8", "#E41A1C")) +
  scale_fill_manual(values = c("#4DAF4A", "#377EB8", "#E41A1C"))

# save plot
ggsave(filename = "code/plots/pp_age.png", 
       plot = pp_age_plot, 
       width = 12, height = 5, bg='#ffffff')
  

#### Predicted probability: Gender ####

# reshape to longer
gender_pp_longer <- rbind(
  gender_pp_df %>% 
    select(gender, mean_p1, lower_p1, upper_p1) %>% 
    rename("mean_p" = "mean_p1",
           "lower_p" = "lower_p1",
           "upper_p" = "upper_p1") %>% 
    mutate(prob = "Willingness"),
  gender_pp_df %>% 
    select(gender, mean_p2, lower_p2, upper_p2) %>% 
    rename("mean_p" = "mean_p2",
           "lower_p" = "lower_p2",
           "upper_p" = "upper_p2") %>% 
    mutate(prob = "Completion"),
  gender_pp_df %>% 
    select(gender, mean_p_joint, lower_p_joint, upper_p_joint) %>% 
    rename("mean_p" = "mean_p_joint",
           "lower_p" = "lower_p_joint",
           "upper_p" = "upper_p_joint") %>% 
    mutate(prob = "Joint")
)  %>% 
  mutate(gender = if_else(gender == 0, "female", "male") %>% 
           factor(levels = c("female", "male")),
         prob = prob %>% 
           factor(levels = c("Willingness", "Completion", "Joint")))

# plot
pp_gender_plot <- ggplot(gender_pp_longer) +
  geom_pointrange(aes(ymin = lower_p, ymax = upper_p, y = mean_p, x = gender), 
                  position = position_dodge(width = 0.3)) +
  theme_bw() +
  labs(x = "", y = "Predicted probability") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        strip.text.x = element_text(size = 14)) +
  facet_wrap(~prob, ncol = 3)

ggsave(filename = "plots/pp_gender.png",
       plot = pp_gender_plot,
       width = 9, height = 4, bg='#ffffff')


#### Predicted probability: Education ####

# reshape to longer
edu_pp_longer <- rbind(
  edu_pp_df %>% 
    select(new_value, mean_p1, lower_p1, upper_p1) %>% 
    rename("mean_p" = "mean_p1",
           "lower_p" = "lower_p1",
           "upper_p" = "upper_p1") %>% 
    mutate(prob = "Willingness"),
  edu_pp_df %>% 
    select(new_value, mean_p2, lower_p2, upper_p2) %>% 
    rename("mean_p" = "mean_p2",
           "lower_p" = "lower_p2",
           "upper_p" = "upper_p2") %>% 
    mutate(prob = "Completion"),
  edu_pp_df %>% 
    select(new_value, mean_p_joint, lower_p_joint, upper_p_joint) %>% 
    rename("mean_p" = "mean_p_joint",
           "lower_p" = "lower_p_joint",
           "upper_p" = "upper_p_joint") %>% 
    mutate(prob = "Joint")
)  %>% 
  mutate(edu = new_value %>% 
           factor(levels = c( "higher", "intermediate", "lower")),
         prob = prob %>% 
           factor(levels = c("Willingness", "Completion", "Joint")))

# plot
pp_edu_plot <- ggplot(edu_pp_longer) +
  geom_pointrange(aes(ymin = lower_p, ymax = upper_p, y = mean_p, x = edu), 
                  position = position_dodge(width = 0.3)) +
  theme_bw() +
  labs(x = "", y = "Predicted probability") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        strip.text.x = element_text(size = 14)) +
  facet_wrap(~prob, ncol = 3) 

# save
ggsave(filename = "plots/pp_edu.png",
       plot = pp_edu_plot,
       width = 9, height = 4, bg='#ffffff')


#### Predicted probability: party vote ####

# reshape to longer
vote_pp_longer <- rbind(
  vote_pp_df %>% 
    select(new_value, mean_p1, lower_p1, upper_p1) %>% 
    rename("mean_p" = "mean_p1",
           "lower_p" = "lower_p1",
           "upper_p" = "upper_p1") %>% 
    mutate(prob = "Willingness"),
  vote_pp_df %>% 
    select(new_value, mean_p2, lower_p2, upper_p2) %>% 
    rename("mean_p" = "mean_p2",
           "lower_p" = "lower_p2",
           "upper_p" = "upper_p2") %>% 
    mutate(prob = "Completion"),
  vote_pp_df %>% 
    select(new_value, mean_p_joint, lower_p_joint, upper_p_joint) %>% 
    rename("mean_p" = "mean_p_joint",
           "lower_p" = "lower_p_joint",
           "upper_p" = "upper_p_joint") %>% 
    mutate(prob = "Joint")
) %>% 
  mutate(party = new_value %>% 
           factor(levels = c("CDU/CSU", "AfD", "SPD", "Greens", "LINKE", "BSW",
                             "FDP", "other", "NA/not voted")),
         prob = prob %>% 
           factor(levels = c("Willingness", "Completion", "Joint")))

# plot
pp_vote_plot2 <- ggplot(vote_pp_longer) +
  geom_pointrange(aes(ymin = lower_p, ymax = upper_p, y = mean_p, x = party,
                      color = party), 
                  position = position_dodge(width = 0.1)) +
  theme_bw() +
  labs(x = "", y = "Predicted probability", color = "") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        strip.text.x = element_text(size = 14)) +
  facet_wrap(~prob, ncol = 3) +
  scale_color_manual(values = c("#000000", "#00ADEF", "#E3001A", "#009933",
                                "#CE0058", "#E2007A", "#FFEB00", "grey40", "grey"))

# save
ggsave(filename = "code/plots/pp_vote.png",
       plot = pp_vote_plot,
       width = 12, height = 5, bg='#ffffff')


#### Predicted probability: Split ####

# reshape to longer
split_pp_longer <- rbind(
  split_pp_df %>% 
    select(split_google, mean_p1, lower_p1, upper_p1) %>% 
    rename("mean_p" = "mean_p1",
           "lower_p" = "lower_p1",
           "upper_p" = "upper_p1") %>% 
    mutate(prob = "Willingness"),
  split_pp_df %>% 
    select(split_google, mean_p2, lower_p2, upper_p2) %>% 
    rename("mean_p" = "mean_p2",
           "lower_p" = "lower_p2",
           "upper_p" = "upper_p2") %>% 
    mutate(prob = "Completion"),
  split_pp_df %>% 
    select(split_google, mean_p_joint, lower_p_joint, upper_p_joint) %>% 
    rename("mean_p" = "mean_p_joint",
           "lower_p" = "lower_p_joint",
           "upper_p" = "upper_p_joint") %>% 
    mutate(prob = "Joint")
) %>% 
  mutate(split_google = if_else(split_google == 0, "Web services", "Google") %>% 
           factor(levels = c("Google", "Web services")),
         prob = prob %>% 
           factor(levels = c("Willingness", "Completion", "Joint")))

# plot
pp_split_plot <- ggplot(split_pp_longer) +
  geom_pointrange(aes(ymin = lower_p, ymax = upper_p, y = mean_p, x = split_google), 
                  position = position_dodge(width = 0.3)) +
  theme_bw() +
  labs(x = "", y = "Predicted probability") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        strip.text.x = element_text(size = 14)) +
  facet_wrap(~prob, ncol = 3) 

# save
ggsave(filename = "plots/pp_split.png",
       plot = pp_split_plot,
       width = 9, height = 4, bg='#ffffff')
