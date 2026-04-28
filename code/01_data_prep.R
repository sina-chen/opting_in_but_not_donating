#-------------------------------------------------------------------------------
# 
# Opting-in but not donating: Data Preparation
#
# Author: Sina Chen
# Date: 2026-04-28
# 
#-------------------------------------------------------------------------------


setwd("XXX") # set your working directory here



# Libraries ---------------------------------------------------------------

{
  library(tidyverse)
  library(haven)
  library(readxl)
}

set.seed(2025)



# Data --------------------------------------------------------------------

# T60W: DD short survey data with meta data
t60w <- read_dta("data/ZA10132_v2-0-0.dta")

# T60: GLES Tracking data
t60 <- read_dta("data/ZA10105_v1-0-0.dta")

# year of birth continuous (contact the authors for this information)
yob <- readRDS("data/t60_yob.rds")



# Merge data --------------------------------------------------------------

# merge T60w and T60 survey data
survey_data <- merge(t60w %>% 
                       select(lfdn, t1069bb, t1087_1, t1087_2, t1087_3, t1088a,
                              t1088b, t1088c, t1088d, t1088e, t1088f, t1088g,
                              t1088h, t1088i, t1088j, don_status, don_process), 
                     t60 %>% 
                       select(lfdn, t1, t2, t3, t5, t303a, t303b, t303c, t303d,
                              t303e, t303f, t303g, t303h, t303i, t303j, t303k,
                              t303l, t303m), by = "lfdn")

# merge continuous year of birth to survey data
survey_yob <- merge(survey_data, yob, by.x = "lfdn", by.y = "lfdn")
rm(t60, t60w, yob, survey_data)

# Generate variables for analysis -----------------------------------------

# generate relevant variables
data <- survey_yob %>% 
  filter(labelled::unlabelled(t5) != -99) %>% 
  mutate(consent2 = if_else(t1087_1 == 1|t1087_2 == 1|t1087_3 == 1|
                                (t1087_1 == 0 & don_process == 2), 1, 
                              0),
         finished2 = if_else(don_status %in% c(1,2,4,5), 1, 0),
         age = 2025-t2_cont,
         age_sc = c(scale(age)),
         age_grouped = case_when(age < 30 ~ "18-29",
                                 age >= 30 & age < 45 ~ "30-44",
                                 age >= 45 & age < 60 ~ "45-60",
                                 age >= 60 ~ "60+"),
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
         gender_labelled = if_else(labelled::unlabelled(t1) == "maennlich", 
                                 "Male", "Female"),
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
         pol_int = case_when(t5 == 1 ~ "Very",
                             t5 == 2 ~ "Somewhat",
                             t5 >= 3 ~ "<= Moderate"),
         split_google = case_when(t1087_1 %in% c(0,1) ~ 0,
                                  t1087_2 %in% c(0,1) ~ 1,
                                  t1087_3 %in% c(0,1) ~ 1),
         split_group = case_when(t1087_1 %in% c(0,1) ~ "Web services",
                                 t1087_2 %in% c(0,1) ~ "Google",
                                 t1087_3 %in% c(0,1) ~ "Google Search",
                                 .default = NA) ,
         t1088_answered = if_else(t1088a == 1|t1088b == 1|t1088c == 1|
                                    t1088d == 1|t1088e == 1|t1088f == 1|
                                    t1088g == 1|t1088h == 1|t1088i == 1|
                                    t1088j == 1, 1, 0)
  ) %>% 
  filter(!is.na(split_google))


# save data
saveRDS(data, "data/data_analysis_final.rds")




