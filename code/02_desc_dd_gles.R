#-------------------------------------------------------------------------------
#
# Opting-in but not donating: Descriptive results
#
# Author: Sina Chen
# Date: 2026-04-28
#
#-------------------------------------------------------------------------------


setwd("XXX") # set your working directory here


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(haven)
library(scales) 
library(RColorBrewer)
library(ggplot2)
library(forcats)



# Data --------------------------------------------------------------------

# t60w
data <- readRDS("data/data_analysis_final.rds")



# Preparation -------------------------------------------------------------

# subset data for willingness
data_consent <- data %>% 
  filter(consent2 == 1)

# subset data for completion
data_finished <- data_consent %>% 
  filter(finished2 == 1)


# Plots -------------------------------------------------------------------


#### Age ####

# compute age groups, sample share and participation rates
data_age_grouped <- data %>% 
  mutate(n_sample = nrow(data)) %>% 
  group_by(age_grouped, n_sample) %>% 
  summarise(n_age = n(),
            n_consent = sum(consent2==1),
            n_completion = sum(finished2==1)) %>% 
  ungroup() %>% 
  mutate(age_share = n_age/n_sample,
         consent_share = n_consent/n_age,
         completion_share = n_completion/n_age) 

# plot age
age_plot <- ggplot(data = data_age_grouped) +
  geom_histogram(aes(x= age_grouped, y = age_share, color = "Sample share (%)"),
                 stat = "identity", fill = "lightgrey") +
  geom_point(aes(x = age_grouped, 
                 y = consent_share, 
                 color = "Willingness rate (%)"),
             size = 5,
             shape = 17) +
  geom_point(aes(x = age_grouped, 
                 y = completion_share,
                 color = "Completion rate (%)"),
             size = 5,
             shape = 15) +
  labs(x = "Age in years", color = "")  +
  theme_bw() +
  scale_y_continuous(name = "Sample share (%)",
                     labels = scales::percent, 
                     sec.axis = sec_axis(name = "",
                                         transform =~.*1, 
                                         labels = scales::percent)) +
  scale_color_manual(values = c("lightgrey", "#4DAF4A", "#377EB8"), 
                     breaks = c("Sample share (%)", "Willingness rate (%)", 
                                "Completion rate (%)")) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))


#### Education ####

# compute sample share and participation rates
data_edu_grouped <- data %>% 
  mutate(n_sample = nrow(data)) %>% 
  group_by(edu, n_sample) %>% 
  summarise(n_edu = n(),
            n_consent = sum(consent2==1),
            n_completion = sum(finished2==1)) %>% 
  ungroup() %>% 
  mutate(edu_share = n_edu/n_sample,
         consent_share = n_consent/n_edu,
         completion_share = n_completion/n_edu) 

# plot education
edu_plot <- ggplot(data = data_edu_grouped) +
  geom_histogram(aes(x= edu, y = edu_share, color = "Sample share (%)"),
                 stat = "identity", fill = "lightgrey") +
  geom_point(aes(x = edu, 
                 y = consent_share, 
                 color = "Willingness rate (%)"),
             size = 5,
             shape = 17) +
  geom_point(aes(x = edu, 
                 y = completion_share,
                 color = "Completion rate (%)"),
             size = 5,
             shape = 15) +
  labs(x = "Education", color = "")  +
  theme_bw() +
  scale_y_continuous(name = "",
                     labels = scales::percent, 
                     sec.axis = sec_axis(name = "",
                                         transform =~.*1, 
                                         labels = scales::percent)) +
  scale_color_manual(values = c("lightgrey", "#4DAF4A", "#377EB8"), 
                     breaks = c("Sample share (%)", "Willingness rate (%)", 
                                "Completion rate (%)")) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))


#### Gender ####

# compute sample share and participation rates
data_gender_grouped <- data %>% 
  mutate(n_sample = nrow(data)) %>% 
  group_by(gender_labelled, n_sample) %>% 
  summarise(n_gender = n(),
            n_consent = sum(consent2==1),
            n_completion = sum(finished2==1)) %>% 
  ungroup() %>% 
  mutate(gender_share = n_gender/n_sample,
         consent_share = n_consent/n_gender,
         completion_share = n_completion/n_gender) 

# plot gender
gender_plot <- ggplot(data = data_gender_grouped) +
  geom_histogram(aes(x= gender_labelled, y = gender_share, color = "Sample share (%)"),
                 stat = "identity", fill = "lightgrey") +
  geom_point(aes(x = gender_labelled, 
                 y = consent_share, 
                 color = "Willingness rate (%)"),
             size = 5,
             shape = 17) +
  geom_point(aes(x = gender_labelled, 
                 y = completion_share,
                 color = "Completion rate (%)"),
             size = 5,
             shape = 15) +
  labs(x = "Gender", color = "")  +
  theme_bw() +
  scale_y_continuous(name = "",
                     labels = scales::percent, 
                     sec.axis = sec_axis(name = "Partcipation rate (%)",
                                         transform =~.*1, 
                                         labels = scales::percent)) +
  scale_color_manual(values = c("lightgrey", "#4DAF4A", "#377EB8"), 
                     breaks = c("Sample share (%)", "Willingness rate (%)", 
                                "Completion rate (%)")) +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))


#### Party vote ####

# compute sample share and participation rates
data_vote_grouped <- data %>% 
  mutate(n_sample = nrow(data)) %>% 
  group_by(party_vote, n_sample) %>% 
  summarise(n_vote = n(),
            n_consent = sum(consent2==1),
            n_completion = sum(finished2==1)) %>% 
  ungroup() %>% 
  mutate(vote_share = n_vote/n_sample,
         consent_share = n_consent/n_vote,
         completion_share = n_completion/n_vote) 

# plot party vote
vote_plot <- ggplot(data = data_vote_grouped) +
  geom_histogram(aes(x= party_vote, y = vote_share, color = "Sample share (%)"),
                 stat = "identity", fill = "lightgrey") +
  geom_point(aes(x = party_vote, 
                 y = consent_share, 
                 color = "Willingness rate (%)"),
             size = 5,
             shape = 17) +
  geom_point(aes(x = party_vote, 
                 y = completion_share,
                 color = "Completion rate (%)"),
             size = 5,
             shape = 15) +
  labs(x = "Party vote", color = "")  +
  theme_bw() +
  scale_y_continuous(name = "Sample share (%)",
                     labels = scales::percent, 
                     sec.axis = sec_axis(name = "Partcipation rate (%)",
                                         transform =~.*1, 
                                         labels = scales::percent)) +
  scale_color_manual(values = c("lightgrey", "#4DAF4A", "#377EB8"), 
                     breaks = c("Sample share (%)", "Willingness rate (%)", 
                                "Completion rate (%)")) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14))


#### Arrange individual characteristics plots ####

# arrange
desc_plot <- ggarrange(ggarrange(NULL,age_plot, NULL, edu_plot, NULL, gender_plot, NULL, 
                      ncol = 7, 
                                 align = "h", common.legend = F,
                      widths = c(-0.137, 1, -0.06, 0.85, -0.06, 0.65, -0.137)), 
                       vote_plot, common.legend = T, ncol = 1, 
                       align = "v", legend = "bottom", heights = c(1, 1))

# save
ggsave(desc_plot, filename = "plots/desc_plot.png", 
       width = 12, height = 8, bg='#ffffff')


#### Consent & Completion by experimental group ####

# prepare data
data_partcipation_split <- data %>% 
  mutate(participation_status = case_when(consent2 == 0 ~ "Opt-out",
                                          consent2 == 1 & finished2 == 0 ~ "Dropout",
                                          consent2 == 1 & finished2 == 1 ~ "Completed"),
         split_group = split_group %>% 
           factor(levels = c("Web services", "Google Search", "Google"))) %>% 
  group_by(split_group, participation_status) %>% 
  summarise(n_resp = n()) %>% 
  ungroup() %>% 
  group_by(split_group) %>% 
  mutate(share_resp = n_resp/sum(n_resp),
         pos=1-(cumsum(share_resp)-share_resp/2)) %>% 
  ungroup()

# plot
desc_exp_participation_plot <- ggplot(data_partcipation_split, 
                                aes(fill=participation_status, 
                                    y = share_resp, 
                                    x=split_group)) + 
  geom_bar( stat="identity") +
  coord_flip() +
  scale_fill_manual(values = rev(c("#36648B", "#63B8FF", "#FF8C69")), 
                    guide = guide_legend(reverse = T))  +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  labs(y = "Respondent share", x = "", fill = "Partcipation status")

# save
ggsave(filename = "plots/desc_participation_exp_plot.png", 
       plot = desc_exp_participation_plot, 
       width = 9, height = 2, bg='#ffffff')



#### Reasons non-participation total #####

# count by response option
no_time <- data.frame(reason = "no time",
         n_res = sum(data$t1088a == 1))

not_interested <- data.frame(reason = "not interested",
         n_res = sum(data$t1088b == 1))

insufficient_info <- data.frame(
  reason = "insufficient information\n on participation",
  n_res = sum(data$t1088c == 1))

underpayment <- data.frame(reason = "underpayment",
         n_res = sum(data$t1088d == 1))

difficulties_donation <- data.frame(
  reason = "expected difficulties\n with donation",
  n_res = sum(data$t1088e == 1))

privacy_protection <- data.frame(reason = "privacy protection",
         n_res = sum(data$t1088f == 1))

concerns_misuse <- data.frame(
  reason = "concerns about data misuse\n or security breaches",
  n_res = sum(data$t1088g == 1)) 

low_benefits <- data.frame(reason = "low benefits for\n research purposes",
         n_res = sum(data$t1088h == 1))

no_trust <- data.frame(reason = "no trust in researchers",
         n_res = sum(data$t1088i == 1)) 

concerns_webservice <- data.frame(
  reason = "concerns about web services/ \n google/google search",
  n_res = sum(data$t1088j == 1)) 

# merge reasons non partcipation
reasons_nopart <- rbind(no_time, not_interested, insufficient_info, 
                        underpayment, difficulties_donation, privacy_protection,
                        concerns_misuse, low_benefits, no_trust,
                        concerns_webservice) %>% 
  mutate(reason = factor(reason, 
                         levels = c("no time",
                                    "not interested",
                                    "insufficient information\n on participation",
                                    "underpayment",
                                    "expected difficulties\n with donation",
                                    "privacy protection",
                                    "concerns about data misuse\n or security breaches",
                                    "low benefits for\n research purposes",
                                    "no trust in researchers",
                                    "concerns about web services/ \n google/google search"
                         ))) 


# plot
reason_nonpart_plot <- ggplot(reasons_nopart) +
  geom_bar(aes(x = reason, y = n_res), 
           stat = "identity",
           position = "dodge", color = "grey", fill = "grey") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
        axis.text.y = element_text(size =12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  labs(x = "", y = "No. responses") 

ggsave(filename = "plots/reason_total_nonpart.png", 
       plot = reason_nonpart_plot, 
       width = 9, height = 6, bg='#ffffff')



#### Overview table ####

# age 
table(data$age_grouped)
prop.table(table(data$age_grouped) )

# age consent
table(data_consent$age_grouped, useNA = "always")
prop.table(table(data_consent$age_grouped) )

# age finished
table(data_finished$age_grouped, useNA = "always")
prop.table(table(data_finished$age_grouped))


# edu
table(data$edu)
prop.table(table(data$edu) )

# edu consent
table(data_consent$edu, useNA = "always")
prop.table(table(data_consent$edu) )

# edu finished
table(data_finished$edu, useNA = "always")
prop.table(table(data_finished$edu))


# gender
table(data$t1, useNA = "always")
prop.table(table(data$t1) )

# gender consent
table(data_consent$t1, useNA = "always")
prop.table(table(data_consent$t1) )

# gender completed
table(data_finished$t1, useNA = "always")
prop.table(table(data_finished$t1))


# party vote
table(data$party_vote)
prop.table(table(data$party_vote) )

# party vote consent
table(data_consent$party_vote, useNA = "always")
prop.table(table(data_consent$party_vote) )

# party vote finished
table(data_finished$party_vote, useNA = "always")
prop.table(table(data_finished$party_vote))


# split
table(data$split_group)
prop.table(table(data$split_group))

# split consent
table(data_consent$split_group, useNA = "always")
prop.table(table(data_consent$split_group) )

# split finished
table(data_finished$split_group, useNA = "always")
prop.table(table(data_finished$split_group))


#### Reason non participation table ####

# no time 
table(data$t1088a, data$split_group) %>% 
  addmargins()

# not interested 
table(data$t1088b, data$split_group) %>% 
  addmargins()

# insufficient information on participation 
table(data$t1088c, data$split_group) %>% 
  addmargins()

# underpayment
table(data$t1088d, data$split_group) %>% 
  addmargins()

# expected difficulties  with donation
table(data$t1088e, data$split_group) %>% 
  addmargins()

# privacy protection
table(data$t1088f, data$split_group) %>% 
  addmargins()

# concerns about data misuse or security breaches
table(data$t1088g, data$split_group) %>% 
  addmargins()

# low benefits forresearch purposes
table(data$t1088h, data$split_group) %>% 
  addmargins()

# no trust in researchers
table(data$t1088i, data$split_group) %>% 
  addmargins()

# concerns about web services/google/google search
table(data$t1088j, data$split_group) %>% 
  addmargins()



