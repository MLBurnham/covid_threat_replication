

library(tidyverse)
library(gridExtra)
library(survey)
library(svyVGAM)
library(stargazer)
library(InteractionPoweR)
library(wCorr)

# suppress summary warnings from dplyr
options(dplyr.summarise.inform = FALSE)

# set local directory (change for local analysis)
# setwd("/var/git/soda502/")

# load data and recode key variables
df <- read.csv("survey_res_clean.csv") %>% mutate(
  # political orientation variables
  d_politics = factor(case_when(v_politics == 1 ~ "Dem",
                                v_politics == 2 ~ "Lean Dem",
                                v_politics == 3 ~ "Indep",
                                v_politics == 4 ~ "Lean Rep",
                                v_politics == 5 ~ "Rep",
                                TRUE ~ "Other"),
                      levels=c("Dem", "Lean Dem", "Indep", "Other", "Lean Rep", "Rep")),
  v_vote_2020_pres = case_when(v_vote_2020_pres == 1 ~ 2,
                               v_vote_2020_pres == 2 ~ 1,
                               TRUE ~ v_vote_2020_pres),
  d_vote_2020_pres = factor(case_when(v_vote_2020_pres == 2 ~ "Trump",
                                      v_vote_2020_pres == 1 ~ "Biden",
                                      v_vote_2020_pres == 3 ~ "Other",
                                      TRUE ~ "No vote"),
                            levels=c("Biden", "Trump", "Other", "No vote")),
  # reduce political orientation to binary: first on voting record, then on party
  d_pol = factor(case_when(v_vote_2020_pres == 1 ~ "Left",
                           v_vote_2020_pres == 2 ~ "Right",
                           v_politics %in% c(1, 2) ~ "Left",
                           v_politics %in% c(4, 5) ~ "Right",
                           TRUE ~ "None"),
                 levels=c("Left", "Right", "None")),
  v_pol = as.integer(d_pol),
  # calculate nearest proximity to COVID-19 infection
  v_prox_inf = case_when(
    v_covid_inf_respondent == 1 ~ 5,
    v_covid_inf_household == 1 ~ 4,
    v_covid_inf_nonhousehold == 1 ~ 3,
    v_covid_inf_other == 1 ~ 2,
    TRUE ~ 1
  ),
  d_prox_inf = factor(
    case_when(v_prox_inf == 5 ~ "Respondent infected",
              v_prox_inf == 4 ~ "Household infected",
              v_prox_inf == 3 ~ "Family infected",
              v_prox_inf == 2 ~ "Acquaintance infected",
              TRUE ~ "No known infected"),
    levels=c("No known infected",
             "Acquaintance infected",
             "Family infected",
             "Household infected",
             "Respondent infected")
  ),
  # reduce infections to broad categories
  v_prox_inf_any = case_when(
    v_prox_inf == 5 ~ 3,
    v_prox_inf == 1 ~ 1,
    TRUE ~ 2
  ),
  d_prox_inf_any = factor(
    case_when(v_prox_inf_any == 3 ~ "Respondent infected",
              v_prox_inf_any == 2 ~ "Known infected",
              v_prox_inf_any == 1 ~ "No known infected"),
    levels=c("No known infected",
             "Known infected",
             "Respondent infected")
  ),
  v_prox_inf_none = (v_prox_inf_any == 1),
  v_prox_inf_known = (v_prox_inf_any == 2),
  v_prox_inf_resp = (v_prox_inf_any == 3),
  # calculate nearest proximity to COVID-19 death
  v_prox_death = case_when(
    v_covid_death_household == 1 ~ 4,
    v_covid_death_nonhousehold == 1 ~ 3,
    v_covid_death_other == 1 ~ 2,
    TRUE ~ 1
  ),
  d_prox_death = factor(case_when(v_prox_death == 4 ~ "Household death",
                                  v_prox_death == 3 ~ "Family death",
                                  v_prox_death == 2 ~ "Acquaintance death",
                                  TRUE ~ "No known death"),
                        levels=c("No known death",
                                 "Acquaintance death",
                                 "Family death",
                                 "Household death")),
  v_prox_death_any = case_when(v_prox_death == 1 ~ 1,
                               TRUE ~ 2),
  v_prox_death_any_bin = (v_prox_death_any == 2),
  v_prox_death_none = (v_prox_death_any == 1),
  d_prox_death_any = factor(case_when(v_prox_death_any == 1 ~ "No known death",
                                      v_prox_death_any == 2 ~ "Known death"),
                            levels=c("No known death",
                                     "Known death")),
  # vaccine hesitancy and self-reported concern status as coded in survey
  v_vax_hes_status = case_when(v_vaccinated == 1 ~ 1,
                               v_unvaxxed_not_hesitant == 1 ~ 2,
                               v_unvaxxed_some_hesitant == 1 ~ 3,
                               TRUE ~ 4),
  d_vax_hes_status = factor(case_when(v_vax_hes_status == 1 ~ "Vax",
                                      v_vax_hes_status == 2 ~ "Unvax, no hes",
                                      v_vax_hes_status == 3 ~ "Unvax, some hes",
                                      TRUE ~ "Unvax, very hes"),
                            levels=c("Vax",
                                     "Unvax, no hes",
                                     "Unvax, some hes",
                                     "Unvax, very hes")),
  d_covid_concern=factor(d_covid_concern,
                         levels=c("Not at all concerned",
                                  "A little concerned",
                                  "Moderately concerned",
                                  "Very concerned")),
  v_covid_concern_none=(v_covid_concern == 1),
  v_covid_concern_some=(v_covid_concern == 2),
  v_covid_concern_mod=(v_covid_concern == 3),
  v_covid_concern_very=(v_covid_concern == 4),
  # behavioral measures coding
  d_beh_wear_mask_indoors=factor(v_beh_wear_mask_indoors,
                                   levels=c(1,2,3),
                                   labels=c("Will follow",
                                            "Maybe follow",
                                            "Will NOT follow")),
  d_beh_social_distancing=factor(v_beh_social_distancing,
                                 levels=c(1,2,3),
                                 labels=c("Will follow",
                                          "Maybe follow",
                                          "Will NOT follow")),
  v_vax_bin = case_when(v_vaccinated + v_unvaxxed_not_hesitant >= 1 ~ 1,
                        TRUE ~ 0),
  d_vax_bin = case_when(v_vax_bin == 1 ~ "ProVax",
                        v_vax_bin == 0 ~ "AntiVax"),
  v_mask_bin = case_when(v_beh_wear_mask_indoors <= 2 ~ 1,
                         TRUE ~ 0),
  d_mask_bin = case_when(v_mask_bin == 1 ~ "ProMask",
                         v_mask_bin == 0 ~ "AntiMask"),
  v_dist_bin = case_when(v_beh_social_distancing <= 2 ~ 1,
                         TRUE ~ 0),
  d_dist_bin = case_when(v_dist_bin == 1 ~ "ProSocialDist",
                         v_dist_bin == 0 ~ "AntiSocialDist"),
  # demographic variable coding
  v_age_s = case_when(v_age10 <= 5 ~ 0,
                      TRUE ~ 1),
  d_age_s = factor(case_when(v_age_s == 0 ~ "Age <= 60",
                             TRUE ~ "Age > 60"),
                   levels=c("Age <= 60", "Age > 60")),
  v_edu_s = case_when(v_educ <= 4 ~ 0,
                      TRUE ~ 1),
  d_edu_s = factor(case_when(v_edu_s == 0 ~ "Less than 4-year degree",
                             TRUE ~ "4-year degree or more"),
                   levels=c("Less than 4-year degree",
                            "4-year degree or more")),
  v_urb_s = case_when(v_urban == 5 ~ 0,
                      TRUE ~ 1),
  d_urb_s = factor(case_when(v_urb_s == 0 ~ "Rural",
                             TRUE ~ "UrbanOrSuburban"),
                   levels=c("Rural", "UrbanOrSuburban")),
  v_gen_s = case_when(v_gender == 1 ~ 0,
                      TRUE ~ 1),
  d_gen_s = factor(case_when(v_gen_s == 0 ~ "Man",
                             TRUE ~ "WomanOrOther"),
                   levels=c("Man", "WomanOrOther")),
  d_race = factor(d_race,
                  levels=c("White, not-Hispanic",
                           "African American or Black",
                           "Asian American",
                           "Hispanic or Latino",
                           "Native American / American Indian",
                           "Pacific Islander")),
  d_income_s = factor(
    case_when(v_income %in% c(4, 5, 6) ~ "Upper50%",
                         TRUE ~ "Lower50%"),
    levels=c("Lower50%", "Upper50%"))
)

# filtering: removing individuals with no voting or party affiliations
tdf <- df %>% filter(d_pol != "None")

# baseline survey design model (shared across all models)
svydes <- svydesign(id=~uuid, weights=~weight, data=tdf)

# figure: raw respondents by party affiliation and voting patterns
ggplot(data=df) +
  geom_bar(mapping=aes(x=d_politics, fill=d_vote_2020_pres),
           position="dodge") +
  scale_fill_manual(name="Election vote",
                    values=c("Biden"="blue",
                             "Trump"="red",
                             "Other"="#1da334",
                             "No vote"="grey")) +
  xlab("Party affiliation") +
  ylab("Raw num. respondents")

# figure: weighted respondents by party affiliation and voting patterns
ggplot(data=df %>%
         group_by(d_politics, d_vote_2020_pres) %>%
         summarise(counts=sum(weight))) +
  geom_bar(aes(x=d_politics,
               y=counts, fill=factor(d_vote_2020_pres)),
           position="fill", stat="identity") +
  scale_fill_manual(name="Election vote",
                    values=c("Biden"="blue",
                             "Trump"="red",
                             "Other"="#1da334",
                             "No vote"="grey")) +
  xlab("Political party affiliation") +
  ylab("Weighted 2020 presidential election vote percentage") +
  ggtitle("Weighted party affiliation and 2020 presidential voting record")

# tables: respondent risk perceptions by party affiliation
ggplot(data=tdf %>%
         group_by(d_politics, d_covid_concern) %>%
         summarise(counts=sum(weight))) +
  geom_bar(aes(x=d_politics,
               y=counts, fill=factor(d_covid_concern)),
           position="fill", stat="identity") +
  xlab("Political party affiliation") +
  ylab("Weighted % concern status") +
  ggtitle("Weighted self-reported COVID-19 concern by party affiliation") +
  guides(fill=guide_legend(title="Self-reported COVID-19 concern")) +
  theme(axis.text.x=element_text(angle=90))

ggplot(data=tdf %>%
         group_by(d_politics, d_prox_inf) %>%
         summarise(counts=sum(weight))) +
  geom_bar(aes(x=d_politics,
               y=counts, fill=factor(d_prox_inf)),
           position="fill", stat="identity") +
  xlab("Political party affiliation") +
  ylab("Weighted % infection status") +
  ggtitle("Weighted known infection status by party affiliation") +
  guides(fill=guide_legend(title="Proximity to COVID-19 infection")) +
  theme(axis.text.x=element_text(angle=90))

ggplot(data=tdf %>%
         group_by(d_politics, d_prox_death) %>%
         summarise(counts=sum(weight))) +
  geom_bar(aes(x=d_politics,
               y=counts, fill=factor(d_prox_death)),
           position="fill", stat="identity") +
  xlab("Political party affiliation") +
  ylab("Weighted % death status") +
  ggtitle("Weighted known death status by party affiliation") +
  guides(fill=guide_legend(title="Proximity to COVID-19 death")) +
  theme(axis.text.x=element_text(angle=90))

# interaction EDA plots

interaction_eda_plot = function(beh_var, risk_var, beh_scale) {
  #'
  #' @param beh_var string, behavioral variable name
  #' @param risk_var string, risk variable name
  #' @param beh_scale scale_fill_manual ggplot2 object
  #'
  ggplot(data=tdf %>%
           group_by_at(vars("d_pol", beh_var, risk_var)) %>%
           summarise(counts=sum(weight))) +
    geom_bar(aes(x=d_pol,
                 y=counts,
                 fill=get(beh_var)),
             position="fill", stat="identity") +
    xlab("Political orientation") +
    ylab("Weighted proportion") +
    facet_grid(as.formula(paste("~", risk_var))) +
    theme(axis.text.x = element_text(angle=90)) +
    beh_scale
}

interaction_eda_plot_group = function(risk_var) {
  p1 <- interaction_eda_plot("d_vax_hes_status", risk_var, vax_scale)
  p2 <- interaction_eda_plot("d_beh_wear_mask_indoors", risk_var, mask_scale)
  p3 <- interaction_eda_plot("d_beh_social_distancing", risk_var, dist_scale)

  grid.arrange(p1, p2, p3, nrow=3)
}
interaction_eda_plot_group("d_covid_concern")
interaction_eda_plot_group("d_prox_inf_any")
interaction_eda_plot_group("d_prox_death_any")


survey_model_group= function(beh_var,
                             beh_var_name,
                             risk_var,
                             risk_levels) {
  #'
  #' @param beh_var string, name of behavior (response) variable
  #' @param beh_var_name string, label for behavior in output table
  #' @param risk_var string, name of risk (predictor) variable
  #' @param risk_levels string vector, labels for risk levels
  #'

  # build model formulas
  fixed_mod_form = as.formula(paste(beh_var, "~", risk_var, "+ d_pol"))
  int_mod_form = as.formula(paste(beh_var, "~", risk_var, "* d_pol"))
  fixed_mod_controls_form = as.formula(
    paste(
      beh_var,
      "~ d_gen_s + d_age_s + d_edu_s + d_urb_s + d_race + d_income_s +",
      risk_var,
      "+ d_pol"
    )
  )
  int_mod_controls_form = as.formula(
    paste(
      beh_var,
      "~ d_gen_s + d_age_s + d_edu_s + d_urb_s + d_race + d_income_s +",
      risk_var,
      "* d_pol"
    )
  )

  # fit models
  fixed_mod = svyglm(fixed_mod_form,
                     design=svydes,
                     family=binomial(link="logit"),
                     rescale=TRUE)
  int_mod = svyglm(int_mod_form,
                   design=svydes,
                   family=binomial(link="logit"),
                   rescale=TRUE)
  fixed_controls_mod = svyglm(fixed_mod_controls_form,
                     design=svydes,
                     family=binomial(link="logit"),
                     rescale=TRUE)
  int_controls_mod = svyglm(int_mod_controls_form,
                   design=svydes,
                   family=binomial(link="logit"),
                   rescale=TRUE)

  # construct model results table
  stargazer(
    fixed_mod,
    int_mod,
    fixed_controls_mod,
    int_controls_mod,
    align=TRUE,
    omit=c("d_gen_s", "d_age_s", "d_edu_s", "d_urb_s", "d_race", "d_income_s"),
    dep.var.labels=c(paste("Weighted log-odds of", beh_var_name)),
    no.space=TRUE,
    omit.stat=c("ser"),
    covariate.labels=c(
      c(paste("Risk:", risk_levels)),
      c("Politics: right-oriented"),
      c(paste("Interaction: right-oriented x", risk_levels)),
      "Constant"
    ),
    add.lines=list(c(
      "Control Variables", "No", "No", "Yes", "Yes"
    ))
  )
}

# create modeling tables for 3 behavioral measures x 3 risk measures = 9 tables

# vaccination models
survey_model_group("v_vax_bin",
                   "Vaccination",
                   "d_covid_concern",
                   c("little concerned",
                     "moderately concerned",
                     "very concerned"))

survey_model_group("v_vax_bin",
                   "Vaccination",
                   "d_prox_inf_any",
                   c("Known infected",
                     "Respondent infected"))

survey_model_group("v_vax_bin",
                   "Vaccination",
                   "d_prox_death_any",
                   c("Known death"))

# masking models
survey_model_group("v_mask_bin",
                   "Masking",
                   "d_covid_concern",
                   c("little concerned",
                     "moderately concerned",
                     "very concerned"))

survey_model_group("v_mask_bin",
                   "Masking",
                   "d_prox_inf_any",
                   c("Known infected",
                     "Respondent infected"))

survey_model_group("v_mask_bin",
                   "Masking",
                   "d_prox_death_any",
                   c("Known death"))

# social distancing models
survey_model_group("v_dist_bin",
                   "Social Distancing",
                   "d_covid_concern",
                   c("little concerned",
                     "moderately concerned",
                     "very concerned"))

survey_model_group("v_dist_bin",
                   "Social Distancing",
                   "d_prox_inf_any",
                   c("Known infected",
                     "Respondent infected"))

survey_model_group("v_dist_bin",
                   "Social Distancing",
                   "d_prox_death_any",
                   c("Known death"))


vax_scale = scale_fill_manual(name="Vaccine hesitancy",
                              values=c("Vax"="#1da334",
                                       "Unvax, no hes"="yellow",
                                       "Unvax, some hes"="orange",
                                       "Unvax, very hes"="red"))

mask_scale = scale_fill_manual(name="Mask adherence",
                               values=c("Will follow"="#1da334",
                                        "Maybe follow"="orange",
                                        "Will NOT follow"="red"))

dist_scale = scale_fill_manual(name="Social distancing",
                               values=c("Will follow"="#1da334",
                                        "Maybe follow"="orange",
                                        "Will NOT follow"="red"))


# ------------------------------------------------------------------------------------
# power analysis 

set.seed(20220312)

power_analysis_survey_ints = function(beh_var,
                                      beh_name,
                                      risk_var,
                                      risk_name,
                                      ref_var,
                                      effect_levels) {
  
  # recalculate model to get interaction survey design effect
  int_mod = svyglm(
    paste(
      beh_var,
      "~ d_gen_s + d_age_s + d_edu_s + d_urb_s + d_race + d_income_s +",
      risk_var,
      "* d_pol"
    ),
    design=svydes,
    family=quasibinomial(link="logit"),
    rescale=TRUE,
    deff=TRUE
  )
  
  # calculate effects for power analysis from observed data
  tdf2 = tdf[tdf[, beh_var] == 1 | tdf[, ref_var] == 1, ]
  ess = dim(tdf2)[1] / tail(deff(int_mod), 1)
  
  rx1y = weightedCorr(tdf2[, beh_var], 
                      tdf2[, risk_var], 
                      method="Pearson",
                      weights=tdf2[, "weight"])
  rx2y = weightedCorr(tdf2[, beh_var], 
                      tdf2[, "v_pol"], 
                      method="Pearson",
                      weights=tdf2[, "weight"])
  rx12 = weightedCorr(tdf2[, "v_pol"], 
                      tdf2[, risk_var], 
                      method="Pearson",
                      weights=tdf2[, "weight"])
  rx12y = weightedCorr(tdf2[, beh_var], 
                       tdf2[, "v_pol"] * tdf2[, risk_var], 
                       method="Pearson",
                       weights=tdf2[, "weight"])
  
  
  # calculate power at different interaction effect sizes
  pi_values = power_interaction(n.iter=1000, 
                                N=ess,
                                r.x1.y=rx1y,
                                r.x2.y=rx2y,
                                r.x1x2.y = effect_levels,
                                r.x1.x2 = rx12,
                                k.x1=2,
                                k.x2=2,
                                k.y=2,
                                seed=20220312,
                                adjust.correlations=FALSE)
  # add observed effect size to plot
  obs_int = (
    weightedCorr(tdf2[, beh_var], 
                 tdf2[, "v_pol"] * tdf2[, risk_var],
                 method="Pearson",
                 weights=tdf2[, "weight"]) 
  )
  
  return(
    plot_power_curve(pi_values, power_target=0) + 
      xlab(paste0("Right Politics X ",
                  risk_name, 
                  " on ",
                  beh_name)) + 
      geom_vline(xintercept=obs_int,
                 color="red") + 
      ylim(-.05, 1.05)
  )
}


vxc1 = power_analysis_survey_ints(
  "v_vax_bin", 
  "Vaccination", 
  "v_covid_concern_some", 
  "COVID concern = 'a little'",
  "v_covid_concern_none",
  seq(.01, .31, .05)
)
vxc2 = power_analysis_survey_ints(
  "v_vax_bin", 
  "Vaccination", 
  "v_covid_concern_mod", 
  "COVID concern = 'moderately'",
  "v_covid_concern_none",
  seq(.01, .31, .05)
)
vxc3 = power_analysis_survey_ints(
  "v_vax_bin", 
  "Vaccination", 
  "v_covid_concern_very", 
  "COVID concern = 'very'",
  "v_covid_concern_none",
  seq(.01, .31, .05)
)

vxd = power_analysis_survey_ints(
  "v_vax_bin", 
  "Vaccination", 
  "v_prox_death_any_bin", 
  "Respondent knows COVID death",
  "v_prox_death_none",
  seq(.01, .31, .05)
)
  
grid.arrange(vxc1, vxc2, vxc3, vxd, ncol=1)





