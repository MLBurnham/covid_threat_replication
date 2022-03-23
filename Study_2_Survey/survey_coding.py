"""
Survey item preprocessing

File requirements:
- Survey responses: aarc_vax_final_june9.csv
- Codebook: aarc_vax_codebook.xlsx
"""

import os
import pandas as pd

# set working directory here
# os.chdir('/private/var/git/soda502')

# target questions: survey item names mapped to descriptive keys
target_qs = {
    "S2": "race",
    "S3": "state",
    "S5": "gender",
    "age": "age10",
    "S10": "educ",
    "S12": "urban",
    "S18": "politics",
    "Q1": "flu_vax_typical",
    "Q2": "flu_vax_2021",
    "Q3": "covid_vax_status",
    "Q4r1": "unvax_plan_soonest",
    "Q4r2": "unvax_scheduled",
    "Q4r3": "unvax_intent_wo_plan",
    "Q4r4": "unvax_deciding",
    "Q4r5": "unvax_no_plan",
    "Q4r6": "unvax_refusal",
    "Q4r7": "unvax_ineligible",
    "Q4r8": "unvax_unknown",
    "HESITANTr1": "vaccinated",
    "HESITANTr2": "unvaxxed_not_hesitant",
    "HESITANTr3": "unvaxxed_some_hesitant",
    "HESITANTr4": "unvaxxed_very_hesitant",
    "Q5": "perceived_covid_reported_concern",
    "Q6": "perceived_social_ties_covid",
    "Q7": "perceived_vax_endorsed_FDA_CDC_safe",
    "Q8": "perceived_social_ties_disagreements",
    "Q9": "perceived_vax_safety",
    "Q10": "perceived_vax_race_diff",
    "Q11": "perceived_vax_health_inequity",
    "Q13ra": "beh_stay_home_sick",
    "Q13rb": "beh_seek_doctor_if_symptomatic",
    "Q13rc": "beh_wear_mask_indoors",
    "Q13rd": "beh_avoid_mass_gatherings",
    "Q13re": "beh_wash_hands_regularly",
    "Q13rf": "beh_social_distancing",
    "Q14r1": "covid_inf_respondent",
    "Q14r2": "covid_inf_household",
    "Q14r3": "covid_inf_nonhousehold",
    "Q14r4": "covid_inf_other",
    "Q14r5": "covid_inf_no_known",
    "Q18r1": "covid_death_household",
    "Q18r2": "covid_death_nonhousehold",
    "Q18r3": "covid_death_other",
    "Q18r4": "covid_death_no_known",
    "Q19": "covid_concern",
    "Q24ra": "covid_econ_lost_job",
    "Q24rb": "covid_econ_lost_hours",
    "Q24rc": "covid_econ_workplace_closed",
    "Q24rd": "covid_econ_unemployed",
    "Q24re": "covid_econ_lost_insurance",
    "D16": "vote_2020_pres",
    "D17": "income"
}

# load survey responses and codebook
df = pd.read_csv("aarc_vax_final_june9.csv", low_memory=False)
codebook = pd.read_excel("aarc_vax_codebook.xlsx")
aux_vars = df[['uuid', 'weight']]

# merge codebook to responses by melting on item value pairs
odf = pd.merge(
    pd.melt(df[list(target_qs.keys()) + ["uuid"]], id_vars="uuid"),
    pd.DataFrame({"variable": codebook["Variable"].ffill(),
                  "value": codebook["code"],
                  "a_desc": codebook["Description"]}),
    on=("variable", "value"),
    how="left"
)
odf["q_desc"] = odf["variable"].map(target_qs)

# repivot table and relabel columns
odf = pd.pivot(odf, index="uuid", columns="q_desc", values=["a_desc", "value"])
odf.columns = [("d_" if i == "a_desc" else "v_") + j for (i, j) in odf.columns]
odf = pd.merge(odf, aux_vars, on='uuid', how='inner')

# write output to same directory as input
odf.to_csv("survey_res_clean.csv")
