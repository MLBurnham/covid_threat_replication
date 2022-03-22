# <COVID-19 Threat Perception Replication Archive>

## Description

This is the replication archive for Perceived Risk, Political Polarization, and Adherence to COVID-19 Mitigation Guidelines

## Table of Contents 

### Study 1: Twitter

- covid_classifier_training.IPYNB: A notebook for training the transformer model used for classifying tweets. The notebook is best run in google colab and can be done easily using a free account. A GPU is required.
- requirements.txt: The requirements for running the classifier notebook if not using Colab.
- tweets_textless.csv: Contains tweet IDs and their classification as compliant or non-compliant. Text is removed as per Twitter's API user agreement. Tweets can be rehydrated using the tweet ID and the twitter API.
- twitter_figures.R: R script to replicate the plots for the Twitter study. Requires full text of the tweets.
- twitter_ICR.R: R script for intercoder reliability on the labeled tweets used for training the classifier.
- twitter_models.R: R script to replicate the models presented in Study 1.
- twitter_PA.R: R script to replicate power analysis for Twitter data.
- users.csv: User level data that includes ideology, twitter activity, and county level demographics and covid data. Used by twitter_models.R

### Study 2: Survey
- aarc_survey_analysis.R: Contains regression and power analysis for study 2.
- sessionInfo: Packages and versions used for aarc_survey_analysis.R.
