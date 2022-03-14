# R version 4.1.2
library(irr) # version 0.84.1

setwd("C:/Users/mikeb/OneDrive - The Pennsylvania State University/covid_threat")
coded <- read.csv('training_tweets_labeled.csv')

#########################
## Variable Descriptions
#########################
# toss: A flag for ambiguous tweets that will be dropped for training

# non_comp: The final label for non-compliant tweets used in training.
# Represents the reconciled label assigned by the researchers when there
# was disagreement between the coders

# coder1nonc and coder2nonc: Labels assigned by coder 1 and 2 respectively

###############
## Reliability
###############
# code agreement between coders
coded$coderagree <- ifelse(coded$coder1nonc == coded$coder2nonc, 1, 0)

# Intercoder reliability metrics
# Cohen's Kappa
kappa2(coded[,c("coder1nonc", "coder2nonc")]) # kappa = 0.81, z = 36.2

# Percent agreement
sum(coded$coderagree)/1999 # agreement = 0.925