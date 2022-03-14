# R version 4.1.2
library(dplyr) # version 1.0.7
library(MASS) # version 7.3-54
library(ggplot2) # version 3.3.5
library(stargazer) # version 5.2.2
library(interplot) # version 0.2.3

#########################
## Import and clean data
#########################
setwd("C:/Users/mikeb/OneDrive - The Pennsylvania State University/covid_threat/replication")

# Import tweets
tweets <- read.csv("data/classified_tweets.csv")
# import users
users <- read.csv('data/users.csv')
# drop incomplete data and keep only users with robust ideology estimates
users <- users[complete.cases(users),]
users <- users[users['tweets'] >= 1 & users['Rhat'] <= 1.1,]

# create the 10 digit user id for tweets to join on
tweets$id10 <- as.numeric(substr(tweets$user, 1, 10))
# keep only tweets from our user sample
tweets <- tweets[tweets$id10 %in% users$id10,]
# join tweets and users so that tweets have ideological data
tweets <- left_join(tweets, users[,c('id10', 'ideology')], by = 'id10')

# Create new user variables for analysis
users$pct_black <- users$nhb/users$population
users$pct_asn <- users$asian/users$population
users$pct_hisp <- users$hisp/users$population
users$rep_share <- users$rep_votes/users$totalvotes
users$infect_rate <- users$cases/users$population
users$death_rate <- users$deaths/users$population

###########
## T test
##########
# ideology of non-compliant tweets
x <- tweets[tweets$non_comp == 1, 'ideology']
# ideology of other tweets
y <- tweets[tweets$non_comp == 0, 'ideology']
# t-test
t.test(x, y)

#####################
# NB models infection
#####################

fit_infect1 <- glm.nb(threat_min ~ 
                       # Ideology and Death
                       ideology +
                       infect_rate + 
                       
                       # Offset
                       offset(log(tweets)) +
                       
                       # Fixed Effects
                       factor(state),
                     
                     data = users)
summary(fit_infect1)

fit_infect2 <- glm.nb(threat_min ~ 
                        # Ideology and Death
                        ideology +
                        infect_rate +
                        ideology*infect_rate +
                        
                        # Offset
                        offset(log(tweets)) +
                        
                        # Fixed Effects
                        factor(state),
                      
                      data = users)
summary(fit_infect2)

fit_infect3 <- glm.nb(threat_min ~ 
                        # Ideology and Infection
                        ideology +
                        infect_rate + 
                        
                        # Offset
                        offset(log(tweets)) +
                        
                        # Age, education, density
                        p_65_up +
                        pbach_grad +
                        POPPCT_URBAN +
                        
                        # Race
                        pct_hisp +
                        pct_black +
                        pct_asn +
                        
                        # Income
                        log(med_inc) +
                        
                        #Politics
                        rep_share +
                        
                        # Fixed Effects
                        factor(state),
                      
                      data = users)
summary(fit_infect3)

fit_infect4 <- glm.nb(threat_min ~ 
                        # Ideology and Infection
                        ideology +
                        infect_rate + 
                        ideology*infect_rate +
                        
                        # Offset
                        offset(log(tweets)) +
                        
                        # Age, education, density
                        p_65_up +
                        pbach_grad +
                        POPPCT_URBAN +
                        
                        # Race
                        pct_hisp +
                        pct_black +
                        pct_asn +
                        
                        # Income
                        log(med_inc) +

                        #Politics
                        rep_share +
                        
                        # Fixed Effects
                        factor(state),
                      
                      data = users)
summary(fit_infect4)

# Full table
stargazer(fit_infect1, fit_infect2, fit_infect3, fit_infect4,
          omit = '[s][t][a][t][e]',
          add.lines = list(c('Fixed Effects', 'Yes', 'Yes', 'Yes', 'Yes')),
          dep.var.labels = 'Rate of Non-compliant Tweet Generation',
          covariate.labels = c('Ideology', 
                               'Infection Rate', 
                               'Pop. Over 65', 
                               'Pop. College Grad.', 
                               'Pop. Urban', 
                               'Pop. Hispanic', 
                               'Pop. Black', 
                               'Pop. Asian',
                               'Log(Median Income)',
                               'Rep. Vote Share', 
                               'Ideology x Infection', 
                               'Constant'),
          type = 'latex',
          no.space = T,
          label = 'tab:twit_infect')


# Truncated table
stargazer(fit_infect1, fit_infect2, fit_infect3, fit_infect4,
          omit = c('state', 'p_65_up', 'med_inc', 'pbach_grad', 'POPPCT_URBAN', 
                   'pct_black', 'pct_asn', 'pct_hisp', 'rep_share'),
          add.lines = list(c('Control variables', 'No', 'No', 'Yes', 'Yes'), 
                           c('State fixed Effects', 'Yes', 'Yes', 'Yes', 'Yes')),
          dep.var.labels = 'Rate of Non-compliant Tweet Generation',
          covariate.labels = c('Ideology', 
                               'Infection Rate', 
                               'Ideology x Infection'),
          type = 'latex',
          no.space = T,
          label = 'tab:twit_infect')

################
#NB models death
################

fit_death1 <- glm.nb(threat_min ~ 
                        # Ideology and Death
                        ideology +
                        death_rate + 
                          
                        # Offset
                        offset(log(tweets)) +
                        
                        # Fixed Effects
                        factor(state),
                      
                      data = users)
summary(fit_death1)

fit_death2 <- glm.nb(threat_min ~ 
                       # Ideology and Death
                       ideology +
                       death_rate +
                       ideology*death_rate +
                       
                       # Offset
                       offset(log(tweets)) +
                       
                       # Fixed Effects
                       factor(state),
                     
                     data = users)
summary(fit_death2)

fit_death3 <- glm.nb(threat_min ~ 
                        # Ideology and Death
                        ideology +
                        death_rate + 
                        
                        # Offset
                        offset(log(tweets)) +
                        
                        # Age, education, density
                        p_65_up +
                        pbach_grad +
                        POPPCT_URBAN +
                        
                        # Race
                        pct_hisp +
                        pct_black +
                        pct_asn +
                        
                        # Income
                        log(med_inc) +
                        
                        #Politics
                        rep_share +
                        
                        # Fixed Effects
                        factor(state),
                      
                      data = users)
summary(fit_death3)

fit_death4 <- glm.nb(threat_min ~ 
                       # Ideology and Death
                       ideology +
                       death_rate + 
                       ideology*death_rate +
                       
                       # Offset
                       offset(log(tweets)) +
                       
                       # Age, education, density
                       p_65_up +
                       pbach_grad +
                       POPPCT_URBAN +
                       
                       # Race
                       pct_hisp +
                       pct_black +
                       pct_asn +
                       
                       # Income
                       log(med_inc) +

                       #Politics
                       rep_share +
                       
                       # Fixed Effects
                       state,
                     
                     data = users)
summary(fit_death4)

# Full table
stargazer(fit_death1, fit_death2, fit_death3, fit_death4,
          omit = '[s][t][a][t][e]',
          add.lines = list(c('Fixed Effects', 'Yes', 'Yes', 'Yes', 'Yes')),
          dep.var.labels = 'Rate of Non-compliant Tweet Generation',
          covariate.labels = c('Ideology', 
                               'Death Rate', 
                               'Pop. Over 65', 
                               'Pop. College Grad.', 
                               'Pop. Urban', 
                               'Pop. Hispanic', 
                               'Pop. Black', 
                               'Pop. Asian',
                               'Log(Median Income)',
                               'Rep. Vote Share', 
                               'Ideology x Death'),
          type = 'latex',
          no.space = T,
          label = 'tab:twit_death'
          )

# Truncated table
stargazer(fit_death1, fit_death2, fit_death3, fit_death4,
          omit = c('state', 'p_65_up', 'med_inc', 'pbach_grad', 'POPPCT_URBAN', 
                   'pct_black', 'pct_asn', 'pct_hisp', 'rep_share'),
          add.lines = list(c('Control variables', 'No', 'No', 'Yes', 'Yes'), 
                           c('State fixed Effects', 'Yes', 'Yes', 'Yes', 'Yes')),
          dep.var.labels = 'Rate of Non-compliant Tweet Generation',
          covariate.labels = c('Ideology', 
                               'Death Rate', 
                               'Ideology x Death'),
          type = 'latex',
          no.space = T,
          label = 'tab:twit_death')

####################
## Interaction Plot
####################

# refit model with glm so it can pass to the interplot function
death4glm <- glm(threat_min ~ death_rate + 
                   ideology +
                   death_rate*ideology +
                   offset(log(tweets)) +
                   # Age, education, density
                   p_65_up +
                   pbach_grad +
                   POPPCT_URBAN +
                   # Race
                   pct_hisp +
                   pct_black +
                   pct_asn +
                   # Income
                   log(med_inc) +
                   #Politics
                   rep_share +
                   # Fixed Effects
                   factor(state),
                 data = users,
                 family = negative.binomial(theta = fit_death4$theta))
summary(death4glm)

# plot interaction
d4_interplot <- interplot(m = death4glm, var1 = 'ideology', var2 = 'death_rate', ci = .95) +
  xlab('Death Rate') +
  ylab('Estimated Coefficient for Ideology') +
  theme_classic()

d4_interplot
