setwd("C:\\Users\\mikeb\\OneDrive - The Pennsylvania State University\\covid_threat\\replication\\covid_threat_replication\\Study_1_Twitter")
library(InteractionPoweR)
library(ggplot2)
library(gridExtra)
users <- read.csv('users.csv')

# variables are normalized during the simulation. This poses a problem for ideology
# since the distribution is strongly bimodal. We therefore convert it to a binary variable
# for the power analysis. This weakens the correlations.
users$con <- ifelse(users$ideology > -0.167, 1, 0)

# death rate power analysis
rx1x2y <- cor(users$con*users$death_rate, users$non_compliant)
rx1y <- cor(users$con, users$non_compliant)
rx2y <- cor(users$death_rate, users$non_compliant)
rx1x2 <- cor(users$con, users$death_rate)
# difference in correlation by making ideology binary.
cor(users$ideology*users$death_rate, users$non_compliant) - rx1x2y

test_power <- power_interaction(
  n.iter = 1000,
  alpha = .05,            
  N = nrow(users),                  
  #r.x1x2.y = seq(.01, .26, .02),
  r.x1x2.y = c(.01, .015, .02, .025, .03, .05, .075, .1, .2, .3), 
  r.x1.y = rx1y,              
  r.x2.y = rx2y,              
  r.x1.x2 = rx1x2,
  k.x1 = 2,
  seed = 907,             
  cl = 18
)


death.plt <- plot_power_curve(test_power, power_target=0) + 
    xlab("Ideology X Death Rate") + 
    geom_vline(xintercept=rx1x2y,
               color="red") + 
    ylim(-.05, 1.05)
death.plt


# Infected power analysis
rx1x2y <- cor(users$con*users$infect_rate, users$non_compliant)
rx1y <- cor(users$con, users$non_compliant)
rx2y <- cor(users$infect_rate, users$non_compliant)
rx1x2 <- cor(users$con, users$infect_rate)
# difference in correlation by making ideology binary.
cor(users$ideology*users$infect_rate, users$non_compliant) - rx1x2y

test_power <- power_interaction(
  n.iter = 1000,
  alpha = .05,            
  N = nrow(users),                  
  r.x1x2.y = c(.01, .015, .02, .025, .03, .05, .075, .1, .2, .3), 
  r.x1.y = rx1y,              
  r.x2.y = rx2y,              
  r.x1.x2 = rx1x2,
  k.x1 = 2,
  seed = 907,             
  cl = 18
)
test_power

infect.plt <- plot_power_curve(test_power, power_target=0) + 
  xlab("Ideology X Infection Rate") + 
  geom_vline(xintercept=rx1x2y,
             color="red") + 
  ylim(-.05, 1.05)



grid.arrange(death.plt, infect.plt, ncol=1)
