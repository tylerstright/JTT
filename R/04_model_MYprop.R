# Purpose: Model proportion of tagged fish smolting during different 
# migration years.
# Author: Ryan N. Kinzer

#load packages
library(tidyverse)
library(nnet)

# load combined data rec'd from dart
load('./data/allObs.rda')

sp_dat <- mod_dat %>%
  filter(tag_season == 'Spring',
         release_site %in% c('JOHTRP', 'SECTRP')) %>%
  mutate(overwinter = relevel(overwinter, ref = '0'),
         release_year = factor(release_year))

sp_dat$overwinter <- droplevels(sp_dat$overwinter)

# intercept only model
mod_null <- multinom(overwinter ~ 1, data = sp_dat)

mod_full <- multinom(overwinter ~ release_site + release_year + release_site:release_year, data = sp_dat)
anova(mod_null, mod_full)

mod_site_year <- multinom(overwinter ~ release_site + release_year, data = sp_dat)
anova(mod_site_year, mod_full)

mod_site <- multinom(overwinter ~ release_site, data = sp_dat)
anova(mod_site, mod_site_year)
# site and year are important, interaction is also a significant term

# Full model summaries
summary(mod_full)

# calculate z-statistic
z <- summary(mod_full)$coefficients/summary(mod_full)$standard.errors
z

# calculate p-values
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Estimated proportions
mod_full$fitted.values[1:20,]

# predictive ability
chisq.test(sp_dat$overwinter, predict(mod_full))
# not a great predictive model for individuals
table(sp_dat$overwinter, predict(mod_full))
# data down rows, and model predictions across columns





