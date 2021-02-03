# Purpose: Explore steelhead migration data.
# Author: Ryan N. Kinzer

#load packages
library(tidyverse)
library(cuyem)

# load combined data rec'd from dart
load('./data/allObs.rda')

# get first obs and plot
mod_dat %>%
  ggplot(aes(x = length, y = overwinter, colour = as.factor(release_year))) +
  geom_jitter(alpha = .25) +
  facet_grid(tag_season ~ release_site) +
  labs(x = 'Fork Length at Tagging',
       y = 'Overwinters from Tagging')

# calc proportions
mig_p <- mod_dat %>%
  est_group_p(.summary_var = overwinter,
              alpha = 0.05,
              release_site,
              release_year,
              tag_season)

# plot proportions
mig_p %>%
  ggplot(aes(x = tag_season, y = p,group = release_year, colour = as.factor(release_year)))+
  geom_pointrange(aes(ymin = lwr, ymax = upr),
                  position = position_dodge(width = .5)) +
  facet_grid(overwinter~release_site)
