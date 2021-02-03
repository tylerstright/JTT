# Purpose: Explore steelhead migration data.
# Author: Ryan N. Kinzer

#load packages
library(tidyverse)
library(cuyem)

# load data
load('./data/dobos/sth_data.rda')

# quick look
n_distinct(sth_data$tag_code) # one extra record?
# why don't we have multiple obs of the same tags

sth_data %>% group_by(site_first_name, site_last_name) %>% tally()
# only the first juv obs and last adult obs?

# load data rec'd from dart
load('./data/dart_pit_obs_19_20.rda')

# add some fields
dat <- PIT_obs %>%
  mutate(migration_year = lubridate::year(obs_time),
         overwinter = migration_year - release_year,
         tag_season = case_when(
           between(lubridate::month(release_date),1,6) ~ 'Spring',
           between(lubridate::month(release_date),7,8) ~ 'Summer',
           between(lubridate::month(release_date),9,12) ~ 'Fall'),
         tag_season = fct_relevel(tag_season, 'Summer', 'Fall', 'Spring')) %>%
  filter(stage == 'J') %>% # juvenile obs only
  filter(total_rkm <= 695) # obs at mainstem hydrosystem only; eliminate IPTDS


# get first obs and plot
dat %>%
  group_by(tag_id) %>%
  slice(which.min(min_time)) %>%
  ggplot(aes(x = length, y = overwinter, colour = release_site)) +
  geom_point() +
  facet_wrap( ~ tag_season)


# calc proportions
mig_p <- dat %>%
  est_group_p(.summary_var = overwinter,
              alpha = 0.05,
              release_site,
              release_year,
              tag_season)

# plot proportions
mig_p %>%
  ggplot(aes(x = tag_season, y = p, colour = release_site))+
  geom_pointrange(aes(ymin = lwr, ymax = upr)) %>%
  facet_wrap(~release_year)
  
