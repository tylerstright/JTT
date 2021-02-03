# Purpose: Combine all observations across release sites and obs years.
# Author: Ryan N. Kinzer

#load packages
library(tidyverse)

# find files
folder <- './data/observations/'
files <- list.files(folder, pattern = ".rda")

# map across all files and combine, then format
allObs <- file.path(folder, files) %>%
  map_df(.f = function(x){
    load(x)
    obs %>% mutate(across(.cols = everything(), as.character))
  }) %>%
  mutate(release_date = lubridate::ymd(release_date),
         across(.cols = contains('time'), lubridate::ymd_hms),
         across(.cols = c(contains('year'), nfish, length, travel_days, total_rkm), as.integer)
         ) %>%
  mutate(migration_year = lubridate::year(obs_time),
         overwinter = as.factor(migration_year - release_year),
         tag_season = case_when(
           between(lubridate::month(release_date),1,6) ~ 'Spring',
           between(lubridate::month(release_date),7,8) ~ 'Summer',
           between(lubridate::month(release_date),9,12) ~ 'Fall'),
         tag_season = fct_relevel(tag_season, 'Summer', 'Fall', 'Spring')) 

# filter out only detections used for analysis
mod_dat <- allObs %>%
  filter(stage == 'J') %>% # juvenile obs only
  filter(total_rkm <= 695) %>% # obs at mainstem hydrosystem only; eliminate IPTDS
  filter(between(release_year, 2010, 2017)) %>% # filter to get release years with complete migration year potential
  group_by(tag_id) %>%
  slice(which.min(obs_time))

save(allObs, mod_dat, file = './data/allObs.rda')

rm(list = ls())
