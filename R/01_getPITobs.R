# Purpose: Download PIT-tag observations from DART
# Author: Ryan N. Kinzer
# 26 January 2021

# load packages
library(tidyverse)
library(fisheR)

# get data
release_sites <- c('JOHTRP', 'SECTRP', 'IMNTRP', 'LOLOC', 'CLWRSF', 'LAKEC','SECESR')
yrs <- 2000:2020

cross2(release_sites, yrs) %>% map(set_names, c('x', 'y'))

PIT_obs <- cross2(release_sites, yrs) %>%
  map(set_names, c('site', 'year')) %>%
  map_dfr(
       .f = ~{
         get_PITobs(
           query_type = 'release_site',
           release_site = .$site,
           species = 'Steelhead',
           run = 'All',
           start_date = paste0('01/01/',.$year),
           end_date = paste0('12/31/',.$year))
       }
  )

save(PIT_obs, file = './data/dart_pit_obs.rda')