# Purpose: Download PIT-tag observations from DART
# Author: Ryan N. Kinzer
# 26 January 2021

# load packages
library(tidyverse)
library(fisheR)

# get data
release_sites <- 'IMNTRP' #c('JOHTRP', 'SECTRP')#, 'IMNTRP', 'LOLOC', 'CLWRSF', 'LAKEC','SECESR')
yrs <- 2010:2020

# Creates a dataframe of obs. 

# PIT_obs <- cross2(release_sites, yrs) %>%
#   map(set_names, c('site', 'year')) %>%
#   map_dfr(
#        .f = ~{
#          get_PITobs(
#            query_type = 'release_site',
#            release_site = .$site,
#            species = 'Steelhead',
#            run = 'All',
#            start_date = paste0('01/01/',.$year), # obs date ranges
#            end_date = paste0('12/31/',.$year))
#        }
#   )
# 
# save(PIT_obs, file = './data/dart_pit_obs_19_20.rda')

# Create files for each combination

cross2(release_sites, yrs) %>%
  map(set_names, c('site', 'year')) %>%
  map(
    .f = function(.){
      obs <- get_PITobs(
        query_type = 'release_site',
        release_site = .$site,
        species = 'Steelhead',
        run = 'All',
        start_date = paste0('01/01/',.$year), # obs_time date ranges
        end_date = paste0('12/31/',.$year))
      
      save(obs, file = paste0('./data/observations/DARTobs_',.$site,'_',.$year,'.rda'))
    }
  )
