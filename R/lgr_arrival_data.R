library(tidyverse)
library(fisheR)
library(lubridate)

# lists
migration_year <- '2020'
lgr_sites <- c('GRJ', 'GRA', 'GRS')  #(for obs_site filter: juv, adult, spill)
npth_chinook_sites <- c('BCCAP', 'CEFLAF', 'CJRAP', 'NLVP', 'LUGUAF', 'NPTH', 'PLAP', 'KOOS', 'MEADOC', 'NEWSOC')
steelhead_sites <- c('IMNTRP','LSHEEF', 'JOHTRP', 'SECTRP', 'LOLOC', 'CLWRSF', 'NEWSOC','MEAD2C')
chinook_sites <- c('IMNTRP', 'IMNAHR', 'IMNAHW', 'JOHTRP', 'JOHNSC', 'SECTRP', 'LOLOC', 'CLWRSF',
                   'LOSTIP', 'LOSTIR', 'CLWR', npth_chinook_sites)
fchn_clearwater <- c('CLWR','NLVP','NPTH','CEFLAF','BCCAP','LUGUAF')
fchn_snake <- c('PLAP','CJRAP')

# MY 2020 start and end dates (filters) ----
# Beach Seining
sein_start <- mdy('06/16/2020')
sein_end <- mdy('08/13/2020')
# RST operation dates
johtrp_start <- mdy('05/23/2019')
johtrp_end <- mdy('06/14/2020')

imntrp_start <- mdy('10/04/2019')
imntrp_end <- mdy('07/12/2020')

sectrp_start <- mdy('06/24/2019')
sectrp_end <- mdy('11/11/2019') # no spring trapping

loloc_start <- mdy('09/29/2019')
loloc_end <- mdy('06/21/2020')

clwrsf_start <- mdy('09/26/2019')
clwrsf_end <- mdy('06/21/2020')


# DART data retrieval ----
# sth_dart <- steelhead_sites %>%
#   map_dfr(.f = ~{
#     get_PITobs(
#       query_type = 'release_site',
#       release_site = .x,
#       species = 'Steelhead',
#       run = 'All',
#       start_date = paste0('01/01/', migration_year),
#       end_date = paste0('12/31/', migration_year))  %>%
#       mutate(across(everything(), as.character))
#   })
# save(sth_dart, file = './data/arrival/sth_dart.rda')

# chn_dart <- chinook_sites %>%
#   map_dfr(.f = ~{
#     get_PITobs(
#       query_type = 'release_site',
#       release_site = .x,
#       species = 'Chinook',
#       run = 'All',
#       start_date = '01/01/2020',
#       end_date = '12/31/2020')  %>%
#       mutate(across(everything(), as.character))
#   })
# save(chn_dart, file = './data/arrival/chn_dart.rda')

load('./data/arrival/sth_dart.rda')
load('./data/arrival/chn_dart.rda')

# combine and process data ----
dart_my20 <- bind_rows(sth_dart, chn_dart) %>%
  separate(obs_time, into=c('obs_date', 'obs_time'), sep = ' ') %>%
  mutate(across(c(release_date, obs_date), ymd)) %>%
  mutate(tag_coord = str_extract(tag_file, '^[A-Z]{3}')) %>%
  mutate(run = if_else(sprrt == '15W', 'Fall', run)) %>%  # include 15W as Fall Chinook
  mutate(RST_stream = case_when(
    release_site == 'CLWRSF' & sprrt %in% c('32H','32W','11W') ~ TRUE, # exclude beach sein
    release_site %in% c('IMNAHR','IMNAHW','IMNTRP','JOHNSC','JOHTRP',
                        'LOLOC','LOSTIP','LOSTIR','LSHEEF','MEAD2C','NEWSOC', 
                        'SECTRP') ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  # Assign Release Groups
  mutate(release_groups = case_when(
    # Beach Seining
    release_site == 'CLWR' & tag_coord == 'BDA' & between(release_date, sein_start, sein_end) ~ 'CLRWBS', #13W Beach Sein - NPT', # BA as tag coordinator. Fall Chinook.
    release_site == 'CLWR' & tag_coord == 'WPC' & release_year == 2020 ~ 'CLRWBS', #'13W Beach Sein - USGS', #& release_date == ymd('2020-07-06')  # USGS contribution (worked with NPT on 7/6/20)
    # release_site == 'CLWRSF' & tag_coord == 'BDA' & tag_file %in% c(paste0('BDA-2020-', c(191,195,196,198),'-SF1.XML'),
    #                                                                 paste0('BDA-2020-', c(195,196,198),'-SF2.XML')) ~ 'SFCWBS', # ONLY ONE RECORD!***********
        # & between(release_date, mdy('07-09-2020'), mdy('07-16-2020')) ~ 'SF Clearwater Beach Sein',# BDA = Billy Arnsberg (SF Beach Seining)
    
    # RST
    release_site == 'IMNTRP' & between(release_date, imntrp_start, imntrp_end) ~ 'IMNTRP', 
    release_site == 'JOHTRP' & between(release_date, johtrp_start, johtrp_end) ~ 'JOHTRP', 
    release_site == 'SECTRP' & between(release_date, sectrp_start, sectrp_end) ~ 'SECTRP',
    release_site == 'CLWRSF' & tag_coord == 'NPC' & between(release_date, clwrsf_start, clwrsf_end) ~ 'SFCTRP', # NPC = Nez Perce Clearwater (SF RST)
    release_site == 'LOLOC' & tag_file != 'SCS-2019-211-LC1.XML' & between(release_date, loloc_start, loloc_end) & sprrt !='32H'~ 'LOLTRP', # LC1.XML=11H release
    release_site == 'LOSTIR' & between(release_date, mdy('07/01/2019'), mdy('06/30/2020')) ~ 'Lostine Naturals',
    # FALL CHINOOK
    release_site == 'BCCAP' & tag_file == 'SCS-2020-107-BC1.XML' ~ 'BCCAP I', # 13H
    release_site == 'BCCAP' & tag_file == 'SCS-2020-126-BC2.XML' ~ 'BCCAP II', # 13H
    release_site == 'CEFLAF' & tag_file == 'SCS-2020-133-CF1.XML' ~ 'CEFLAF', # 13H
    release_site == 'CJRAP' & tag_file == 'SCS-2020-113-CJ1.XML' ~ "CJRAP I", # 13H
    release_site == 'CJRAP' & tag_file == 'SCS-2020-127-CJ2.XML' ~ "CJRAP II", # 13H
    release_site == 'LUGUAF' & tag_file == 'SCS-2020-132-LG1.XML' ~ "LUGUAF", # 13H
    release_site == 'NLVP' & tag_file == 'SCS-2020-114-NLV.XML' ~ 'NLVP', # 13H
    release_site == 'NPTH' & tag_file == 'SCS-2020-135-OS1.XML' ~ 'NPTH',  # 13H
    release_site == 'PLAP' & tag_file == 'SCS-2020-106-PL1.XML' ~ 'PLAP I', # 13H
    release_site == 'PLAP' & tag_file == 'SCS-2020-125-PL2.XML' ~ 'PLAP II', # 13H
    # SPRING CHINOOK
    release_site == 'KOOS' & tag_file == 'SCS-2019-282-002.XML' ~ 'KNFH', # 11H
    release_site == 'LOLOC' & tag_file == 'SCS-2019-211-LC1.XML' ~ 'LOLOCY', # 11H Yoosa creek release
    release_site == 'MEADOC' & tag_file == 'SCS-2019-169-MC1.XML' ~ 'MEADOC', # MF Salmon / Meadow Creek
    # release_site == 'NEWSOC' & tag_file == 'SCS-2019-211-NC1.XML' ~ 'NEWSOC',  # 11H 
    release_site == 'NEWSOC' & sprrt=='11H' & tag_file == "SCS-2019-211-NC1.XML" ~ 'NEWSAF',  # 11H   ****************************************6/7/21
    release_site == 'NPTH' & tag_file %in% c('SCS-2020-070-NP1.XML','SCS-2020-070-NP2.XML') ~ 'NPTH', # 11H
    release_site %in% c('IMNAHR','IMNAHW') ~ 'IMNHSC', # 11H  
    release_site == 'LOSTIP' ~ 'LOSTIP', # 11H
    release_site == 'JOHNSC' ~ 'JOHNSC', # 12H
    # SUMMER STEELHEAD
    release_site == 'CLWRSF' & tag_file %in% c('CBB-2020-008-006.XML','CBB-2020-008-007.XML', 
                                               paste0('BDL-2019-275-W', c(15,16,25,26,35,36,45,46), '.XML')) ~ 'REDHOS', # 32H Red House Hole (SFC)',
    release_site == 'MEAD2C' & tag_file %in% c(paste0('BDL-2019-275-W', c(11:14,21:24,31:34,41:44),'.XML')) ~ 'MEAD2C', # 32H Meadow Creek (SFC)', # SF Clearwater / Meadow Creek
    release_site == 'NEWSOC' & tag_file %in% c(paste0('BDL-2019-276-W', c(11,12,21,22,31,32,41,42),'.XML')) ~ 'NEWSOC', #'32H Newsome Creek (SFC)',
    release_site == 'LSHEEF' & year(release_date) == 2020 ~ 'LSHEEF', # 32H
    release_site == 'LOLOC' & sprrt == '32H' ~ 'LOLOCE', # at Eldorado
    TRUE ~ 'Unassigned')) %>%
  # Groups for plots
  mutate(plot_group = case_when(
    # Fall Chinook
    run == 'Fall' & release_site %in% fchn_clearwater ~ 'Clearwater River',
    run == 'Fall' & release_site %in% fchn_snake ~ 'Snake River',
    # Hatchery (non-fall)
    rear == 'Hatchery' ~ 'Hatchery',
    # Spring/summer Chinook
    species == 'Chinook salmon' & month(release_date) %in% c(1:6) ~ 'Smolt',
    species == 'Chinook salmon' & month(release_date) %in% c(7:8) ~ 'Parr',
    species == 'Chinook salmon' & month(release_date) %in% c(9:12) ~ 'Presmolt',  
    # Summer Steelhead
    species == 'Steelhead' & month(release_date) %in% c(1:6) ~ 'Winter/Spring',
    species == 'Steelhead' & month(release_date) %in% c(7:12) ~ 'Summer/Fall'
  )) %>%
  mutate(release_site_plotnames = case_when(
    release_site == 'IMNTRP' ~ 'IMNTRP', #'Imnaha River RST',
    release_site == 'LSHEEF' ~ 'LSHEEF', #'Little Sheep Acclimation Facility',
    release_site == 'JOHTRP' ~ 'JOHTRP', #'Johnson Creek RST',
    release_site == 'JOHNSC' ~ 'JOHNSC', #'Johnson Creek',
    release_site == 'SECTRP' ~ 'SECTRP', #'Secesh River RST',
    release_groups == 'LOLTRP' ~ release_groups, 
    release_groups == '11H Lolo Creek' ~ 'LOLOCE', #'Lolo Creek at Eldorado Creek Mouth',
    release_groups == 'SFCTRP' ~ 'SFCTRP', #'South Fork Clearwater RST',
    release_groups == 'NEWSOC' ~ 'NEWSOC', #'Newsome Creek',
    release_groups == 'NEWSAF' ~ 'NEWSAF',
    release_site == 'MEAD2C' ~ 'MEAD2C', #'Meadow Creek (SF Clearwater)',
    release_site %in% c('IMNAHW', 'IMNAHR') ~ 'IMNHSC',
    # release_site %in% c('LOSTIR', 'LOSTIP') ~ 'Lostine River', # these are split by the ggplot facet_wrap into hatchery and natural
    release_site == 'LOSTIR' ~ 'LOSTIR',
    release_site == 'LOSTIP' ~ 'LOSTIP',
    release_site == 'NPTH' ~ 'NPTH',
    release_site == 'KOOS' ~ 'KNFH',
    release_site == 'MEADOC' ~ 'MEADOC',
    release_groups == 'LOLOCY' ~ 'LOLOCY',
    release_groups == 'LOLOCE' ~ 'LOLOCE',
    release_groups == 'REDHOS' ~ 'REDHOS',
    TRUE ~ paste0('WHAA_', release_site))
  ) %>%
  # ADDING LIFESTAGE - for facet_wrap STH and SCHN (not fall, which is stream)
  mutate(lifestage = case_when(
    # Hatchery (non-fall)
    rear == 'Hatchery' & species == 'Steelhead' ~ 'Winter/Spring and Hatchery',
    rear == 'Hatchery' & release_groups %in% c('KNFH', 'NPTH', 'IMNHSC', 'LSHEEF', 'JOHNSC', 'LOLOCE', 'LOSTIP', 
                                               'MEAD2C', 'NEWSOC', 'REDHOS') ~ 'Smolt',
    rear == 'Hatchery' & release_groups %in% c('MEADOC') ~ 'Parr',
    rear == 'Hatchery' & release_groups %in% c('LOLOCY', 'NEWSAF') ~ 'Presmolt',
    rear == 'Hatchery' & run == 'Fall' ~ 'Subyearling',
    # Spring/summer Chinook
    species == 'Chinook salmon' & rear == 'Natural' & month(release_date) %in% c(1:6) ~ 'Smolt',
    species == 'Chinook salmon' & rear == 'Natural' & month(release_date) %in% c(7:8) ~ 'Parr',
    species == 'Chinook salmon' & rear == 'Natural' & month(release_date) %in% c(9:12) ~ 'Presmolt',  
    # Summer Steelhead
    species == 'Steelhead' & rear == 'Natural' & month(release_date) %in% c(1:6) ~ 'Winter/Spring and Hatchery',
    species == 'Steelhead' & rear == 'Natural' & month(release_date) %in% c(7:12) ~ 'Summer/Fall'
  )) %>%
  # FILTERS
  filter(obs_site %in% lgr_sites, # only LGR observation sites
         stage == 'J') %>% # only Juvenile records
  filter(case_when(
    release_site %in% c('IMNTRP','JOHTRP','SECTRP') ~ sprrt %in% c('12W', '32W'),
    release_groups == 'LOLTRP' ~ sprrt %in% c('11W','32W'),
    release_site %in% c('IMNAHW','IMNAHR') ~ sprrt == '11H', # this removes the ODFW ELH 11W's from IMNAHR
    TRUE ~ sprrt == sprrt
  )) %>%
  filter(release_groups != 'Unassigned',
         release_site != 'LOSTIR')

dart_my20$plot_group <- factor(dart_my20$plot_group, levels= c('Parr','Summer/Fall','Presmolt','Smolt',"Winter/Spring and Hatchery",
                                                               'Hatchery', 'Clearwater River','Snake River'))

dart_my20$lifestage <- factor(dart_my20$lifestage, levels= c("Parr", "Summer/Fall","Presmolt","Smolt", 
                                                             "Winter/Spring and Hatchery", "Subyearling"))

save(dart_my20, file='./data/arrival/dart_my20.rda')
# load(file='./data/arrival/dart_my20.rda')

rm(list = ls())
