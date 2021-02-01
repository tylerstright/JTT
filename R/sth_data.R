# Steelhead data to investigate migration behaviors
library(tidyverse)
library(lubridate)
library(cdmsR)
library(cuyem)
library(readxl)

# Get age data from CDMS ---- 
  # cdmsLogin(username='',password='') # don't save your credentials to github :)
  # age_cdms <- getDatasetView(80)
  # save(age_cdms, file = './data/dobos/age_cdms.rda')
load(file = './data/dobos/age_cdms.rda')

dupes <- age_cdms %>%  # find duplicates
  filter(Lifestage == 'Juvenile',
         !is.na(PITCode),
         PITCode != 'NA') %>%
  group_by(PITCode) %>% tally() %>% filter(n==2)

juv_cdms <- age_cdms %>%
  filter(Lifestage == 'Juvenile',
         !is.na(PITCode),
         PITCode != 'NA',
         !PITCode %in% c('NA', 'N:A', dupes$PITCode)) %>%  # remove duplicates (conflicting age estimates) and bad values
  select(NumPIT=PITCode, ScaleFinalAge=EuropeanAge) # modify to fit with age data provided by TH

# STH age data from TH
sth_ages <- read_excel('./data/dobos/NPT 32W Scale Age Data.xlsx') %>%
  mutate(trap_month = month(CollectionDate),
         trap_season = case_when(
           trap_month %in% c(1:6) ~ 'Spring',
           trap_month %in% c(7:8) ~ 'Summer',
           trap_month %in% c(9:12) ~ 'Fall'
         )) %>%
  filter(str_detect(Project, 'NPT'),
         LifeStage == 'Juvenile',
         !is.na(NumPIT),
         Species == 3,
         RearType == 'W',
         CaptureMethod == 'SCREWT',
         ScaleFinalAge != 'N:A') %>%
  select(NumPIT, ScaleFinalAge) %>%
  filter(!NumPIT %in% juv_cdms$NumPIT) # filter out tags present in cdms data

all_ages <- bind_rows(juv_cdms, sth_ages) # combine all age data

rm(age_cdms, dupes, juv_cdms, sth_ages) # remove clutter

# PTAGIS Data ----
ptagis <- read_csv('./data/dobos/Steelhead Dobos 2008-2020.csv')

names(ptagis) <- tolower(gsub(' ', '_', names(ptagis)))

ptagis <- ptagis %>%
  filter(mark_length_max < 300,
         rear_type_code == 'W') %>%
  mutate_at(.vars = c('mark_date', 'release_date', 'first_obs_date'), mdy) %>%
  separate(release_site_name, into = c('site', 'site_name'), sep = ' - ') %>%
  mutate(migratory_year = year(first_obs_date),
         trap_year = if_else(month(mark_date)>=7, year(mark_date)+1, year(mark_date)),
         month_tagged = month(mark_date, label=TRUE),
         # OVERWINTERS
         overwinters=migratory_year-trap_year+1,
         trap_season = case_when(
           month(mark_date) %in% c(1:6) ~ 'Spring',
           month(mark_date) %in% c(7:8) ~ 'Summer',
           month(mark_date) %in% c(9:12) ~ 'Fall')) %>%
  mutate(`trapyear+0` = if_else(trap_year==migratory_year, 1, 0),   # binary of when fish traveled
         `trapyear+1` = if_else(trap_year==migratory_year-1, 1, 0),
         `trapyear+2` = if_else(trap_year==migratory_year-2, 1, 0),
         `trapyear+3` = if_else(trap_year==migratory_year-3, 1, 0),
         `trapyear+4` = if_else(trap_year==migratory_year-4, 1, 0),
         `trapyear+5` = if_else(trap_year==migratory_year-5, 1, 0),
         julian = yday(mark_date)
  ) %>%
  mutate_at(.vars = c('migratory_year', 'trap_year', 'overwinters'), as.factor) %>%
  filter(overwinters != -1)  # only  one "-1" record

ptagis$trap_season <- factor(ptagis$trap_season, levels = c('Summer', 'Fall', 'Spring'))

# combine PTAGIS with Age Data ----
sth_data <- ptagis %>% 
  left_join(all_ages, by=c('tag_code'='NumPIT'))

rm(all_ages, ptagis)

save(sth_data, file = './data/dobos/sth_data.rda')

sth_data_ages <- sth_data %>%
  filter(!is.na(ScaleFinalAge),
         ScaleFinalAge != 'N:A')

save(sth_data_ages, file = './data/dobos/sth_data_ages.rda')
