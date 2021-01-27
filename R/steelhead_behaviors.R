# Steelhead Behaviors
library(tidyverse)
library(lubridate)
library(cdmsR)
library(cuyem)
library(readxl)

# Age Data ---- 
# CDMS
# age_cdms <- getDatasetView(80)
# save(age_cdms, file = './data/dobos/age_cdms.rda')
load(file = './data/dobos/age_cdms.rda')

dupes <- age_cdms %>%
  filter(Lifestage == 'Juvenile',
         !is.na(PITCode),
         PITCode != 'NA') %>%
  group_by(PITCode) %>% tally() %>% filter(n==2)

juv_cdms <- age_cdms %>%
  filter(Lifestage == 'Juvenile',
         !is.na(PITCode),
         PITCode != 'NA',
         !PITCode %in% dupes$PITCode) %>%
  # select(PITCode, StreamAge, OceanAge, RepeatSpawner, TotalAge, EuropeanAge, AgeingComment)
  select(NumPIT=PITCode, ScaleFinalAge=EuropeanAge) %>%
  filter(NumPIT != 'N:A')

# STH data from TH
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
  filter(!NumPIT %in% juv_cdms$NumPIT)
# select(CollectionDate, Project, StreamSection, ForkLengthmm, NumPIT, ScaleFinalAge)

all_ages <- bind_rows(juv_cdms, sth_ages)

# PTAGIS Data ----
dobos <- read_csv('./data/dobos/Steelhead Dobos 2008-2020.csv') # PTAGIS query data

names(dobos) <- tolower(gsub(' ', '_', names(dobos)))

data <- dobos %>%
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

data$trap_season <- factor(data$trap_season, levels = c('Summer', 'Fall', 'Spring'))

ggplot(data, aes(x=mark_length_mm, y = overwinters)) + 
  geom_jitter(height = 0.2, shape=21, aes(fill = trap_year), color = 'black') +
  # facet_grid(trap_season~site_name) +
  facet_grid(month_tagged~site_name) +
  scale_fill_viridis_d(direction = -1) +
  theme_bw() + xlab('Mark Length (mm)') + ylab('Migration Year')

# PTAGIS combined with Age Data ----
base_data <- data %>% 
  left_join(all_ages, by=c('tag_code'='NumPIT'))


dobos_tags <- left_join(sth_ages, data, by = c('NumPIT'='tag_code')) %>%
  filter(!is.na(ScaleFinalAge),
         !ScaleFinalAge %in% c('N:A', '5:0'),
         !is.na(site_first_name)) %>%
  mutate(DTH = first_obs_date-release_date) %>%
  filter(!grepl('Lolo', StreamSection)) # Trap Filter

# ptagis_withage <- data %>%
#   left_join(sth_ages, by= c('tag_code'='NumPIT')) %>%
#   left_join(juv_cdms, by= c('tag_code'='PITCode'))

# DOBOS PLOT
ggplot(dobos_tags, aes(x=mark_length_mm, y = DTH)) + #color?
  geom_point(shape = 21, aes(fill = overwinters), color = 'black') +
  facet_grid(trap_season~ScaleFinalAge) +
  scale_fill_viridis_d(direction = -1) +
  scale_y_continuous() +
  theme_bw()

# length histo
ggplot(dobos_tags, aes(x=mark_length_mm)) +
  geom_histogram(aes(x=mark_length_mm, fill=ScaleFinalAge), position='dodge', bins = 50) +
  scale_fill_viridis_d(direction=-1) +
  facet_wrap(~site_name) +
  theme_bw() +
  theme(plot.background = element_rect(fill='black'),
        panel.background = element_rect(fill='grey'))

# Proportion Calculations ----
season_props <- est_group_p(sth_ages, ScaleFinalAge, 0.5, trap_season, StreamSection)

ggplot(season_props, aes(x=trap_season, y=p))+
  geom_bar(stat = 'identity', aes(fill=ScaleFinalAge)) +
  facet_wrap(~StreamSection) +
  scale_fill_viridis_d()

ggplot(season_props, aes(x=StreamSection, y=p))+
  geom_bar(stat = 'identity', aes(fill=ScaleFinalAge)) +
  facet_wrap(~trap_season) +
  scale_fill_viridis_d()


# proportions of fish by overwinter count and trap
overwinter_props <- est_group_p(data, overwinters, 0.5, trap_year, site_name)
# overwinter_props <- est_group_p(data, overwinters, 0.5, trap_season, site_name)

ggplot(overwinter_props, aes(x=trap_year, y=p))+
  geom_bar(stat = 'identity', aes(fill=overwinters)) +
  facet_wrap(~site_name) +
  scale_fill_viridis_d()

ages_with <- left_join(sth_ages, data, by = c('NumPIT'='tag_code')) %>%
  filter(!is.na(ScaleFinalAge),
         !ScaleFinalAge %in% c('N:A', '5:0'),
         !is.na(site_first_name)) %>%
  mutate(DTH = first_obs_date-release_date) %>%
  filter(!grepl('Lolo', StreamSection)) # Trap Filter

# DOBOS - Ty's method that changes Y axis to "MY" in relation to "TY" (trap year) ----
dobos_tags_tymy <- data %>%
  pivot_longer(cols = c('trapyear+0', 'trapyear+1', 'trapyear+2', 'trapyear+3', 'trapyear+4', 'trapyear+5'), names_to = 'what_year',
               values_to = 'yes_no') %>%
  filter(yes_no == 1) %>%
  left_join(sth_ages, by = c('tag_code'='NumPIT')) %>%
  filter(!is.na(ScaleFinalAge),
         !ScaleFinalAge %in% c('N:A', '5:0'),
         !is.na(site_first_name)) %>%
  mutate(DTH = first_obs_date-release_date) %>%  # days to hydrosystem detection
  filter(!grepl('Lolo', StreamSection))


# ggplot(dobos_tags_tymy, aes(x=mark_length_mm, y = what_year)) +
#   geom_jitter(height = 0.2, shape = 21, aes(fill = trap_year), color = 'black') +
#   facet_grid(trap_season~ScaleFinalAge) +
#   # facet_grid(month_tagged~ScaleFinalAge) +
#   scale_fill_viridis_d(direction = -1) +
#   theme_bw()

ggplot(dobos_tags_tymy, aes(x=julian, y = mark_length_mm)) + 
  geom_jitter(height = 0.2, aes(color = ScaleFinalAge), size = 2) +
  # facet_wrap(~what_year) +
  facet_grid(what_year~trap_year) +
  # facet_grid(month_tagged~ScaleFinalAge) +  
  scale_color_viridis_d(direction = -1) +
  theme_bw() 
