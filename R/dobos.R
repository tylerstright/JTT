# ==============================================================================
library(tidyverse)
library(lubridate)
library(readxl)

# Steelhead Scale Ages ----
sth_ages <- read_excel('./data/dobos/NPT 32W Scale Age Data.xlsx') %>%
  filter(str_detect(Project, 'NPT'),
         LifeStage == 'Juvenile',
         !is.na(NumPIT),
         Species == 3,
         RearType == 'W',
         CaptureMethod == 'SCREWT') %>%
  select(CollectionDate, Project, StreamSection, ForkLengthmm, NumPIT, ScaleFinalAge)

sth_props <- sth_ages %>%
  filter(!is.na(ScaleFinalAge)) %>%
  mutate(year = year(CollectionDate)) %>%
  group_by(year, ScaleFinalAge, StreamSection) %>%
  summarize(n = n()) %>%
  group_by(year, StreamSection) %>%
  mutate(p = round(n/sum(n), 2)) %>%
  pivot_wider(id_cols = c('year', 'StreamSection'), names_from = ScaleFinalAge, values_from = p)

# Read in PTAGIS data and prep ----
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

dobos_tags <- left_join(sth_ages, data, by = c('NumPIT'='tag_code')) %>%
  filter(!is.na(ScaleFinalAge),
         !ScaleFinalAge %in% c('N:A', '5:0'),
         !is.na(site_first_name)) %>%
  mutate(DTH = first_obs_date-release_date) %>%
  filter(!grepl('Lolo', StreamSection)) # Trap Filter

# DOBOS PLOT ----
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

ggsave(filename = './images/dobos_tymy.png', height = 10, width = 15)

# No Ages
ggplot(data, aes(x=mark_length_mm, y = overwinters)) + 
  geom_jitter(height = 0.2, shape=21, aes(fill = trap_year), color = 'black') +
  # facet_grid(trap_season~site_name) +
  facet_grid(month_tagged~site_name) +
  scale_fill_viridis_d(direction = -1) +
  theme_bw() + xlab('Mark Length (mm)') + ylab('Migration Year')



