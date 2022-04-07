# 32H Holdovers - Appendix for Juvenile Report
library(tidyverse)
library(cuyem)
library(cdmsR)
library(lubridate)
library(flextable)
#  library(fisheR)
source('../fisher/R/get_PITobs.R') # updated to include LGS.


# Get Data ----
years <- 2009:year(Sys.Date())
traps <- c('IMNTRP', 'JOHTRP', 'LOLOC', 'SECTRP')

# GRJ Lower Granite Juvenile
# lgr <- years %>%
#   map_dfr(.f = ~{
#     get_PITobs(query_type = 'obs_site',
#                obs_site = 'GRJ',
#                release_site = NULL,
#                species = 'Steelhead',
#                run = 'All',
#                rear_type = 'Wild',
#                start_date = paste0('01/01/', .x),
#                end_date = paste0('12/31/', .x)) %>%
#       mutate(across(everything(), as.character))
#   })
# save(lgr, file = './data/holdovers/lgr.rda') # first 95182

# GOJ Little Goose Juvenile
# goj <- years %>%
#   map_dfr(.f = ~{
#     get_PITobs(query_type = 'obs_site',
#                obs_site = 'GOJ',
#                release_site = NULL,
#                species = 'Steelhead',
#                run = 'All',
#                rear_type = 'Wild',
#                start_date = paste0('01/01/', .x),
#                end_date = paste0('12/30/', .x)) %>%
#       mutate(across(everything(), as.character))
#   })
# save(goj, file = './data/holdovers/goj.rda')

load('./data/holdovers/lgr.rda')
load('./data/holdovers/goj.rda')


# data prep ----
df_clean <- bind_rows(lgr, goj) %>%
  # NPT Traps
  filter(mark_site %in% traps) %>%
  # fix datatypes
  separate(obs_time, into = c('obs_date', 'obs_time'), sep = ' ') %>%
  mutate(across(c('release_date', 'obs_date'), lubridate::ymd),
         across(c('nfish', 'length', 'total_rkm', 'location_days', 'release_year'), as.integer),
         tag_my = if_else(month(release_date) %in% 1:6, year(release_date), year(release_date)+1),
         obs_my = if_else(month(obs_date) %in% 1:6, year(obs_date), year(obs_date)+1),
         stream = case_when(
           release_site == 'IMNTRP' ~ 'Imnaha River',
           release_site == 'JOHTRP' ~ 'Johnson Creek',
           release_site == 'LOLOC' ~ 'Lolo Creek',
           release_site == 'SECTRP' ~ 'Secesh River'
         )) %>%
  # arrange and remove duplicate records
  arrange(tag_id, obs_date) %>%
  distinct(tag_id, .keep_all = TRUE) %>%
  select(stream, tag_id, length, release_site, release_date, obs_site, obs_date, tag_my, obs_my) %>%
  mutate(holdover = if_else(obs_my > tag_my, 'Holdover', 'Same Year'),
         count = 1) %>%
  # Peter's figures say "juvenile steelhead tagged from January 1 through June 30" 
  filter(month(release_date) %in% 1:6)

# calculate holdover #
p_holdovers <- df_clean %>%
  group_by(release_site, tag_my) %>%
  summarize(`Total detected` = sum(count),
            holdies = sum(count[holdover=='Holdover'])) %>%
  mutate(`Percent delayed emigrants` = round((holdies/`Total detected`)*100, 1)) %>%
  select(-holdies)

my_filter <- c(2009, 2021) # determines tag years for Y axis

# complete summary
yearly_sum <- df_clean %>%
  group_by(release_site, tag_my, obs_my) %>%
  summarize(n = n()) %>%
  group_by(release_site, tag_my)  %>%
  spread(key = obs_my, value = n) %>%
  ungroup() %>%
  filter(between(tag_my, my_filter[1], my_filter[2])) %>%
  left_join(p_holdovers, by = c("release_site", "tag_my")) %>%
  mutate(`Percent delayed emigrants` = paste0(`Percent delayed emigrants`, '%'))

# build tables ----
tbl_holdover <- function(data) {
  flextable(data,
            cwidth = c(1.5, rep(0.75, (length(names(data))-3), 2, 2))) %>%
    add_header_row(colwidths = c(1, length(names(data))-1),
                   values = c('', 'Number detected per migration year')) %>%
    colformat_num(j = 1, big.mark = '', decimal.mark = ) %>%
    align(j=c(1:ncol(data)), align='center', part= 'all') %>%
    font(fontname = 'Times New Roman', part = 'all') %>%
    height(part = 'body', height = 1.5) %>%
    border_inner_h(part='header')
}

for(i in 1:length(traps)) {
  tmp_tbl <- tbl_holdover(yearly_sum %>%
                            filter(release_site == traps[i]) %>%
                            select(-release_site) %>%
                            rename(`MY Tagged` = tag_my))
  
  assign(traps[i], tmp_tbl)
}

# Fig - fork length histogram ----
length_sum <- df_clean %>%
  group_by(stream, holdover) %>%
  summarize(mean_length = mean(length, na.rm=TRUE),
            sd = sd(length, na.rm = TRUE))

ggplot(data = length_sum, mapping =  aes(x= stream, y=mean_length, fill = holdover)) +
  geom_bar(stat='identity', position = 'dodge', width = 0.6, color = 'black', size=0.6) +
  geom_errorbar(aes(ymin=mean_length-sd, ymax=mean_length+sd), position = position_dodge(0.6), width = 0.25, size = 0.8) +
  theme_bw() +
  scale_fill_grey(start = 0.5, end = 0.95) +
  # scale_x_discrete(expand = c(0.05,0.05)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200), expand = c(0,0), limits = c(0,200)) +
  ylab('Mean Length (mm)') +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.title.x = element_blank())

# Age Composition ----
# age <- get_DatasetView(80)
# save(age, file = './data/age.rda')
load('./data/age.rda')

age_comp <- age %>%
  filter(Lifestage=='Juvenile',
         Species == 'Steelhead') %>%
  select(PITCode, StreamAge) %>%
  right_join(df_clean, by = c('PITCode' = 'tag_id')) %>%
  filter(!StreamAge %in% c(NA, -99)) %>%
  group_by(stream, StreamAge, holdover) %>%
  summarize(count = sum(count, na.rm=TRUE)) %>%
  group_by(stream, holdover) %>%
  spread(key=StreamAge, value = count)

age_comp$n <- rowSums(age_comp[,3:7], na.rm = TRUE)

age_p <- age_comp %>%
  mutate(`Age 1` = if_else(!is.na(`1`), paste0(round((`1`/n)*100, 1), '%'), '0.0%'),
         `Age 2` = if_else(!is.na(`2`), paste0(round((`2`/n)*100, 1), '%'), '0.0%'),
         `Age 3` = if_else(!is.na(`3`), paste0(round((`3`/n)*100, 1), '%'), '0.0%'),
         `Age 4` = if_else(!is.na(`4`), paste0(round((`4`/n)*100, 1), '%'), '0.0%'),
         `Age 5` = if_else(!is.na(`5`), paste0(round((`5`/n)*100, 1), '%'), '0.0%')) %>%
  select(-`1`,-`2`,-`3`,-`4`,-`5`) %>%
  arrange(holdover) %>%
  # this combines them into same cell, if that's what you want.
     # shows "HOLDOVER (SAME YEAR)"
  group_by(stream) %>%
  mutate(n = paste0(paste(n, collapse = ' ('), ')'),
         `Age 1` = paste0(paste(`Age 1`, collapse = ' ('), ')'), # seems like I could use lapply or sapply?
         `Age 2` = paste0(paste(`Age 2`, collapse = ' ('), ')'),
         `Age 3` = paste0(paste(`Age 3`, collapse = ' ('), ')'),
         `Age 4` = paste0(paste(`Age 4`, collapse = ' ('), ')'),
         `Age 5` = paste0(paste(`Age 5`, collapse = ' ('), ')')) %>%
  ungroup() %>%
  distinct(stream, .keep_all=TRUE) %>%
  rename(`Trap site` = stream) %>%
  select(-holdover)

flextable(age_p,
          cwidth = c(1.5, 1, rep(2, ncol(age_p)-2))) %>%
  align(j=c(2:ncol(age_p)), align='center', part= 'all') #%>%
  font(fontname = 'Times New Roman', part = 'all') %>%
  height(part = 'body', height = 1.5) %>%
  border_inner_h(part='header')

