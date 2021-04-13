library(cuyem)
library(tidyverse)
library(lubridate)
library(viridis)

# load data - queried from DART
load(file='./data/arrival/dart_my20.rda')

# Dates for plots
transport_start <- ymd('2020-04-23') # begin barge collections
spring_spill <- ymd('2020-04-03') # (https://www.fpc.org/WebForm2013/includes/Metadatafor2020VoluntarySpillWeb.pdf)
summer_spill <- ymd('2020-06-21')

# Colors for S_CHN and S_STH plots
# spsu_sites <- dart_my20 %>%
#   filter(run != 'Fall') %>%
#   distinct(release_site_plotnames) %>%
#   pull(release_site_plotnames)
# 
# spsu_colors <- data.frame(
#   release_site_plotnames = sort(spsu_sites), 
#   plot_colors = viridis(n=length(spsu_sites))
# )
# 
# spsu_tmp <- spsu_colors %>%
#   select(acronym=release_site_plotnames, plot_colors)

LT_spsu <- read_csv('./data/arrival/spsu_colors_040821.csv') %>%
  select(acronym, plot_colors2)


# compare_spsu <- full_join(spsu_tmp, LT_spsu, by = 'acronym') 

# write_csv(compare_spsu, file = './data/arrival/spsu_colors_040821_TTS.csv')

# SFCLW 32W - nothin?

# write_csv(spsu_colors, file = './data/arrival/spsu_colors.csv')

# Colors for F_CHN plots
# fall_groups <- dart_my20 %>%
#   filter(run == 'Fall') %>%
#   distinct(release_groups) %>%
#   pull(unique(release_groups))
# 
# fall_colors <- data.frame(
#   release_groups = sort(fall_groups), 
#   plot_colors = viridis(n=length(fall_groups))
# )
# 
# fall_tmp <- fall_colors %>%
#   select(acronym = release_groups, plot_colors)

LT_fall <- read_csv('./data/arrival/fall_colors_040821.csv') %>%
  select(acronym, plot_colors2)

# compare_fall <- full_join(fall_tmp, LT_fall, by = 'acronym')

# write_csv(compare_fall, file = './data/arrival/fall_colors_040821_TTS.csv')

# write_csv(fall_colors, file = './data/arrival/fall_colors.csv')

# Steelhead ----
ggplot(dart_my20 %>% filter(species == 'Steelhead',
                            release_site_plotnames != 'SFCTRP'),   # JOHTRP Smolts - remove?
       aes(x=obs_date, color=release_site_plotnames, linetype=rear)) + 
  geom_vline(aes(xintercept = spring_spill, linetype = 'Spring spill'), show.legend=F) +
  geom_vline(aes(xintercept = transport_start, linetype ='Transport'), show.legend=F) +
  stat_ecdf(size=1) +
  facet_wrap(~lifestage, ncol=1) + #, strip.position='right') +
  scale_x_date(name='Date', labels = scales::date_format('%m/%d/%y'), date_breaks='month', expand = c(0, 0)) +  
  theme_bw() +  
  # scale_color_viridis_d(direction=-1) +
  # scale_color_manual(breaks= spsu_colors$release_site_plotnames,
  # values = spsu_colors$plot_colors) +
  scale_color_manual(breaks= LT_spsu$acronym,
                     values = LT_spsu$plot_colors2) +
  scale_linetype_manual(breaks = c('Natural', 'Hatchery', 'Spring spill', 'Transport'), 
                        # values= c(1,2,4,6)) +
                        values= c('solid','dashed','dotted',11)) +
  # theme(panel.grid.minor = element_blank()) +
  labs(y='Cumulative Proportion',
       color='Release Site', linetype='Line Types')+
  ggtitle('Summer Steelhead: arrival timing at LGR') +
  # LT ADDON
  theme(text=element_text(family="Times", size=12),
        strip.background = element_blank())

ggsave(filename = './data/arrival/sth_my20.png', width = 10, height = 7)

# Chinook ----
ggplot(dart_my20 %>% filter(species == 'Chinook salmon', run != 'Fall', 
                            !plot_group %in% c('Hatchery - Snake River','Hatchery - Clearwater River', 'Natural - Clearwater River'),
                            obs_date > '2020-03-01'),
       aes(x=obs_date, color=release_site_plotnames, linetype=rear)) + 
  geom_vline(aes(xintercept = spring_spill, linetype = 'Spring Spill'), show.legend=F) +
  geom_vline(aes(xintercept = transport_start, linetype ='Transport'), show.legend=F) +
  stat_ecdf(size=1) +
  facet_wrap(~lifestage, ncol=1) +
  scale_x_date(name = 'Date', labels = scales::date_format('%m/%d/%y'), date_breaks='month', expand = c(0, 0)) + 
  theme_bw() +  
  # scale_color_viridis_d(direction=-1) +
  # scale_color_manual(breaks= spsu_colors$release_site_plotnames,
  #                    values = spsu_colors$plot_colors) +
  scale_color_manual(breaks= LT_spsu$acronym,
                     values = LT_spsu$plot_colors2) +
  scale_linetype_manual(breaks = c('Natural', 'Hatchery', 'Spring Spill', 'Transport'), 
                        values= c('solid','dashed','dotted',11)) +
  # theme(panel.grid.minor = element_blank()) +
  labs(y = list(title='Cumulative Proportion'),
       color='Release Site', linetype='Line Types') +
  ggtitle('Spring/summer Chinook salmon: arrival timing at LGR') +
  # LT ADDON
  theme(text=element_text(family="Times", size=12),
        strip.background = element_blank())

ggsave(filename = './data/arrival/schn_my20.png', width = 10, height = 7)


# Fall Chinook ----
ggplot(dart_my20 %>% filter(species == 'Chinook salmon', run == 'Fall'),
       aes(x=obs_date, color=release_groups, linetype=rear)) +
  geom_vline(aes(xintercept = summer_spill, linetype = 'Summer Spill'), show.legend=F) +
  stat_ecdf(size=1) + 
  facet_wrap(~plot_group, ncol = 1) + 
  scale_x_date(name = 'Date', labels = scales::date_format('%m/%d/%y'), date_breaks='month') +
  theme_bw() +   
  # scale_color_viridis_d(direction=-1) +
  # scale_color_manual(breaks= fall_colors$release_groups,
  #                    values = fall_colors$plot_colors) +
  scale_color_manual(breaks= LT_fall$acronym,
                     values = LT_fall$plot_colors2) +
  scale_linetype_manual(breaks = c('Natural', 'Hatchery', 'Summer Spill'), 
                        values= c('solid','dashed','dotted')) +
  # theme(panel.grid.minor = element_blank()) +
  labs(y = list(title='Cumulative Proportion'),
       color='Release Site', linetype='Line Types') +
  ggtitle('Fall Chinook salmon: arrival timing at LGR') +
  # LT ADDON
  theme(text=element_text(family="Times", size=12),
        strip.background = element_blank())

ggsave(filename = './data/arrival/fchn_my20.png', width = 10, height = 7)
