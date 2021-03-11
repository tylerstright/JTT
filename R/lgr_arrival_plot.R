library(cuyem)
library(tidyverse)
library(lubridate)
library(fisheR)

# load data - queried from DART
load(file='./data/arrival/dart_my20.rda')

# Dates for plots
transport_start <- ymd('2020-04-23') # begin barge collections
spring_spill <- ymd('2020-04-03') # (https://www.fpc.org/WebForm2013/includes/Metadatafor2020VoluntarySpillWeb.pdf)
summer_spill <- ymd('2020-06-21')

# Colors for S_CHN and S_STH plots
spsu_sites <- dart_my20 %>%
  filter(run != 'Fall') %>%
  distinct(release_site) %>%
  pull(release_site)

spsu_colors <- data.frame(
  release_site = spsu_sites, 
  plot_colors = viridis(n=length(spsu_sites))
)

# Colors for F_CHN plots
fall_groups <- dart_my20 %>%
  filter(run == 'Fall') %>%
  distinct(release_groups) %>%
  pull(unique(release_groups))

fall_colors <- data.frame(
  release_groups = fall_groups, 
  plot_colors = viridis(n=length(fall_groups))
)


# Steelhead ----
ggplot(dart_my20 %>% filter(species == 'Steelhead'), 
       aes(x=obs_date, color=release_site)) + 
  geom_vline(aes(xintercept = spring_spill, linetype = 'Spring spill')) +
  geom_vline(aes(xintercept = transport_start, linetype ='Transport')) +
  stat_ecdf(size=1) +
  facet_wrap(~plot_group, ncol=1) + #, strip.position='right') +
  scale_x_date(name='Date', labels = scales::date_format('%m/%d/%y'), date_breaks='month') +  
  theme_bw() +  
  # scale_color_viridis_d(direction=-1) +
  scale_color_manual(breaks= spsu_colors$release_site,
                     values = spsu_colors$plot_colors) +
  theme(panel.grid.minor = element_blank()) +
  labs(y='Cumulative Proportion',
       color='Release Site', linetype='Operations')+
  ggtitle('Summer Steelhead: arrival timing at LGR')

ggsave(filename = './data/arrival/sth_my20.png', width = 8, height = 5)

# Chinook ----
ggplot(dart_my20 %>% filter(species == 'Chinook salmon', run != 'Fall', 
                            !plot_group %in% c('Hatchery - Snake River','Hatchery - Clearwater River', 'Natural - Clearwater River')),
        aes(x=obs_date, color=release_site)) + 
  geom_vline(aes(xintercept = spring_spill, linetype = 'Spring Spill')) +
  geom_vline(aes(xintercept = transport_start, linetype ='Transport')) +
  stat_ecdf(size=1) +
  facet_wrap(~plot_group, ncol=1) +
  scale_x_date(name = 'Date', labels = scales::date_format('%m/%d/%y'), date_breaks='month') + 
  theme_bw() +  
  # scale_color_viridis_d(direction=-1) +
  scale_color_manual(breaks= spsu_colors$release_site,
                     values = spsu_colors$plot_colors) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = list(title='Cumulative Proportion'),
       color='Release Site', linetype='Operations') +
  ggtitle('Spring/summer Chinook salmon: arrival timing at LGR')

ggsave(filename = './data/arrival/schn_my20.png', width = 8, height = 5)


# Fall Chinook ----
ggplot(dart_my20 %>% filter(species == 'Chinook salmon', run == 'Fall'),
       aes(x=obs_date, color=release_groups)) +
  geom_vline(aes(xintercept = summer_spill, linetype = 'Summer Spill')) +
  stat_ecdf(size=1) + 
  facet_wrap(~plot_group, ncol = 1) + 
  scale_x_date(name = 'Date', labels = scales::date_format('%m/%d/%y'), date_breaks='month') +
  theme_bw() +   
  # scale_color_viridis_d(direction=-1) +
  scale_color_manual(breaks= fall_colors$release_groups,
                     values = fall_colors$plot_colors) +
  theme(panel.grid.minor = element_blank()) +
  labs(y = list(title='Cumulative Proportion'),
       color='Release Site', linetype='Operations') +
  ggtitle('Fall Chinook salmon: arrival timing at LGR')

ggsave(filename = './data/arrival/fchn_my20.png', width = 8, height = 5)
