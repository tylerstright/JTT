# Samples per trap year & season - to help indicate bias in sampling.
sampling_tally <- sth_data_ages %>%
  group_by(site_name, trap_year, trap_season) %>%
  tally() %>%
  spread(trap_season, n)

# Overwinter Proportions
overwinter_prop <- est_group_p(.data=sth_data, .summary_var=overwinters, alpha=0.05, site_name, trap_year)

ggplot(overwinter_prop, aes(x=trap_year, y=p, label = p)) +
  geom_bar(stat = 'identity', aes(fill=overwinters)) +
  # geom_text(size=3, position = position_stack(vjust=0.5)) +
  facet_wrap(~site_name) +
  scale_fill_viridis_d()

# Age Proportions
age_prop <- est_group_p(.data=sth_data_ages, .summary_var=ScaleFinalAge, alpha=0.05, site_name, trap_year)

ggplot(age_prop, aes(x=trap_year, y=p, label = p)) +
  geom_bar(stat = 'identity', aes(fill=ScaleFinalAge)) +
  # geom_text(size=3, position = position_stack(vjust=0.5)) +
  facet_wrap(~site_name) +
  scale_fill_viridis_d()


# Dobos-inspired plots
ggplot(sth_data, aes(x=mark_length_mm, y = overwinters)) + 
  geom_jitter(height = 0.2, shape=21, aes(fill = trap_year), color = 'black') +
  # facet_grid(trap_season~site_name) +
  facet_grid(month_tagged~site_name) +
  scale_fill_viridis_d(direction = -1) +
  theme_bw() + xlab('Mark Length (mm)') + ylab('Overwinters')


# Ty's modified dobos [data+plot]
tagyear_plus <- sth_data %>%
  pivot_longer(cols = c('trapyear+0', 'trapyear+1', 'trapyear+2', 'trapyear+3', 'trapyear+4', 'trapyear+5'), names_to = 'what_year',
               values_to = 'yes_no') %>%
  filter(yes_no == 1,
         !is.na(ScaleFinalAge),
         !ScaleFinalAge %in% c('N:A', '5:0'),
         !is.na(site_first_name))

ggplot(tagyear_plus, aes(x=julian, y = mark_length_mm)) +
  geom_jitter(height = 0.2, aes(color = ScaleFinalAge), size = 2) +
  # facet_wrap(~what_year) +
  facet_grid(what_year~trap_year) +
  # facet_grid(month_tagged~ScaleFinalAge) +
  scale_color_viridis_d(direction = -1) +
  theme_bw()

 # grouped into whether they migrated during tag year or in later years.
nowlaterp <- est_group_p(.data = tagyear_plus %>%
                           mutate(now_later = if_else(what_year=='trapyear+0', 'traveled', 'stayed')) %>%
                           # mutate(now_later = if_else(overwinters==1, 'traveled', 'stayed')) %>%
                           group_by(now_later, site_name, trap_year),
                         .summary_var = now_later, alpha = 0.05, site_name, trap_year)

ggplot(nowlaterp, aes(x=trap_year, y=p, label = p)) +
  geom_bar(stat = 'identity', aes(fill=now_later)) +
  # geom_text(size=3, position = position_stack(vjust=0.5)) +
  facet_wrap(~site_name) +
  scale_fill_viridis_d()
