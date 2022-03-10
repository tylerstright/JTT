# Taken originally from lgr_arrival_plot.R (remove when ready)
#' @title gg_LGRarrival:
#'
#' @description plot arrival timing for Juvenile Report using PTAGIS data
#'
#' @author Tyler Stright
#'
#' @param data data queried from DART, prepared for this function.
#'
#' @param transport_start beginning date of juvenile transport via barge (YYYY-MM-DD).
#' Check PTAGIS.org site metadata at LGJ for date.
#' 
#' @param spring_spill beginning date of spring spill operations at Lower Granite Dam (YYYY-MM-DD).
#' Check FPC for spill dates.
#' 
#' @param summer_spill beginning date of summer spill operations at Lower Granite Dam (YYYY-MM-DD)
#' Check FPC for spill dates.
#' 
#' @param folder_path path to the desired folder for saving PNG plots
#' 
#' @import lubridate readr httr dplyr tidyr
#' @export
#' @return NULL
#' @examples gg_LGRarrival(data = ptagis_df)

gg_LGRarrival <- function(data,
                          transport_start,
                          spring_spill,
                          summer_spill, 
                          folder_path) {
  
  # TEST
  # data = arrival_my21
  # folder_path = './data/my21/arrival/'
  # transport_start = '2021-04-23'
  # spring_spill = '2021-04-03'
  # summer_spill = '2021-06-21'
  
  
  # packages
  library(cuyem)
  library(tidyverse)
  library(lubridate)
  library(viridis)
  
  # throw errors
  {if(is.null(data))stop("data must be supplied")}
  {if(is.null(transport_start) | !grepl('^\\d{4}-\\d{2}-\\d{2}$', transport_start))stop("transport_start must be provided as YYYY-MM-DD")}
  {if(is.null(spring_spill) | !grepl('^\\d{4}-\\d{2}-\\d{2}$', spring_spill))stop("spring_spill must be provided as YYYY-MM-DD")}
  {if(is.null(summer_spill) | !grepl('^\\d{4}-\\d{2}-\\d{2}$', summer_spill))stop("summer_spill must be provided as YYYY-MM-DD")}
  {if(is.null(data))stop("path to desired folder must be supplied")}
  
  
  # prep dates
  transport_start <- ymd(transport_start)
  # FPC Spill info: https://www.fpc.org/WebForm2013/includes/Metadatafor2020VoluntarySpillWeb.pdf
  spring_spill <- ymd(spring_spill) 
  summer_spill <- ymd(summer_spill)
  
  # colors ----
  plot_colors <- structure(list(
    acronym = c("BCCAP I", "BCCAP II", "CEFLAF", "CJRAP I", 
                "CJRAP II", "CLRWBS", "SFCBS", "LUGUAF", "NLVP", "NPTH", "PLAP I", 
                "PLAP II", "IMNHSC", "IMNTRP", "JOHNSC", "JOHTRP", "KNFH", "LOLOCE", 
                "LOLOCY", "LOLTRP", "LOSTIP", "LOSTIR", "LSHEEF", "MEAD2C", "MEADOC", 
                "NEWSAF", "NEWSOC", "NPTH", "REDHOS", "SECTRP", "SFCTRP"), 
    color = c("#B4DE2CFF", 
              "#FDE725FF", "#009292", "#ff6db6", "#ffb6db", "#006ddb", "#999999", 
              "#b66dff", "#6db6ff", "#490092", "#920000", "#b6dbff", "#000000", 
              "#006666", "#009292", "#ff6db6", "#ffb6db", "#006ddb", "#999999", 
              "#b66dff", "#6db6ff", NA, "#490092", "#920000", "#b6dbff", "#1D91C0", 
              "#924900", "#db6d00", "#D4B9DA", "#B4DE2CFF", "#FDE725FF")), 
    row.names = c(NA, -31L), 
    class = c("tbl_df", "tbl", "data.frame")) %>%
    arrange(acronym)
  
  
  # Steelhead Plot ----
  SSTH <- ggplot(data %>% filter(species == 'Steelhead'), 
                 aes(x=obs_date, color=release_group, linetype=rear)) + #release_site_plotnames
    geom_vline(aes(xintercept = spring_spill, linetype = 'Spring spill'), show.legend=F) +
    geom_vline(aes(xintercept = transport_start, linetype ='Transport'), show.legend=F) +
    stat_ecdf(size=1) +
    facet_wrap(~facet_var, ncol=1) +
    scale_x_date(name='Date', labels = scales::date_format('%m/%d/%y'), date_breaks='month', expand = c(0, 0)) +  
    theme_bw() +  
    scale_color_manual(breaks= plot_colors$acronym,
                       values = plot_colors$color) +
    scale_linetype_manual(breaks = c('Natural', 'Hatchery', 'Spring spill', 'Transport'), 
                          values= c('solid','dashed','dotted',11)) +
    scale_y_continuous(breaks = c(0.01, 0.1, 0.5, 0.9, 1.0), expand = expansion(mult = c(0,0.02))) +
    labs(y='Cumulative Proportion',
         color='Release Site', linetype='Line Types')+
    theme(text=element_text(family="serif", size=12), 
          strip.background = element_blank()) +
    guides(color = guide_legend(order=1),
           linetype = guide_legend(order=2))
  
  
  # Spring Chinook Plot ----
  SCHN <- ggplot(data %>% filter(species == 'Chinook salmon', run != 'Fall'),
                 aes(x=obs_date, color=release_group, linetype=rear)) + 
    geom_vline(aes(xintercept = spring_spill, linetype = 'Spring Spill'), show.legend=F) +
    geom_vline(aes(xintercept = transport_start, linetype ='Transport'), show.legend=F) +
    stat_ecdf(size=1) +
    facet_wrap(~facet_var, ncol=1) +
    scale_x_date(name = 'Date', labels = scales::date_format('%m/%d/%y'), date_breaks='month', expand = c(0, 0)) + 
    theme_bw() +  
    scale_color_manual(breaks= plot_colors$acronym,
                       values = plot_colors$color) +
    scale_linetype_manual(breaks = c('Natural', 'Hatchery', 'Spring Spill', 'Transport'), 
                          values= c('solid','dashed','dotted',11)) +
    scale_y_continuous(breaks = c(0.01, 0.1, 0.5, 0.9, 1.0), expand = expansion(mult = c(0,0.02))) +
    labs(y = list(title='Cumulative Proportion'),
         color='Release Site', linetype='Line Types') +
    theme(text=element_text(family="serif", size=12),  
          strip.background = element_blank())
  
  # Fall Chinook Plot ----
  FCHN <- ggplot(data %>% filter(species == 'Chinook salmon', run == 'Fall'),
                 aes(x=obs_date, color=release_group, linetype=rear)) +
    geom_vline(aes(xintercept = summer_spill, linetype = 'Summer Spill'), show.legend=F) +
    stat_ecdf(size=1) + 
    facet_wrap(~facet_var, ncol = 1) + 
    scale_x_date(name = 'Date', labels = scales::date_format('%m/%d/%y'), date_breaks='month') +
    theme_bw() +   
    scale_color_manual(breaks= plot_colors$acronym,
                       values = plot_colors$color) +
    scale_linetype_manual(breaks = c('Natural', 'Hatchery', 'Summer Spill'), 
                          values= c('solid','dashed','dotted')) +
    scale_y_continuous(breaks = c(0.01, 0.1, 0.5, 0.9, 1.0), expand = expansion(mult = c(0,0.02))) +
    labs(y = list(title='Cumulative Proportion')) + 
    guides(color = guide_legend(title = "Release Group", order = 1),
           linetype = guide_legend(title = "Line Types", order = 2)) +
    theme(text=element_text(family="serif", size=12),
          strip.background = element_blank())
  
  # save plots ----
  ggsave(SSTH, filename = paste0(folder_path, 'lgrarrival_ssth.png'), width = 10, height = 7)
  ggsave(SCHN, filename = paste0(folder_path, 'lgrarrival_schn.png'), width = 10, height = 7)
  ggsave(FCHN, filename = paste0(folder_path, 'lgrarrival_fchn.png'), width = 10, height = 7)
  
  return(list = list(SSTH, SCHN, FCHN))
  
}