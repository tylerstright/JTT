#' @title tbl_missedTransport::
#'
#' @description Creates Table 9 in juvenile report: proportion arriving before transport.
#'
#' @author Tyler Stright
#'
#' @param data data queried from DART, prepared for this function.
#'
#' @param transport_start beginning date of juvenile transport via barge (YYYY-MM-DD).
#' Check PTAGIS.org site metadata at LGJ for date.
#' 
#' @import flextable
#' @export
#' @return NULL
#' @examples tbl_missedTransport(data = arrival_df, transport_start = '2020-04-23')

tbl_missedTransport <- function(data, transport_start, folder_path) {
  
  # throw errors
  {if(is.null(data))stop("data must be supplied")}
  {if(is.null(transport_start) | !grepl('^\\d{4}-\\d{2}-\\d{2}$', transport_start))stop("transport_start must be provided as YYYY-MM-DD")}
  
  # prep
  mt_prep <- data %>%
    filter(release_group %in% c(
      'IMNTRP', 'IMNHSC','LSHEEF', # Imnaha River
      'JOHTRP', 'JOHNSC', # Johnson Creek
      'SECTRP', # Secesh River
      'SFCTRP', 'REDHOS','MEAD2C', 'NEWSOC','NEWSAF', # South Fork Clearwater River
      'LOLTRP', 'LOLOCY', # Lolo Creek
      'LOSTIP', # Lostine River
      'MEADOC', # Meadow Creek : Selway River
      'KNFH', # KOOS / Clear Creek 
      'NPTH' # Clearwater River
    ),
    run != 'Fall') %>% # Ignore Fall Chinook
    mutate(stream = case_when(
      release_group %in% c('IMNTRP', 'IMNHSC','LSHEEF') ~ 'Imnaha River', 
      release_group %in% c('JOHTRP','JOHNSC') ~ 'Johnson Creek', 
      release_group %in% c('LOLTRP', 'LOLOCY') ~ 'Lolo Creek', 
      release_group %in% c('SECTRP') ~ 'Secesh River',
      release_group %in% c('SFCTRP', 'NEWSAF','REDHOS',
                           'MEAD2C','NEWSOC') ~ 'SF Clearwater River',
      # SF Broken out into groups, for peter.
      # release_group == 'REDHOS' ~ 'SF Clearwater - Red House Hole',
      # release_group == 'MEAD2C' ~ 'SF Clearwater - Meadow Creek',
      # release_group == 'SFCTRP' ~ 'South Fork Clearwater Trap',
      # release_group %in% c('NEWSAF', 'NEWSOC') ~ 'SF Clearwater - Newsome Creek',
      release_group == 'LOSTIP' ~ 'Lostine River',
      release_group == 'MEADOC' ~ 'Meadow Creek (Selway River)',
      release_group == 'KNFH' ~ 'Clear Creek',
      release_group == 'NPTH' ~ 'Clearwater River'
    )) %>%
    mutate(count = 1) %>%
    mutate(emigrant_group = paste(rear, gsub(' salmon', '', species), facet_var),
           emigrant_group = case_when(
             grepl('Winter/Spring and Hatchery', emigrant_group) & rear == 'Hatchery' ~ gsub('Winter/Spring and Hatchery', 'juveniles', emigrant_group),
             grepl('Winter/Spring and Hatchery', emigrant_group) & month(release_date) %in% c(1:6) ~ gsub('Winter/Spring and Hatchery', 'winter/spring juveniles', emigrant_group),
             grepl('Summer/Fall', emigrant_group) & month(release_date) %in% c(7:12) ~ gsub('Summer/Fall', 'summer/fall juveniles', emigrant_group),
             TRUE ~ emigrant_group
           ),
           emigrant_group = case_when(
             grepl('Parr', emigrant_group) ~ gsub('Parr', 'parr', emigrant_group),
             grepl('Presmolt', emigrant_group) ~ gsub('Presmolt', 'presmolts', emigrant_group),
             grepl('Smolt', emigrant_group) ~ gsub('Smolt', 'smolts', emigrant_group),
             TRUE ~ emigrant_group
           ),
           emigrant_group = if_else(grepl('Steelhead', emigrant_group), 
                                    gsub('Steelhead', 'steelhead', emigrant_group), 
                                    emigrant_group)) %>%
    group_by(stream, emigrant_group) %>%
    summarize(total = sum(count),
              no_transport = sum(count[obs_date < transport_start])) %>%
    mutate(p_missed = round(no_transport/total, 2))
  
  # spread and transpose
  mt_spread <- mt_prep %>%
    mutate(value = paste(format(p_missed, digits = 3), ' (', no_transport, ')', sep='')) %>%
    select(-total, -no_transport, -p_missed) %>%
    group_by(emigrant_group) %>%
    spread(key=stream, value = value) 
  
  mt_spread$emigrant_group <- factor(mt_spread$emigrant_group, 
                                     levels= c("Natural Chinook parr",
                                               "Hatchery Chinook parr",
                                               "Natural Chinook presmolts",
                                               "Hatchery Chinook presmolts",
                                               "Natural Chinook smolts",
                                               "Hatchery Chinook smolts",
                                               "Natural steelhead summer/fall juveniles",
                                               "Natural steelhead winter/spring juveniles",
                                               "Hatchery steelhead juveniles"))
  mt_spread <- mt_spread %>%
    arrange(emigrant_group) %>%
    rename(`Emigrant group`=emigrant_group)
  
  mt_transposed <- data.table::transpose(mt_spread, keep.names = 'Stream')
  
  names(mt_transposed) <- mt_transposed[1, ]
  
  mt_final <- mt_transposed[2:nrow(mt_transposed),]
  
  names(mt_final)[1] <- 'Stream'
  
  mt_final <- mt_final %>%
    replace(is.na(.), '-')
    
  # build flextable
  
  ft <- flextable(mt_final,
                  cwidth = c(1.5,1,1,1,1,1,1,1,1,1)) %>% # manually adjust to get correct fit.
    add_header_row(colwidths = c(1, 9),
                   values = c('', 'Emigrant Group')) %>%
    align(j=c(2:10), align='center', part= 'all') %>%
    font(fontname = 'Times New Roman', part = 'all') %>%
    height(part = 'body', height = 1.5) %>%
    border_inner_h(part='header') #%>%
  # fontsize(size = 12) %>%
  # theme_vanilla() %>% 
  # fit_to_width(max_width = 7)
  # ft <- vline(ft, j=c('Emigrant group'))
  
  save_as_docx(ft, path = paste0(folder_path, 'missedTransport.docx'))
  
  return(ft)
  
}
