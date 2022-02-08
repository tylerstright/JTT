#' @title create target catch table from prepared Rotary Screw Trapping Data
#' @description produces report table for unexpanded target catch by trap
#' @param data clean, filtered P4 RST dataset from get_RSTdata() %>% clean_RSTdata() %>% my_RSTdata()
#' @export
#' @import dplyr
#' @author Tyler T. Stright
#' @examples
#' p4_raw <- cdmsR::getP4data(MigrationYear = 2021)
#' p4_clean <- cuyem::clean_P4data(p4_raw)
#' tbl_targetCatch(p4_clean)

tbl_targetCatch <- function(data){
  data %>%
    rename(species = srrverbose) %>%
    filter(target == 1,
           !is.na(emigrant_group)) %>%
    group_by(streamname, emigrant_group) %>%
    summarize(n = round(sum(nfish, na.rm = TRUE), 0)) %>%
    spread(emigrant_group, n) %>%
    # mutate(`Hatchery Steelhead Juveniles` = if_else(streamname %in% c('Johnson Creek', 'Secesh River'), as.double(NA), `Hatchery Steelhead Juveniles`)) %>%
    select(`Trap Site` = streamname,
           `Natural Parr` = `Natural Chinook Salmon Parr`,
           `Natural Presmolts` = `Natural Chinook Salmon Presmolts`,
           `Natural Smolts` = `Natural Chinook Salmon Smolts`,
           `Hatchery Smolts` = `Hatchery Chinook Salmon Smolts`,
           `Natural Juveniles` = `Natural Steelhead Juveniles`,
           `Hatchery Juveniles` = `Hatchery Steelhead Juveniles`) %>%
    mutate(across(everything(), .fns= ~replace_na(., 'N/A'))) %>%
    flextable(cwidth = c(2, 1, 1, 1, 1, 1, 1)) %>%
    add_header_row(colwidths = c(1, 4, 2),
                   values = c('', 'Chinook Salmon', 'Steelhead')) %>%
    vline(j = c(1, 5)) %>%
    # merge_h(part = 'header') %>%
    align(j=c(2:7), align = 'center', part = 'header') %>%
    align(j=c(2:7), align = 'center', part = 'body') %>%
    font(fontname = 'Times New Roman', part = 'all')
}
