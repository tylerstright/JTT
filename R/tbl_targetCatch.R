#' @title create target catch table from prepared Rotary Screw Trapping Data
#' @description produces report table for unexpanded target catch by trap
#' @param data clean, filtered P4 RST dataset from get_RSTdata() %>% clean_RSTdata() %>% my_RSTdata()
#' @export
#' @import dplyr
#' @author Tyler T. Stright
#' @examples
#' rst_raw <- get_RSTdata()
#' rst_clean <- clean_RSTdata(rst_raw)
#' rst_my <- my_RSTdata(imntrp = c('10/04/2019', '07/12/2020'),
#'     johtrp = c('05/23/2019', '06/14/2020'),
#'     loltrp = c('09/29/2019', '06/21/2020'),
#'     sectrp = c('06/24/2019', '11/11/2019'),
#'     sfctrp = c('09/26/2019', '06/21/2020'))

library(flextable)

# need to finish assigning streamnames in clean_RSTdata.R
tbl_targetCatch <- function(data){
  rst_clean %>%
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
    flextable(cwidth = c(2, 1, 1, 1, 1, 1, 1)) %>%
    add_header_row(colwidths = c(1, 4, 2),
                   values = c('', 'Chinook Salmon', 'Steelhead')) %>%
    vline(j = c(1, 5)) %>%
    # merge_h(part = 'header') %>%
    align(j=c(2:7), align = 'center', part = 'header') %>%
    align(j=c(2:7), align = 'center', part = 'body') %>%
    font(fontname = 'Times New Roman', part = 'all')
}
