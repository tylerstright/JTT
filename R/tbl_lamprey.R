#' @title create lamprey catch table from prepared Rotary Screw Trapping Data
#' @description produces report table for lamprey catch by trap
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

# we should pass this function data only from the MY in question. same data should be sent to all table/plot functions
# so that processing is only done once.
tbl_lamprey <- function(data) {
  rst_clean %>%
    filter(speciesrunreartype == 'A0W') %>%
    group_by(streamname, lifestage) %>%
    summarize(n = sum(nfish, na.rm = TRUE)) %>%
    spread(key = lifestage, value = n) %>%
    mutate(across(everything(), .fns= ~replace_na(., 0))) %>%
    select(`Trap Site` = streamname, Ammocoete, Macropthalmia, Adult) %>%
    flextable(cwidth = c(2, 1, 1, 1)) %>%
    add_header_row(colwidths = c(1, 3),
                   values = c('', 'Life stage')) %>%
    align(j=c(2:4), align = 'center', part = 'header') %>%
    align(j=c(2:4), align = 'center', part = 'body') %>%
    font(fontname = 'Times New Roman', part = 'all')
}
