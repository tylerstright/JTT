#' @title create lamprey catch table from prepared Rotary Screw Trapping Data
#' @description produces report table for lamprey catch by trap
#' @param data clean, filtered P4 RST dataset from get_RSTdata() %>% clean_RSTdata() %>% my_RSTdata()
#' @export
#' @import dplyr
#' @author Tyler T. Stright
#' @examples
#' p4_raw <- cdmsR::getP4data(MigrationYear = 2021, CaptureMethod = 'SCREWT)
#' p4_clean <- cuyem::clean_P4data(p4_raw)
#' tbl_lamprey(p4_clean)

tbl_lamprey <- function(data) {
  
  my21_clean %>%
    rename(species = srrverbose) %>%
    filter(speciesrunreartype == 'A0W') %>%
    group_by(streamname, lifestage) %>%
    summarize(n = sum(nfish, na.rm = TRUE)) %>%
    complete(streamname, lifestage = c('Ammocoete', 'Macropthalmia', 'Adult')) %>% # makes sure all life stages are present
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
