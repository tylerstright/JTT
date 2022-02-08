#' @title create incidental catch table from prepared Rotary Screw Trapping Data
#' @description produces report table for incidental catch by trap
#' @param data clean, filtered P4 RST dataset from get_RSTdata() %>% clean_RSTdata() %>% my_RSTdata()
#' @export
#' @import dplyr
#' @author Tyler T. Stright
#' @examples
#' p4_raw <- cdmsR::getP4data(MigrationYear = 2021, CaptureMethod = 'SCREWT')
#' p4_clean <- cuyem::clean_P4data(p4_raw)
#' tbl_incidentatl(p4_clean)

tbl_incidental <- function(data){

  data %>%
    rename(species = srrverbose) %>%
    filter(target == 0,
           !speciesrunreartype %in% c('NFD', '00U', '05U' ,'90U')) %>%
    group_by(family, streamname, species, scientific_name) %>%
    summarize(n = sum(nfish)) %>%
    spread(key = streamname, value = n) %>%
    select(Family = family, `Common Name` = species, `Scientific Name` = scientific_name, everything()) %>%
    mutate(across(everything(), .fns= ~replace_na(., 0))) %>%
    flextable(cwidth = c(1, 2, 2, 1, 1, 1, 1, 1)) %>%
    add_header_row(colwidths = c(3, 5),
                   values = c(NA, 'Trap Site')) %>%
    align(j=c(4:8), align = 'center', part = 'header') %>%
    compose(j = 'Scientific Name', value = as_paragraph(as_i(`Scientific Name`))) %>%
    align(j=c(4:8), align = 'center', part = 'body') %>%
    font(fontname = 'Times New Roman', part = 'all')

}
