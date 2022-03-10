#' @title create RST operations table from prepared Rotary Screw Trapping Data
#' @description produces report table for operational dates by trap
#' @param data clean, filtered P4 RST dataset from get_RSTdata() %>% clean_RSTdata() %>% my_RSTdata()
#' @export
#' @import dplyr
#' @author Tyler T. Stright
#' @examples
#' p4_raw <- cdmsR::getP4data(MigrationYear = 2021)
#' p4_clean <- cuyem::clean_P4data(p4_raw)
#' tbl_operations(p4_clean)


tbl_operations <- function(data){
  data %>%
    rename(species = srrverbose) %>%
    distinct(filename, .keep_all = TRUE) %>%
    group_by(streamname) %>%
    mutate(trapping_period = paste(min(event_date), '-', max(event_date)),
           s3_inop = if_else(grepl('S3', operational_condition) & hours_sampled == 0, 1, 0), # environment
           s5_inop = if_else(grepl('S5', operational_condition) & hours_sampled == 0, 1, 0), # staffing, equipment, etc.
           op_days = if_else(hours_sampled > 0, 1, 0)) %>%
    group_by(streamname, trapping_period) %>%
    summarize(`# days operated` = sum(op_days),
              Environment = sum(s3_inop),
              `Equipment/Staffing` = sum(s5_inop)) %>%
    arrange(streamname) %>%
    select(`Trap Site`=streamname, `Trapping Period`=trapping_period, `# days operated`, Environment, `Equipment/Staffing`) %>%
    flextable(cwidth = c(2, 3, 1, 1, 1)) %>%
    add_header_row(colwidths = c(3, 2),
                   values = c('', '# days not operated')) %>%
    vline(j = 3) %>%
    # merge_h(part = 'header') %>%
    align(j=c(2:5), align = 'center', part = 'header') %>%
    align(j=c(2:5), align = 'center', part = 'body') %>%
    font(fontname = 'Times New Roman', part = 'all')
}
