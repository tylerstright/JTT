#' @title create RST operations table from prepared Rotary Screw Trapping Data
#' @description produces report table for operational dates by trap
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
#' operations_table <- tbl_operations(rst_my)


tbl_operations <- function(data){
  rst_my %>%
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
