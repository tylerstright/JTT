#' @title gg_environment:
#'
#' @description graph environmental data (water temperature and flow)
#'
#' @author Tyler Stright
#'
#' @param data average daily values for water temp and discharge ready for graphing (Required Fields: StreamName, Date, MeanDailyTemp and/or MeanDailyFlow) 
#'
#' @param metric indicate what metrics to include. Default='temp and discharge'
#'
#' @param save save .png of plots? Default: 'yes'
#'
#' @param folder_path path to desired folder for saving plots.
#' 
#' @import lubridate readr httr dplyr tidyr
#' @export
#' @return NULL
#' @examples gg_environment(data = imn_waterdat, save = 'yes', folderpath = './data/my21/environment/')

gg_environment = function(data, 
                          metric = c('temperature and flow', 'temperature', 'flow'), 
                          save=c('no', 'yes'), 
                          folder_path) {
  
  stopifnot(!is.null(data))
  metric <- match.arg(metric, several.ok = FALSE)
  save <- match.arg(save, several.ok = FALSE)
  
  
  # Pull StreamName
  graphing_stream <- unique(data$StreamName)
  
  # Temp and Flow ----
  if(metric == 'temperature and flow') {
    # calculate scale factor for dual y-axis.
    scaleFactor <- round(max(data$MeanDailyTemp, na.rm=TRUE)/max(data$MeanDailyFlow, na.rm=TRUE), 5)
    
    g <- ggplot(data, aes(x=Date)) +
      geom_line(aes(y=MeanDailyTemp), color = 'red') +
      geom_line(aes(y=MeanDailyFlow*scaleFactor), color = 'blue', linetype = 'dashed') +
      scale_y_continuous(name=expression(paste("Temperature ("*degree*"C)"), sep=''), breaks = scales::breaks_pretty(7),
                         sec.axis=sec_axis(~./scaleFactor, name = expression(paste("Discharge ("*ft^3*"/s)", sep='')), breaks = scales::breaks_pretty(7))) +
      scale_x_date(name = '', labels = scales::label_date("%m/%d/%y"), breaks = scales::breaks_pretty(13)) +
      theme_bw() +
      theme(
        plot.title = element_text(family = 'serif'),
        axis.text.x = element_text(hjust = 1, angle = 45, size = 12, family = 'serif'),
        axis.ticks.length.x = unit(.15, "cm"),
        axis.title.y.left=element_text(color="red", size = 13, family = 'serif'),
        axis.text.y.left=element_text(color="red", size = 11, family = 'serif'),
        axis.title.y.right=element_text(color="blue", size = 13, family = 'serif'),
        axis.text.y.right=element_text(color="blue", size = 11, family = 'serif'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      ggtitle(label = paste0(graphing_stream, ' - Mean Daily Temperature and Discharge')) +
      theme(plot.title=element_text(hjust=0.5))
    
    if(save == 'yes'){
    ggsave(filename = paste(folder_path, gsub(' ', '', graphing_stream), '_env', year(max(data$Date)), '.jpg', sep=''),
           device = 'jpeg', units = 'in', width = 6.5, height = 3.5)
    }
    
    return(g)
  }
  
  # Temperature only ----
  if(metric == 'temperature'){
    
    g <- ggplot(data, aes(x=Date)) +
      geom_line(aes(y=MeanDailyTemp), color = 'red') +
      scale_y_continuous(name=expression(paste("Temperature ("*degree*"C)"), sep=''), breaks = scales::breaks_pretty(7)) +
      scale_x_date(name = '', labels = scales::label_date("%m/%d/%y"), breaks = scales::breaks_pretty(13)) +
      theme_bw() +
      theme(
        plot.title = element_text(family = 'serif'),
        axis.text.x = element_text(hjust = 1, angle = 45, size = 12, family = 'serif'),
        axis.ticks.length.x = unit(.15, "cm"),
        axis.title.y.left=element_text(color="red", size = 13, family = 'serif'),
        axis.text.y.left=element_text(color="red", size = 11, family = 'serif'),
        axis.title.y.right=element_text(color="blue", size = 13, family = 'serif'),
        axis.text.y.right=element_text(color="blue", size = 11, family = 'serif'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      ggtitle(label = paste0(graphing_stream, ' - Mean Daily Temperature')) +
      theme(plot.title=element_text(hjust=0.5))
    
    if(save == 'yes'){
    ggsave(filename = paste(folder_path, gsub(' ', '', graphing_stream), '_env', year(max(data$Date)), '.jpg', sep=''),
           device = 'jpeg', units = 'in', width = 6.5, height = 3.5)
    }
    
    return(g)
  }
  
  # Flow only ----
  if(metric == 'flow'){
    
    g <- ggplot(data, aes(x=Date)) +
      geom_line(aes(y=MeanDailyFlow), color = 'blue', linetype = 'dashed') +
      scale_y_continuous(name = expression(paste("Discharge ("*ft^3*"/s)", sep='')), breaks = scales::breaks_pretty(7)) +
      scale_x_date(name = '', labels = scales::label_date("%m/%d/%y"), breaks = scales::breaks_pretty(13)) +
      theme_bw() +
      theme(
        plot.title = element_text(family = 'serif'),
        axis.text.x = element_text(hjust = 1, angle = 45, size = 12, family = 'serif'),
        axis.ticks.length.x = unit(.15, "cm"),
        axis.title.y.left=element_text(color="red", size = 13, family = 'serif'),
        axis.text.y.left=element_text(color="red", size = 11, family = 'serif'),
        axis.title.y.right=element_text(color="blue", size = 13, family = 'serif'),
        axis.text.y.right=element_text(color="blue", size = 11, family = 'serif'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      ggtitle(label = paste0(graphing_stream, ' - Mean Daily Discharge')) +
      theme(plot.title=element_text(hjust=0.5))
    
    if(save == 'yes'){
    ggsave(filename = paste(folder_path, gsub(' ', '', graphing_stream), '_env', year(max(data$Date)), '.jpg', sep=''),
           device = 'jpeg', units = 'in', width = 6.5, height = 3.5)
    }
      
    return(g)
  }
  
}