#' @title Plot drought characteristics derived by the \code{funSDEI} 
#' 
#' @param input_data dataframe to plot
#' @param mvar variable for plotting
#' @param fvar variable for multiplot, it can be the season
#' @export
#'

# plot energy drought characteristics
plot_index_UEmap <- function(input_data, mvar, fvar) {
  #### Load shapefiles
  eumap_0_2 <-  readr::read_rds(system.file("eumap_merged_0_2_2016.rds", package = "panas"))
  #### Check the input data frame
  if (!is.data.frame(input_data)) {
    stop('The input_data is not a data frame')
  }
  # change the name of the variable to plot 
  colnames(input_data)[colnames(input_data)==mvar] <- "value"
  
  #### Homogenise the column names
  # colnames(input_data) = c('area','value')
  ## Remove unneeded regions (to speed up the plot)
  if (max(stringr::str_length(input_data$area)) == 2) {
    eumap_0_2 <-  dplyr::filter(eumap_0_2, stringr::str_length(id) == 2)
  } else if (max(stringr::str_length(input_data$area)) == 3) {
    eumap_0_2 <-  dplyr::filter(eumap_0_2, stringr::str_length(id) <= 3)
  }
  ## Match EUMAP ADM0 names
  input_data <-  mutate(input_data, area = if_else(area == 'GR', 'EL', area)) %>%
    mutate(area = if_else(area == 'GB', 'UK', area))
  
  jj <-  left_join(eumap_0_2, input_data, by = c('id' = 'area')) %>%
    mutate(level = stringr::str_length(id))
  
  if (!is.null(fvar)){
    jj <- jj[which(!is.na(jj[fvar])),]
  }
  
  # define colour palette
  myPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))
  
  g <-  ggplot2::ggplot(jj, aes(x = long, y = lat, group = group)) +
    borders(database = 'world', regions = '.', fill = 'white', size = 0.1) +
    geom_polygon(color = 'black', size = 0.1, fill = 'white') +
    geom_polygon(data = dplyr::filter(jj, !is.na(value)), aes(fill = value), color = 'white', size = 0.1) +
    geom_path(data = dplyr::filter(jj, level == 2), color = 'gray50', size = 0.1) +
    scale_fill_stepsn(colours = myPalette(12), n.breaks=10, name= mvar) +
    coord_quickmap(ylim = c(35, 65), xlim = c(-15, 25))
  
  if (!is.null(fvar)){
    
    gp <-  g + facet_grid(reformulate(fvar)) +
      xlab('Longitude') + ylab('Latitude') +
      theme_bw() +
      theme( legend.key.width = unit(0.5,"cm"),
             legend.key.height  = unit(1.5,"cm"),
             legend.box = "vertical")
    
  }else{
    
    gp <- g +
      xlab('Longitude') + ylab('Latitude') +
      theme_bw() +
      theme( legend.key.width = unit(0.5,"cm"),
             legend.key.height  = unit(1.5,"cm"),legend.box = "vertical")
    
  }
  return(gp)
}
