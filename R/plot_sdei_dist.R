#' Function to plot the index distribution
#' @param dx dataframe with the SDEI information, dates, index and type of standardized index
#' @param ivar index to plot
#' @param yy yeas of study
#' @param title optional title of the plot
#' @param n_bins the number of bins to show
#' @export
#'
plot_sdei_dist <- function(dx, ivar ="SDEI", yy = seq(1979, 2019), n_bins = 100, title = ""){
  # Visualise the distribution of the index

  if(length(unique(dx$type)) > 1){
    p <- dx%>%dplyr::filter(format(date,"%Y")%in%yy)%>%
      ggplot() +
      geom_histogram(aes_string(x=ivar), col = "black", bins = n_bins, alpha = 0.4) +
      xlab(title) + scale_y_continuous(name = "Density", expand = expansion(c(0, 0.05))) +
      facet_wrap(~type) +
      theme_bw() +
      theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            axis.text.y=element_blank(), axis.ticks.y=element_blank())
  }else{
    p <- dx%>%dplyr::filter(format(date,"%Y")%in%yy)%>%
      ggplot() +
      geom_histogram(aes_string(x=ivar), col = "black", bins = n_bins, alpha = 0.4) +
      xlab(title) + scale_y_continuous(name = "Density", expand = expansion(c(0, 0.05))) +
      theme_bw() +
      theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            axis.text.y=element_blank(), axis.ticks.y=element_blank())
  }

  return(p)

}





