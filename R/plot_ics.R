#' Function to plot installed capacity values from the data
#' @param df dataframe of the form of data_enerH, with the values of IC wind and solar
#' @param names of the countries o be plotted
#' @export
plot_ics <- function(df, names=NULL){

  # maybe this function can be modified a bit to make its use more flexible
  # function to plot installed wind and solar capacities in each country

  if(is.null(names)){
    names <- df$country
  }

  plot_icw <- ggplot2::ggplot(data.frame(n=names, c=df$IC17_wind)) +
    geom_bar(aes(x = n, y = c), stat="identity") +
    scale_y_continuous(name="Installed wind capacity (2017)", expand = expansion(c(0, 0.05))) +
    scale_x_discrete(name="") + theme_bw()

  plot_ics <- ggplot2::ggplot(data.frame(n=names, c=df$IC17_solar)) +
    geom_bar(aes(x = n, y = c), stat="identity") +
    scale_y_continuous(name="Installed solar capacity (2017)", expand = expansion(c(0, 0.05))) +
    scale_x_discrete(name="") + theme_bw()

  gridExtra::grid.arrange(plot_icw, plot_ics)

}
