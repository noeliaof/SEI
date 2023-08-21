#' @title Plot standardised indices
#'
#' @description Plot a time series containing standardised indices, or a histogram
#' of the indices.
#'
#' @param x vector or xts object containing the indices to be plotted.
#' @param type type of plot (either time series "ts", or histogram "hist").
#' @param title optional title of the plot.
#' @param lab axis label (y-axis if type = "ts", x-axis if type = "hist").
#' @param n_bins the number of bins to show in the histogram.
#'
#' @return
#' A ggplot object displaying the standardised index values.
#'
#' @author Sam Allen, Noelia Otero
#'
#' @examples
#' data(data_supply)
#' # consider hourly German energy supply data in 2019
#' supply_de <- subset(data_supply, country == "Germany", select = c("date", "PWS"))
#' supply_de <- xts::xts(supply_de$PWS, order.by = supply_de$date)
#' options(xts_check_TZ = FALSE)
#' supply_de_std <- std_index(supply_de, timescale = "hours")
#'
#' plot_sei(supply_de, title = "German renewable energy production in 2019")
#' plot_sei(supply_de_std, title = "German SREPI in 2019")
#'
#' plot_sei(supply_de, type = "hist", title = "German renewable energy production in 2019")
#' plot_sei(supply_de_std, type = "hist", title = "German SREPI in 2019")
#'
#' @name plot_sei
NULL

#' @rdname plot_sei
#' @export
plot_sei <- function(x, type = c("ts", "hist"), title = NULL, lab = "Std. Index", ylims = NULL, n_bins = 30){

  type <- match.arg(type)

  if (type == "ts") {
    df <- zoo::fortify.zoo(x)
    p <- ggplot(df, aes(x = Index, y = x)) +
      geom_ribbon(aes(ymin = pmin(x, 0), ymax = 0), fill = "blue", alpha = 0.5) +
      geom_ribbon(aes(ymin = 0, ymax = pmax(x, 0)), fill = "red", alpha = 0.5) +
      scale_x_datetime(name = "Date", expand = c(0, 0)) +
      scale_y_continuous(name = lab, limits = ylims, expand = c(0, 0)) +
      geom_hline(aes(yintercept = 0), color = "grey", linetype = "dotted") +
      theme_bw() +
      ggtitle(title)
  } else if (type == "hist") {
    if (xts::is.xts(x)) {
      df <- zoo::fortify.zoo(x)
    } else {
      df <- data.frame(x = x)
    }
    p <- ggplot(df) +
      geom_histogram(aes_string(x = x), col = "black", bins = n_bins, alpha = 0.4) +
      xlab(lab) +
      scale_y_continuous(name = "Density", expand = expansion(c(0, 0.05))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      ggtitle(title)
  }

  return(p)

}


