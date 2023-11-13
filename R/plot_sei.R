#' @title Plot standardised indices
#'
#' @description Plot a time series containing standardised indices, or a histogram
#' of the indices.
#'
#' @param x vector or xts object containing the indices to be plotted.
#' @param type type of plot (either time series "ts", or histogram "hist").
#' @param title optional title of the plot.
#' @param lab axis label.
#' @param xlims,ylims lower and upper limits of the axes.
#' @param n_bins the number of bins to show in the histogram.
#'
#' @details
#' The \code{plot_sei()} function can be used to plot either a time series (if
#' \code{type = "ts"}) or a histogram (if \code{type = "hist"}) of the values in \code{x}.
#'
#' A time series can only be displayed if \code{x} is an \pkg{xts} time series.
#'
#' The argument \code{lab} is a string containing the label of the x-axis if
#' \code{type = "hist"} and the y-axis if \code{type = "ts"}.
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
#' supply_de_std <- std_index(supply_de, timescale = "hours")
#'
#' plot_sei(supply_de, title = "German renewable energy production in 2019")
#' plot_sei(supply_de_std, title = "German SREPI in 2019")
#'
#' plot_sei(supply_de, type = "hist", title = "German renewable energy production in 2019")
#' plot_sei(supply_de_std, type = "hist", title = "German SREPI in 2019")
#'
#' @name plot_sei
#' @importFrom stats density
NULL

#' @rdname plot_sei
#' @export
plot_sei <- function(x, type = c("ts", "hist"), title = NULL, lab = "Std. Index", xlims = NULL, ylims = NULL, n_bins = 30){

  type <- match.arg(type)

  if (type == "ts") {
    df <- zoo::fortify.zoo(x)
    colnames(df) <- c("date", "x")
    p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = x)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = pmin(x, 0), ymax = 0), fill = "blue", alpha = 0.5) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = pmax(x, 0)), fill = "red", alpha = 0.5) +
      ggplot2::scale_x_datetime(name = "Date", expand = c(0, 0)) +
      ggplot2::scale_y_continuous(name = lab, limits = ylims, expand = c(0, 0)) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = "grey", linetype = "dotted") +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(title)
  } else if (type == "hist") {
    if (xts::is.xts(x)) {
      df <- zoo::fortify.zoo(x)
    } else {
      df <- data.frame(x = x)
    }
    p <- ggplot2::ggplot(df) +
      ggplot2::geom_histogram(ggplot2::aes(x = as.numeric(x), y = ggplot2::after_stat(density)), col = "black", bins = n_bins, alpha = 0.4) +
      ggplot2::scale_x_continuous(name = lab, limits = xlims, expand = ggplot2::expansion(c(0, 0))) +
      ggplot2::scale_y_continuous(name = "Density", limits = ylims, expand = ggplot2::expansion(c(0, 0.05))) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::ggtitle(title)
  }

  return(p)

}


