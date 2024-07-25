#' @title Plot standardised indices
#'
#' @description Plot a time series or histogram of standardised indices.
#'
#' @param x vector or xts object containing the indices to be plotted.
#' @param type type of plot (either time series "ts", histogram "hist", or barplot "bar").
#' @param title optional title of the plot.
#' @param lab axis label.
#' @param xlims,ylims lower and upper limits of the axes.
#' @param n_bins the number of bins to show in the histogram.
#'
#' @details
#' The \code{plot_sei()} function can be used to plot either a time series (if \code{type = "ts"})
#' or a histogram (if \code{type = "hist"} or \code{type = "bar"}) of the values in \code{x}.
#'
#' A time series can only be displayed if \code{x} is an \pkg{xts} time series.
#'
#' The argument \code{lab} is a string containing the label of the x-axis if
#' \code{type = "hist"} or \code{type = "bar"} and the y-axis if \code{type = "ts"}.
#'
#' The options \code{type = "hist"} and \code{type = "bar"} both display histograms
#' of the data \code{x}. With \code{type = "hist"}, \code{plot_sei()} is essentially a
#' wrapper of \code{geom_histogram()}, while \code{type = "bar"} is a wrapper of
#' \code{geom_bar()}. The latter can provide more flexibility when plotting bounded data,
#' whereas the former is easier to use when superimposing densities on top.
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
#' # type = "hist" and type = "bar both output a histogram of the index values
#' # type = "hist" can be useful to superimpose densities on top of the histogram
#' z <- seq(-3.5, 3.5, length.out = length(supply_de_std))
#' plot_sei(supply_de_std, type = "hist", title = "German SREPI in 2019") +
#'  ggplot2::geom_line(ggplot2::aes(x = z, y = dnorm(z)), col = "blue")
#'
#' # type = "bar" can be useful when the index values are bounded
#' supply_de_std <- std_index(supply_de, timescale = "hours", index_type = "prob11")
#' plot_sei(supply_de_std, type = "hist", xlims = c(-1, 1), title = 'type = "hist"')
#' plot_sei(supply_de_std, type = "bar", xlims = c(-1, 1), title = 'type = "bar"')
#'
#'
#' @name plot_sei
#' @importFrom stats density
NULL

#' @rdname plot_sei
#' @export
plot_sei <- function(x, type = c("ts", "hist", "bar"), title = NULL, lab = "Std. Index",
                     xlims = NULL, ylims = NULL, n_bins = 30){

  type <- match.arg(type)

  if (type == "ts") {
    if (xts::is.xts(x)) {
      df <- zoo::fortify.zoo(x)
      colnames(df) <- c("date", "x")
      p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = x)) +
        ggplot2::scale_x_datetime(name = "Date", expand = c(0, 0))
    } else {
      df <- data.frame(x = x, t = zoo::index(x))
      p <- ggplot2::ggplot(df, ggplot2::aes(x = t, y = x)) +
        ggplot2::scale_x_continuous(name = "Time step", expand = c(0, 0))
    }

    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = pmin(x, 0), ymax = 0), fill = "blue", alpha = 0.5) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = pmax(x, 0)), fill = "red", alpha = 0.5) +
      ggplot2::scale_y_continuous(name = lab, limits = ylims, expand = c(0, 0)) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = "grey", linetype = "dotted") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.margin = ggplot2::margin(c(5.5, 10.5, 5.5, 5.5))) +
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
                     axis.ticks.y = ggplot2::element_blank(),
                     plot.margin = ggplot2::margin(c(5.5, 10.5, 5.5, 5.5))) +
      ggplot2::ggtitle(title)
  } else if (type == "bar") {

    if (is.null(xlims)) xlims <- range(x) + 0.05*c(-min(x), max(x))
    bin_bounds <- seq(xlims[1], xlims[2], length.out = n_bins + 1)
    delta <- diff(bin_bounds)[1]
    rank_freq <- sapply(1:n_bins, function(i) mean((x >= bin_bounds[i]) & (x < bin_bounds[i + 1]), na.rm = T))

    df <- data.frame(freq = rank_freq, rank = bin_bounds[1:n_bins] + delta/2)
    p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "rank", y = "freq")) +
      ggplot2::geom_bar(stat = "identity", width = delta, col = "black", alpha = 0.8) +
      ggplot2::scale_x_continuous(name = lab, limits = xlims) +
      ggplot2::scale_y_continuous(name = "Density", limits = ylims, expand = ggplot2::expansion(c(0, 0.05))) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::ggtitle(title)
  }

  return(p)

}


