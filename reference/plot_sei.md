# Plot standardised indices

Plot a time series or histogram of standardised indices.

## Usage

``` r
plot_sei(
  x,
  type = c("ts", "hist", "bar"),
  title = NULL,
  lab = "Std. Index",
  xlims = NULL,
  ylims = NULL,
  n_bins = 30
)
```

## Arguments

- x:

  vector or xts object containing the indices to be plotted.

- type:

  type of plot (either time series "ts", histogram "hist", or barplot
  "bar").

- title:

  optional title of the plot.

- lab:

  axis label.

- xlims, ylims:

  lower and upper limits of the axes.

- n_bins:

  the number of bins to show in the histogram.

## Value

A ggplot object displaying the standardised index values.

## Details

The `plot_sei()` function can be used to plot either a time series (if
`type = "ts"`) or a histogram (if `type = "hist"` or `type = "bar"`) of
the values in `x`.

A time series can only be displayed if `x` is an xts time series.

The argument `lab` is a string containing the label of the x-axis if
`type = "hist"` or `type = "bar"` and the y-axis if `type = "ts"`.

The options `type = "hist"` and `type = "bar"` both display histograms
of the data `x`. With `type = "hist"`, `plot_sei()` is essentially a
wrapper of `geom_histogram()`, while `type = "bar"` is a wrapper of
`geom_bar()`. The latter can provide more flexibility when plotting
bounded data, whereas the former is easier to use when superimposing
densities on top.

## Author

Sam Allen, Noelia Otero

## Examples

``` r
data(data_supply)
# consider hourly German energy supply data in 2019
supply_de <- subset(data_supply, country == "Germany", select = c("date", "PWS"))
supply_de <- xts::xts(supply_de$PWS, order.by = supply_de$date)
supply_de_std <- std_index(supply_de, timescale = "hours")

plot_sei(supply_de, title = "German renewable energy production in 2019")
#> Warning: In `margin()`, the argument `t` should have length 1, not length 4.
#> ℹ Argument get(s) truncated to length 1.

plot_sei(supply_de_std, title = "German SREPI in 2019")
#> Warning: In `margin()`, the argument `t` should have length 1, not length 4.
#> ℹ Argument get(s) truncated to length 1.


plot_sei(supply_de, type = "hist", title = "German renewable energy production in 2019")
#> Warning: In `margin()`, the argument `t` should have length 1, not length 4.
#> ℹ Argument get(s) truncated to length 1.

plot_sei(supply_de_std, type = "hist", title = "German SREPI in 2019")
#> Warning: In `margin()`, the argument `t` should have length 1, not length 4.
#> ℹ Argument get(s) truncated to length 1.


# type = "hist" and type = "bar both output a histogram of the index values
# type = "hist" can be useful to superimpose densities on top of the histogram
z <- seq(-3.5, 3.5, length.out = length(supply_de_std))
plot_sei(supply_de_std, type = "hist", title = "German SREPI in 2019") +
 ggplot2::geom_line(ggplot2::aes(x = z, y = dnorm(z)), col = "blue")
#> Warning: In `margin()`, the argument `t` should have length 1, not length 4.
#> ℹ Argument get(s) truncated to length 1.


# type = "bar" can be useful when the index values are bounded
supply_de_std <- std_index(supply_de, timescale = "hours", index_type = "prob11")
plot_sei(supply_de_std, type = "hist", xlims = c(-1, 1), title = 'type = "hist"')
#> Warning: In `margin()`, the argument `t` should have length 1, not length 4.
#> ℹ Argument get(s) truncated to length 1.
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_bar()`).

plot_sei(supply_de_std, type = "bar", xlims = c(-1, 1), title = 'type = "bar"')
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the SEI package.
#>   Please report the issue to the authors.


```
