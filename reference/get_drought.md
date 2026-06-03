# Get drought characteristics

Extract characteristics of droughts from a time series of values.
Drought characteristics include the occurrence, intensity, magnitude,
and duration of the drought.

## Usage

``` r
get_drought(
  x,
  thresholds = c(1.28, 1.64, 1.96),
  exceed = TRUE,
  cluster = 0,
  lag = NULL
)
```

## Arguments

- x:

  vector or xts object from which droughts are defined.

- thresholds:

  numeric vector containing thresholds to use when defining droughts.

- exceed:

  logical; `TRUE` if a drought is defined when `x` is above the
  thresholds, `FALSE` otherwise.

- cluster:

  integer specifying the number of time steps over which droughts should
  be clustered.

- lag:

  numeric specifying the value at which the drought should end.

## Value

A data frame containing the original values `x` and the corresponding
drought characteristics.

## Details

A drought is assumed to be defined as an instance when the vector `x`
exceeds (if `exceed = TRUE`) or falls below (if `exceed = FALSE`) the
specified thresholds in `thresholds`.

`thresholds` can be a single value, or a vector of values. In the latter
case, each threshold is assumed to be a different level or intensity of
the drought. If `exceed = TRUE` then a higher threshold corresponds to a
higher intensity, and if `exceed = FALSE` then a lower threshold
corresponds to a higher intensity. For example, if
`thresholds = c(1, 1.5, 2)`, then a level 1 drought occurs whenever `x`
exceeds 1 but is lower than 1.5, a level 2 drought occurs whenever `x`
exceeds 1.5 but is lower than 2, and a level 3 drought occurs whenever
`x` exceeds 2.

By default, `thresholds = c(1.28, 1.64, 1.96)`, which corresponds to the
90th, 95th, and 97.5th percentiles of the standard normal distribution.
These thresholds are often used alongside standardised indices to define
hydrometeorological droughts; see references.

`cluster` represents the number of time steps between different drought
events that should be attributed to the same drought. For example,
suppose \\x\_{i} \geq t, x\_{i + 1} \< t, x\_{i + 2} \geq t\\, where
\\x\_{i}\\ represents the \\i\\-th value in `x`, and \\t\\ is the lowest
threshold in `thresholds`. In this case, one drought event will finish
at time point \\i\\ and a new drought event will begin at time point
\\i + 2\\; no drought will occur at time point \\i + 1\\ because the
value \\x\_{i + 1}\\ is below the threshold defining a drought. Since
both \\x\_{i}\\ and \\x\_{i + 2}\\ are classed as drought events, it may
be desirable to ignore the fluctuation, and assume that the drought
persists through \\x\_{i + 1}\\ despite its value. This can be achieved
by setting `cluster = 1`. If there were two time points separating
different drought events, these can be clustered together by setting
`cluster = 2`, and so on. The default is that no clustering should be
implemented, i.e. `cluster = 0`.

Alternatively, we may wish to assume that the drought persists until `x`
falls below a value that is not necessarily equal to the threshold
defining a drought. For example, hydrometeorological droughts based on
standardised indices, such as the Standardised Precipitation Index
(SPI), are often defined to persist until the standardised index changes
sign, i.e. falls below zero. This can be achieved by setting `lag = 0`.
More generally, `lag` can be any numerical value. If `exceed = TRUE`, a
warning is issued if `lag` is above the lowest threshold, and if
`exceed = FALSE`, a warning is issued if `lag` is below the highest
threshold. If `lag` is `NULL` (the default), then no lagging is
performed.

`get_drought()` currently does not use the time series information in
the xts input, thereby assuming that the time series is complete,
without missing time periods. If `x` is a vector, rather than an xts
object, then this is also implicitly assumed.

The output is a dataframe containing the vector `x`, a logical vector
specifying whether each value of `x` corresponds to a drought event, and
the magnitude of the drought, defined as the sum of the values of `x`
during the drought; see references. The magnitude of the drought is only
shown on the last day of the drought. This makes it easier to compute
statistics about the drought magnitude, such as the average drought
magnitude. If `thresholds` is a vector, the intensity or level of the
drought is also returned.

## References

McKee, T. B., Doesken, N. J., & Kleist, J. (1993): \`The relationship of
drought frequency and duration to time scales', *In Proceedings of the
8th Conference on Applied Climatology* 17, 179-183.

Vicente-Serrano, S. M., Beguería, S., & López-Moreno, J. I. (2010): \`A
multiscalar drought index sensitive to global warming: the standardized
precipitation evapotranspiration index', *Journal of Climate* 23,
1696-1718.
[doi:10.1175/2009JCLI2909.1](https://doi.org/10.1175/2009JCLI2909.1)

Allen, S. & N. Otero (2023): \`Standardised indices to monitor energy
droughts', *Renewable Energy* 217, 119206.
[doi:10.1016/j.renene.2023.119206](https://doi.org/10.1016/j.renene.2023.119206)

## Author

Sam Allen, Noelia Otero

## Examples

``` r

data(data_supply)

# consider daily German energy supply data in 2019
supply_de <- subset(data_supply, country == "Germany", select = c("date", "PWS"))
supply_de <- xts::xts(supply_de$PWS, order.by = supply_de$date)
supply_de_std <- std_index(supply_de, rescale = "days", timescale = "hours")

# a drought may correspond to when energy supply is low
drought_df <- get_drought(supply_de_std, thresholds = c(-1.28, -1.64, -1.96), exceed = FALSE)
head(drought_df)
#>                 Index          x ins occ dur mag
#> 1 2019-01-01 23:00:00  1.6279182   0   0   0   0
#> 2 2019-01-02 23:00:00  1.1486964   0   0   0   0
#> 3 2019-01-03 23:00:00 -0.6468776   0   0   0   0
#> 4 2019-01-04 23:00:00  0.6809347   0   0   0   0
#> 5 2019-01-05 23:00:00  0.8261494   0   0   0   0
#> 6 2019-01-06 23:00:00 -1.2466276   0   0   0   0
mean(drought_df$occ) # droughts occur on roughly 10% of time steps
#> [1] 0.09589041

# cluster droughts two time steps apart
drought_df <- get_drought(supply_de_std, thresholds = c(-1.28, -1.64, -1.96),
                          cluster = 2, exceed = FALSE)
mean(drought_df$occ) # droughts occur on roughly 11% of time steps
#> [1] 0.109589

# let droughts persist until the standardised index changes sign
drought_df <- get_drought(supply_de_std, thresholds = c(-1.28, -1.64, -1.96),
                          lag = 0, exceed = FALSE)
mean(drought_df$occ) # droughts occur on roughly 17% of time steps
#> [1] 0.1671233

```
