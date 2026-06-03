# Time series of average wind speed in Germany

This dataset contains a daily time series of average wind speeds across
Germany between 1979 and 2019.

## Usage

``` r
data("data_wind_de")
```

## Format

An object of type `data.frame` containing 2 variables:

- date:

  A `POSIXct` series of times at which average wind speeds are
  available.

- wsmean:

  The average wind speed in Germany for the corresponding time.

## Details

The dataframe `data_wind_de` contains 14975 (365 x 41 + 10) rows,
containing the daily average wind speed in Germany for 41 years between
1979 and 2019. Ten leap years occur within this period.

This corresponds to a subset of the data that is publicly available at
https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-pressure-levels?tab=overview.
Users are referred to the reference below for further details.

## References

Hersbach, H et al. (2023): ERA5 hourly data on single levels from 1940
to present. Copernicus Climate Change Service (C3S) Climate Data Store
(CDS) [doi:10.24381/cds.adbb2d47](https://doi.org/10.24381/cds.adbb2d47)
Accessed 01-09-2022.

## Examples

``` r
data("wind_de")
#> Warning: data set ‘wind_de’ not found
```
