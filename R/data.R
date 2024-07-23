#' Time series of wind and solar energy production
#'
#' @description
#' This dataset contains hourly time series of wind and solar energy production
#' in 27 European countries in 2019.
#'
#' @format
#' An object of type \code{data.frame} containing 3 variables:
#' \describe{
#'   \item{date}{A \code{POSIXct} series of times at which energy production
#'   is available.}
#'   \item{country}{The country to which the energy production measurement corresponds.}
#'   \item{PWS}{The hourly wind and solar energy production for the corresponding time and country.}
#' }
#'
#' @details
#' The dataframe \code{data_supply} contains 236520 (24 x 365 x 27) rows, containing the wind
#' and solar energy production for each hour in 2019 for each of the 27 countries.
#'
#' This corresponds to a subset of the data used in Bloomfield and Brayshaw (2021),
#' which can be accessed at https://researchdata.reading.ac.uk/321/.
#' Users are referred to this paper for further details.
#'
#' @references
#' Bloomfield, Hannah and Brayshaw, David (2021):
#' ERA5 derived time series of European aggregated surface weather variables, wind power, and solar power capacity factors: hourly data from 1950-2020.
#' \doi{10.17864/1947.000321}
#'
#' @docType data
#'
#' @usage data("data_supply")
#'
#' @keywords datasets
#'
#' @examples
#' data("data_supply")
"data_supply"


#' Time series of average wind speed in Germany
#'
#' @description
#' This dataset contains a daily time series of average wind speeds across Germany between 1979 and 2019.
#'
#' @format
#' An object of type \code{data.frame} containing 2 variables:
#' \describe{
#'   \item{date}{A \code{POSIXct} series of times at which average wind speeds are available.}
#'   \item{wsmean}{The average wind speed in Germany for the corresponding time.}
#' }
#'
#' @details
#' The dataframe \code{data_wind_de} contains 14975 (365 x 41 + 10) rows, containing the
#' daily average wind speed in Germany for 41 years between 1979 and 2019. Ten leap
#' years occur within this period.
#'
#' This corresponds to a subset of the data that is publicly available at
#' https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-pressure-levels?tab=overview.
#' Users are referred to the reference below for further details.
#'
#' @references
#' Hersbach, H et al. (2023):
#' ERA5 hourly data on single levels from 1940 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS)
#' \doi{10.24381/cds.adbb2d47}
#' Accessed 01-09-2022.
#'
#' @docType data
#'
#' @usage data("data_wind_de")
#'
#' @keywords datasets
#'
#' @examples
#' data("wind_de")
"data_wind_de"

