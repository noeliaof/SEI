% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{data_supply}
\alias{data_supply}
\title{Time series of wind and solar energy production}
\format{
An object of type \code{data.frame} containing 3 variables:
\describe{
\item{date}{A \code{POSIXct} series of times at which energy production
is available.}
\item{country}{The country to which the energy production measurement corresponds.}
\item{PWS}{The hourly wind and solar energy production for the corresponding time and country.}
}
}
\usage{
data("data_supply")
}
\description{
This dataset contains hourly time series of wind and solar energy production
in 27 European countries in 2019.
}
\details{
The dataframe \code{data_supply} contains 236520 (24 x 365 x 27) rows, containing the wind
and solar energy production for each hour in 2019 for each of the 27 countries.

This corresponds to a subset of the data used in Bloomfield and Brayshaw (2021),
which can be accessed at https://researchdata.reading.ac.uk/321/.
Users are referred to this paper for further details.
}
\examples{
data("data_supply")
}
\references{
Bloomfield, Hannah and Brayshaw, David (2021):
ERA5 derived time series of European aggregated surface weather variables, wind power, and solar power capacity factors: hourly data from 1950-2020.
\doi{10.17864/1947.000321}
}
\keyword{datasets}
