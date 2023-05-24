# SEI

An R package to calculate standardised indices. Standardised indices are frequently used to convert measurements to a common scale with a meaningful probabilistic interpretation. Well-known examples of such indices include the Standardised Precipitation Index (SPI) and Standardised Precipitation Evapotranpiration Index (SPEI), which are commonly used to monitor hydrological droughts. However, this package can be employed to calculate indices in a wide range of settings. For example, it can readily be used to calculate standardised energy indices, including the Standardised Residual Load Index (SRLI), and the Standardised Renewable Energy Production Index (SREPI).

## Features
The main features of this package include:
  - A wide range of distributions from which to calculate standardised indices (including the empirical distribution)
  - The ability to compute indices from a rolling window of values
  - The capability to construct indices on any timescale
  
## Installation

This package has not been submitted to CRAN. The developmental version can be installed using devtools:
```r
# install.packages("devtools")
library(devtools)
install_github("noeliaof/SEI")
```
The vignette lists several possible extensions that could be implemented. Additional comments, suggestions, and input are more than welcome.

## Background

Standardised indices provide a means to convert measurements to a common and interpretable scale. This is useful for many reasons. For example:
1) The indices are defined on a common scale, and are thus easy to interpret. 
2) This standardised scale also has an underlying probabilistic interpretation, making the indices ideal for risk management and decision-making. 
3) Since the standardisation can be performed separately for different conditions (e.g. seasons or locations), the indices can be defined in a relative sense, allowing the measurements in different conditions to readily be compared. 

The general approach to compute the indices involves estimating the cumulative distribution function F of the measurements, using this to obtain probability integral transform values, and then transforming these using the Gaussian quantile function. The result is a sample of values from a standard normal distribution. A negative value represents a measurement that is lower than average, while a positive value represents an above-average measurement. A very high or low index constitutes an extreme value, and we can deduce how extreme, or unexpected, the value is by comparing it to quantiles of the standard normal distribution. 

The main challenge when calculating standardised indices is estimating the cumulative distribution function of the measurements. This can be achieved by assuming some parametric distribution: the SPI, for example, typically assumes precipitation follows a gamma distribution. Alternatively, we can use kernel density estimation or the empirical distribution function to get a more flexible estimate; however, these methods typically require more data than parametric distributions. In any case, it is important to check that the estimated distribution is valid. This package offers a wide range of distributions, and additionally provides functions to check for the distribution fit. 

The theory underlying standardised indices is discussed in more detail in the package vignette.
