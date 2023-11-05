# SEI

SEI is an R package to calculate standardised indices. Standardised indices are frequently used to convert measurements to a common scale with a meaningful probabilistic interpretation. Well-known examples include the Standardised Precipitation Index (SPI) and Standardised Precipitation Evapotranspiration Index (SPEI), which are used to monitor hydrological droughts. Standardised indices have also recently been proposed to monitor energy droughts, including the Standardised Residual Load Index (SRLI), and the Standardised Renewable Energy Production Index (SREPI). 

These standardised energy indices are discussed in
> Allen, S. and Otero, N (2023). 
> Standardised indices to monitor energy droughts.
> Renewable Energy.
> [https://doi.org/10.1016/j.renene.2023.119206](https://doi.org/10.1016/j.renene.2023.119206)

This package facilitates the implementation of standardised indices in practice. In contrast to other software packages, the SEI package includes 
  - A wide range of distributions from which to calculate standardised indices (including the empirical distribution),
  - Diagnostic checks to ensure the chosen distribution is valid for the input data,
  - The ability to compute indices from a rolling window of values,
  - The ability to compute indices on any timescale,
  - Several notions of standardised indices.
  
## Installation

This package has not been submitted to CRAN. The developmental version can be installed using devtools:
```r
# install.packages("devtools")
library(devtools)
install_github("noeliaof/SEI", build_vignettes = TRUE)
```
The vignette lists several possible extensions that could be implemented. Additional comments, suggestions, and input are more than welcome.

## Background

Standardised indices provide a means to convert measurements to a common and interpretable scale. This is useful for many reasons. For example:
1) The common scale renders the indices easy to interpret. 
2) This standardised scale also has an underlying probabilistic interpretation, making the indices ideal for risk management and decision-making. 
3) Since the standardisation can be performed separately under different conditions (e.g. in different seasons or locations), the indices can be defined in a relative sense, facilitating a comparison between measurements in these different conditions. 

The general approach to compute the indices is to estimate the cumulative distribution function F underlying the measurements, use this to derive probability integral transform (PIT) values, and then to transform these PIT values using the Gaussian quantile function. If F has been estimated accurately, the resulting values will resemble a sample from a standard normal distribution. A negative value represents a measurement that is lower than average, while a positive value represents an above-average measurement. A very high or low index constitutes an extreme value, and we can deduce how extreme the value is by comparing it to quantiles of the standard normal distribution. 

The main challenge when calculating standardised indices is estimating the cumulative distribution function F of the measurements. This can be achieved by assuming some parametric distribution: the SPI, for example, typically assumes precipitation follows a gamma distribution. Alternatively, we can use kernel density estimation or the empirical distribution function to get a more flexible estimate; however, these methods typically require more data than parametric distributions. In any case, it is important to check that the estimated distribution is valid. This package offers a wide range of distributions, and additionally provides diagnostic tools to check that the chosen distribution adequately fits the input data. 

The theory underlying standardised indices is discussed in more detail in the package vignette.
