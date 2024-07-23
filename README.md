[![](https://cranlogs.r-pkg.org/badges/SEI)](https://cran.r-project.org/package=SEI)

# SEI

SEI is an R package to calculate standardised indices. Standardised indices are frequently used to convert measurements to a common scale with a meaningful probabilistic interpretation. Well-known examples include the Standardised Precipitation Index (SPI) and Standardised Precipitation Evapotranspiration Index (SPEI), which are used to monitor hydrological droughts, and the Standardised Residual Load Index (SRLI), and the Standardised Renewable Energy Production Index (SREPI), which have been proposed to monitor energy droughts.
 
This package facilitates the implementation of standardised indices in practice. It's main features include:
  - A range of distributions from which to calculate standardised indices (including flexible non- and semi-parametric methods),
  - Non-stationary parametric distribution estimation via the GAMLSS framework,
  - Estimation of distribution parameters using several methods, including maximum likelihood, method of moments, and method of L-moments,
  - Diagnostic checks to ensure the distributional assumptions are valid for the input data,
  - The ability to compute indices on any timescale, and from a rolling window of values,
  - Three relevant notions of standardised indices,
  - Functions to visualise the calculated standardised indices.

The functionality and usage of the SEI package is discussed in detail in the corresponding [vignette](https://eartharxiv.org/repository/view/6290/). The vignette also lists several possible extensions that could be implemented. Additional comments, suggestions, and input are more than welcome - please feel free to create an issue or get in touch with the authors.


## Installation

The SEI package is available on [CRAN](https://CRAN.R-project.org/package=SEI):
```r
install.packages("SEI")
```

and the developmental version on GitHub can be installed using devtools:
```r
# install.packages("devtools")
library(devtools)
install_github("noeliaof/SEI", build_vignettes = TRUE)
```


## Background

Standardised indices provide a means to convert measurements to a common and interpretable scale. This is useful for many reasons. For example:
1) The common scale renders the indices easy to interpret. 
2) This standardised scale has an underlying probabilistic interpretation, making the indices ideal for risk management and decision-making. 
3) Since the standardisation can be performed separately under different conditions (e.g. in different seasons or locations), the indices can be defined in a relative sense, facilitating a comparison between measurements in these different conditions. 

The general approach to compute the indices is to estimate the cumulative distribution function $F$ underlying the measurements, use this to derive probability integral transform (PIT) values, and then to transform these PIT values using the Gaussian quantile function. If $F$ has been estimated accurately, the resulting values will resemble a sample from a standard normal distribution. A negative value represents a measurement that is lower than average, while a positive value represents an above-average measurement. A very high or low index constitutes an extreme value, and we can deduce how extreme the value is by comparing it to quantiles of the standard normal distribution. 

The main challenge when calculating standardised indices is estimating the cumulative distribution function $F$ of the measurements. This can be achieved by assuming some parametric distribution: the SPI, for example, typically assumes precipitation follows a gamma distribution. Alternatively, we can use kernel density estimation or the empirical distribution function to get a more flexible estimate. Non-stationary distributions that depend on predictor variables can also be employed if the standardised indices should account for trends in the data, for example. In any case, it is important to check that the estimated distribution is valid. 

The SEI package offers a wide range of distributions, and additionally provides diagnostic tools to check that the chosen distribution adequately fits the input data. Further details regarding the theory underlying standardised indices, and the functionality of the SEI package, are provided in the package [vignette](https://eartharxiv.org/repository/view/6290/).


## Citation

To cite the SEI package, please use the following BibTeX entry:

```
@article{SEI,
  title={Calculating Standardised Indices Using SEI},
  author={Allen, Sam and Otero, Noelia},
  year={2023},
  publisher={EarthArXiv}
}
```
