scgUtils <a href="https://sarahcgall.github.io/scgUtils/"><img src="man/figures/logo.png" align="right" height="138" alt="" /></a>
================
<!-- badges: start -->
[![Release](https://img.shields.io/badge/Release-development%20version%200&#46;0&#46;1-1c75bc)](https://github.com/sarahcgall/scgUtils)
[![R-CMD-check](https://github.com/sarahcgall/scgUtils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sarahcgall/scgUtils/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/sarahcgall/scgUtils/graph/badge.svg?token=SG99DJ56I4)](https://codecov.io/gh/sarahcgall/scgUtils)
<!-- badges: end -->

## Overview
`scgUtils` is a comprehensive R package designed to streamline the process of survey data analysis and visualisation. 
It offers a range of functions for data processing, exploration, and stylish visualisations, adhering to branding 
guidelines with custom colour schemes and themes.

### Features

- **Survey Data Processing**: Simplify the manipulation of survey datasets with functions like [`get_data()`](https://sarahcgall.github.io/scgUtils/reference/get_data.html) and [`process_factors()`](https://sarahcgall.github.io/scgUtils/reference/process_factors.html).
- **Data Exploration Tools**: Dive into your data using functions such as [`crosstab()`](https://sarahcgall.github.io/scgUtils/reference/crosstab.html) and [`grp_freq()`](https://sarahcgall.github.io/scgUtils/reference/grp_freq.html) for detailed analyses.
- **Custom Visualizations**: Bring your data to life with various plotting functions, from [`plot_bigfive()`](https://sarahcgall.github.io/scgUtils/reference/plot_bigfive.html) for personality traits to [`plot_sankey()`](https://sarahcgall.github.io/scgUtils/reference/plot_sankey.html) for flow data representation.
- **Styling and Brand Consistency**: Maintain brand integrity with [`colour_pal()`](https://sarahcgall.github.io/scgUtils/reference/colour_pal.html) and [`theme_scg()`](https://sarahcgall.github.io/scgUtils/reference/theme_scg.html), ensuring consistent and appealing data presentations.

### Installation

Install the development version of `scgUtils` directly from GitHub:

``` r
devtools::install_github("sarahcgall/scgUtils")
```

## Feedback and Contributions
Suggestions and contributions are welcome. For any proposed additions, amendments, or feedback, please
[create an issue](https://github.com/sarahcgall/scgUtils/issues).

## Related Packages
Check out [`scgElectionsNZ`](https://sarahcgall.github.io/scgElectionsNZ) for additional datasets relevant for 
NZ General Elections.
