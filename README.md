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
- **Custom Visualisations**: Bring your data to life with various plotting functions, from [`plot_parliament()`](https://sarahcgall.github.io/scgUtils/reference/plot_parliament.html) for election outcomes to [`plot_sankey()`](https://sarahcgall.github.io/scgUtils/reference/plot_sankey.html) for flow data representation.
- **Styling and Brand Consistency**: Maintain brand integrity with [`colour_pal()`](https://sarahcgall.github.io/scgUtils/reference/colour_pal.html) and [`theme_scg()`](https://sarahcgall.github.io/scgUtils/reference/theme_scg.html), ensuring consistent and appealing data presentations.

### Installation

Install the development version of `scgUtils` directly from GitHub:

``` r
devtools::install_github("sarahcgall/scgUtils")
```

## Usage
Here's a quick example of using `scgUtils` to analyse and visualise survey data:

```r
# Load sample data
df <- get_file("inst/extdata/survey.sav")

# Explore data with a crosstab
crosstab(data = df,
         rowVar = "p_eurefvote",
         colVar = "p_edlevel",
         weight = "wt",
         format = "df_wide",
         round_decimals = 2,
         statistics = TRUE,
         plot = TRUE
)
# [1] "p_eurefvote x p_edlevel: Chisq = 371.026 | DF = 10 | Cramer's V = 0.243 | p-value = 0"
#         p_eurefvote Undergraduate A-level Below GCSE  GCSE No qualifications Postgrad Total
# 1 I voted to remain         61.23   40.58      27.47 30.60             23.49    73.47 47.92
# 2  I voted to leave         38.25   59.12      72.53 69.13             76.51    26.42 51.78
# 3        Don't know          0.52    0.30       0.00  0.27              0.00     0.10  0.31
```
![](man/figures/crosstab_example.png)

## Feedback and Contributions
Suggestions and contributions are welcome. For any proposed additions, amendments, or feedback, please
[create an issue](https://github.com/sarahcgall/scgUtils/issues).

## Related Packages
Check out [`scgElectionsNZ`](https://sarahcgall.github.io/scgElectionsNZ) for additional datasets relevant for 
NZ General Elections.
