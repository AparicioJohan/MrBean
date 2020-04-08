# MrBean <img src="man/figures/logo.png" width="120px" align="right"/>

Is a web application for simplifying the analysis of large-scale plant breeding experiments using spatial analysis, as implemented in the R-package `SpATS` (Rodriguez-Alvarez et al. 2018) . It provides the possibility for visualizing raw data, indentifying outliers, fitting mixed linear models (MLM) with or without spatial correction, retrieve BLUPs/BLUEs and heritabilites from single-environment trials. 

## Installation

``` r
devtools::install_github("AparicioJohan/MrBean")
```
or ...

``` r
source("https://install-github.me/AparicioJohan/MrBean")
```

``` r
library(MrBean)
```


## Run the Shiny app

``` r
mrBean()
```

