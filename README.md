# MrBean <img src="man/figures/logo.png" width="120px" align="right"/>

Is a web application for simplifying the analysis of large-scale plant breeding experiments using spatial analysis, as implemented in the R-package `SpATS` (Rodriguez-Alvarez et al. 2018) . It provides the possibility for visualizing raw data, indentifying outliers, fitting mixed linear models (MLM) with or without spatial correction, retrieve BLUPs/BLUEs and heritabilites from single-environment trials. 

## Installation

If you do not have installed `devtools` package, first it is necessary :


``` r
install.packages("devtools")
```

and then

``` r
devtools::install_github("AparicioJohan/MrBean")
```


``` r
library(MrBean)
```


## Run the Shiny app

``` r
app_MrBean()
```

## Demo

A running demo is on [shinyapps.io](https://beanteam.shinyapps.io/MrBean_v01/).


<div class="row">
<div class="card">
<a href="#" target="_blank"><img src="man/figures/background.PNG"></a>
</div>
</div>


