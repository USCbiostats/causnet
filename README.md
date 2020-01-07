
<!-- README.md is generated from README.Rmd. Please edit that file -->

# causnet

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/USCbiostats/causnet.svg?branch=master)](https://travis-ci.org/USCbiostats/causnet)
[![Codecov test
coverage](https://codecov.io/gh/USCbiostats/causnet/branch/master/graph/badge.svg)](https://codecov.io/gh/USCbiostats/causnet?branch=master)
<!-- badges: end -->

The goal of causnet is to …

## Installation

You can install the development version from GitHub with:

``` r
require("devtools")
install_github("USCbiostats/causnet")
```

\~You can install the released version of causnet from
[CRAN](https://CRAN.R-project.org) with:\~

``` r
install.packages("causnet")
```

## Example

``` r
library(causnet)

# simulate data
set.seed(1234)
mydata = simdat(n_var = 5)

# causnet results
links.s = causnet(mydata)

links.s
#> $network
#>   from to component
#> 1    4  5         1
#> 2    3  4         1
#> 3    2  1         1
#> 4    2  3         1
#> 
#> $n_best_parents
#> [1] 1 1 1 1
```

## Code of Conduct

Please note that the causnet project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/1/0/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
