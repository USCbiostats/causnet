
<!-- README.md is generated from README.Rmd. Please edit that file -->

# causnet

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/USCbiostats/causnet/branch/master/graph/badge.svg)](https://codecov.io/gh/USCbiostats/causnet?branch=master)
[![R build
status](https://github.com/USCbiostats/causnet/workflows/R-CMD-check/badge.svg)](https://github.com/USCbiostats/causnet/actions)
<!-- badges: end -->

The goal of causnet is to find a globally optimal causal network given
some data.

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
mydata <- simdat(n_var = 5)

# causnet results
links.s <- causnet(mydata)

links.s
#> $network
#>   from to component
#> 1    4  5         1
#> 2    3  4         1
#> 3    2  3         1
#> 4    1  2         1
#> 5    4  5         2
#> 6    3  4         2
#> 
#> $n_best_parents
#> [1] 1 1 1 1 1 1
```

### using BGE scoring function

To use he a BGE scoring function simply pass thr `score_bge()` to the
`score_fun` argument.

``` r
causnet(mydata, score_fun = score_bge)
#> $network
#>   from to component
#> 1    3  5         1
#> 2    3  4         1
#> 3    1  3         1
#> 4    1  2         1
#> 5    4  5         2
#> 6    3  4         2
#> 
#> $n_best_parents
#> [1] 2 1 2 1 2 1
```

## Code of Conduct

Please note that the causnet project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/1/0/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
