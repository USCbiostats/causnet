
<!-- README.md is generated from README.Rmd. Please edit that file -->

# causnet

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/USCbiostats/causnet.svg?branch=master)](https://travis-ci.org/USCbiostats/causnet)
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
#> 
#> $mutliple_networks
#> $mutliple_networks$bnsets
#> <environment: 0x7f9fa41cfcd8>
#> attr(,"m")
#> [1] 5
#> attr(,"length")
#> [1] 625
#> attr(,"index")
#> [1] 6
#> 
#> $mutliple_networks$possible_parents
#> $mutliple_networks$possible_parents[[1]]
#> [1] 2 3
#> 
#> $mutliple_networks$possible_parents[[2]]
#> [1] 1 3
#> 
#> $mutliple_networks$possible_parents[[3]]
#> [1] 1 2 4 5
#> 
#> $mutliple_networks$possible_parents[[4]]
#> [1] 3 5
#> 
#> $mutliple_networks$possible_parents[[5]]
#> [1] 3 4
#> 
#> 
#> $mutliple_networks$possible_parent_sets
#> $mutliple_networks$possible_parent_sets[[1]]
#> $mutliple_networks$possible_parent_sets[[1]][[1]]
#> [1] 2
#> 
#> $mutliple_networks$possible_parent_sets[[1]][[2]]
#> [1] 3
#> 
#> $mutliple_networks$possible_parent_sets[[1]][[3]]
#> [1] 2 3
#> 
#> 
#> $mutliple_networks$possible_parent_sets[[2]]
#> $mutliple_networks$possible_parent_sets[[2]][[1]]
#> [1] 1
#> 
#> $mutliple_networks$possible_parent_sets[[2]][[2]]
#> [1] 3
#> 
#> $mutliple_networks$possible_parent_sets[[2]][[3]]
#> [1] 1 3
#> 
#> 
#> $mutliple_networks$possible_parent_sets[[3]]
#> $mutliple_networks$possible_parent_sets[[3]][[1]]
#> [1] 1
#> 
#> $mutliple_networks$possible_parent_sets[[3]][[2]]
#> [1] 2
#> 
#> $mutliple_networks$possible_parent_sets[[3]][[3]]
#> [1] 4
#> 
#> $mutliple_networks$possible_parent_sets[[3]][[4]]
#> [1] 5
#> 
#> $mutliple_networks$possible_parent_sets[[3]][[5]]
#> [1] 1 2
#> 
#> $mutliple_networks$possible_parent_sets[[3]][[6]]
#> [1] 1 4
#> 
#> $mutliple_networks$possible_parent_sets[[3]][[7]]
#> [1] 1 5
#> 
#> $mutliple_networks$possible_parent_sets[[3]][[8]]
#> [1] 2 4
#> 
#> $mutliple_networks$possible_parent_sets[[3]][[9]]
#> [1] 2 5
#> 
#> $mutliple_networks$possible_parent_sets[[3]][[10]]
#> [1] 4 5
#> 
#> 
#> $mutliple_networks$possible_parent_sets[[4]]
#> $mutliple_networks$possible_parent_sets[[4]][[1]]
#> [1] 3
#> 
#> $mutliple_networks$possible_parent_sets[[4]][[2]]
#> [1] 5
#> 
#> $mutliple_networks$possible_parent_sets[[4]][[3]]
#> [1] 3 5
#> 
#> 
#> $mutliple_networks$possible_parent_sets[[5]]
#> $mutliple_networks$possible_parent_sets[[5]][[1]]
#> [1] 3
#> 
#> $mutliple_networks$possible_parent_sets[[5]][[2]]
#> [1] 4
#> 
#> $mutliple_networks$possible_parent_sets[[5]][[3]]
#> [1] 3 4
#> 
#> 
#> 
#> $mutliple_networks$bps1
#> $mutliple_networks$bps1[[1]]
#> $mutliple_networks$bps1[[1]][[1]]
#> $mutliple_networks$bps1[[1]][[1]][[1]]
#> [1] 2
#> 
#> $mutliple_networks$bps1[[1]][[1]][[2]]
#> NULL
#> 
#> $mutliple_networks$bps1[[1]][[1]][[3]]
#> [1] 2
#> 
#> 
#> $mutliple_networks$bps1[[1]][[2]]
#> $mutliple_networks$bps1[[1]][[2]][[1]]
#> [1] 1
#> 
#> $mutliple_networks$bps1[[1]][[2]][[2]]
#> [1] 3
#> 
#> $mutliple_networks$bps1[[1]][[2]][[3]]
#> [1] 1 3
#> 
#> 
#> $mutliple_networks$bps1[[1]][[3]]
#> $mutliple_networks$bps1[[1]][[3]][[1]]
#> NULL
#> 
#> $mutliple_networks$bps1[[1]][[3]][[2]]
#> [1] 2
#> 
#> $mutliple_networks$bps1[[1]][[3]][[3]]
#> [1] 4
#> 
#> $mutliple_networks$bps1[[1]][[3]][[4]]
#> NULL
#> 
#> $mutliple_networks$bps1[[1]][[3]][[5]]
#> [1] 2
#> 
#> $mutliple_networks$bps1[[1]][[3]][[6]]
#> [1] 4
#> 
#> $mutliple_networks$bps1[[1]][[3]][[7]]
#> NULL
#> 
#> $mutliple_networks$bps1[[1]][[3]][[8]]
#> [1] 2 4
#> 
#> $mutliple_networks$bps1[[1]][[3]][[9]]
#> [1] 2
#> 
#> $mutliple_networks$bps1[[1]][[3]][[10]]
#> [1] 4
#> 
#> 
#> $mutliple_networks$bps1[[1]][[4]]
#> $mutliple_networks$bps1[[1]][[4]][[1]]
#> [1] 3
#> 
#> $mutliple_networks$bps1[[1]][[4]][[2]]
#> [1] 5
#> 
#> $mutliple_networks$bps1[[1]][[4]][[3]]
#> [1] 3 5
#> 
#> 
#> $mutliple_networks$bps1[[1]][[5]]
#> $mutliple_networks$bps1[[1]][[5]][[1]]
#> NULL
#> 
#> $mutliple_networks$bps1[[1]][[5]][[2]]
#> [1] 4
#> 
#> $mutliple_networks$bps1[[1]][[5]][[3]]
#> [1] 4
#> 
#> 
#> 
#> $mutliple_networks$bps1[[2]]
#> $mutliple_networks$bps1[[2]][[1]]
#> $mutliple_networks$bps1[[2]][[1]][[1]]
#> [1] -416.8063
#> 
#> $mutliple_networks$bps1[[2]][[1]][[2]]
#> [1] -430.8845
#> 
#> $mutliple_networks$bps1[[2]][[1]][[3]]
#> [1] -416.8063
#> 
#> 
#> $mutliple_networks$bps1[[2]][[2]]
#> $mutliple_networks$bps1[[2]][[2]][[1]]
#> [1] -416.8063
#> 
#> $mutliple_networks$bps1[[2]][[2]][[2]]
#> [1] -415.074
#> 
#> $mutliple_networks$bps1[[2]][[2]][[3]]
#> [1] -403.4732
#> 
#> 
#> $mutliple_networks$bps1[[2]][[3]]
#> $mutliple_networks$bps1[[2]][[3]][[1]]
#> [1] -430.8845
#> 
#> $mutliple_networks$bps1[[2]][[3]][[2]]
#> [1] -415.074
#> 
#> $mutliple_networks$bps1[[2]][[3]][[3]]
#> [1] -422.6073
#> 
#> $mutliple_networks$bps1[[2]][[3]][[4]]
#> [1] -430.8845
#> 
#> $mutliple_networks$bps1[[2]][[3]][[5]]
#> [1] -415.074
#> 
#> $mutliple_networks$bps1[[2]][[3]][[6]]
#> [1] -422.6073
#> 
#> $mutliple_networks$bps1[[2]][[3]][[7]]
#> [1] -430.8845
#> 
#> $mutliple_networks$bps1[[2]][[3]][[8]]
#> [1] -408.2938
#> 
#> $mutliple_networks$bps1[[2]][[3]][[9]]
#> [1] -415.074
#> 
#> $mutliple_networks$bps1[[2]][[3]][[10]]
#> [1] -422.6073
#> 
#> 
#> $mutliple_networks$bps1[[2]][[4]]
#> $mutliple_networks$bps1[[2]][[4]][[1]]
#> [1] -422.6073
#> 
#> $mutliple_networks$bps1[[2]][[4]][[2]]
#> [1] -414.4274
#> 
#> $mutliple_networks$bps1[[2]][[4]][[3]]
#> [1] -408.4039
#> 
#> 
#> $mutliple_networks$bps1[[2]][[5]]
#> $mutliple_networks$bps1[[2]][[5]][[1]]
#> [1] -430.8845
#> 
#> $mutliple_networks$bps1[[2]][[5]][[2]]
#> [1] -414.4274
#> 
#> $mutliple_networks$bps1[[2]][[5]][[3]]
#> [1] -414.4274
#> 
#> 
#> 
#> 
#> $mutliple_networks$max_parents
#> [1] 2
```

## Code of Conduct

Please note that the causnet project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/1/0/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
