
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

~You can install the released version of causnet from
[CRAN](https://CRAN.R-project.org) with:~

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
#> 
#> $n_best_parents
#> [1] 1 1 1 1
```

### using BGE scoring function

To use the BGe scoring function, pass `score_bge()` to the `score_fun`
argument.

``` r
causnet(mydata, score_fun = score_bge)
#> $network
#>   from to component
#> 1    4  5         1
#> 2    3  4         1
#> 3    2  3         1
#> 4    1  2         1
#> 
#> $n_best_parents
#> [1] 1 1 1 1
```

### Candidate parents by stability selection

`causnet_stable()` replaces the p-value threshold `alpha` of `causnet()`
with stability selection (Meinshausen and Buhlmann, 2010). A variable is
a candidate parent if its selection frequency over `B` subsamples of
half the observations is at least `pi_thr`. The number of variables
selected in each subsample is derived from `tau`, a per-node bound on
the per-family error rate (PFER) under the assumptions of Theorem 1 of
Meinshausen and Buhlmann (2010): the expected number of falsely selected
candidate parents of each node (each node is screened as its own family
of p - 1 candidates). Summed over the screened nodes, the expected total
number of falsely selected candidate parents (not edges of the final
network) is at most (number of screened nodes) x `tau`. See
`?causnet_stable` for the conditions of these bounds. Use `seed` for
reproducible subsamples.

``` r
causnet_stable(mydata, tau = 1, pi_thr = 0.75, B = 100, seed = 1)
#> [stability selection] n=300, p=5, q=1, pi_thr=0.75, tau=1.00, B=100
#> $network
#>   from to component
#> 1    2  3         1
#> 2    2  1         1
#> 3    4  5         2
#> 
#> $n_best_parents
#> [1] 1 1 1
```

### Phenotype-driven search

`causnet()` and `causnet_stable()` can restrict the search to the
neighbourhood of an outcome variable. Give `outcome` as a column index
or name: starting from the outcome, candidate-parent links are followed
for at most `n_levels` hops (default 3), and the network is learned on
the variables reached this way only. With `outcome = NULL` (the default)
the whole graph is searched. Here the search only uses `V5` and its
candidate parents:

``` r
causnet(mydata, outcome = "V5", n_levels = 1)
#> $network
#>   from to component
#> 1    4  5         1
#> 2    3  4         1
#> 
#> $n_best_parents
#> [1] 1 1
```

## Code of Conduct

Please note that the causnet project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/1/0/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
