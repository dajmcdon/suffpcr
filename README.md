
# suffpcr

<!-- badges: start -->
[![R-CMD-check](https://github.com/dajmcdon/suffpcr/workflows/R-CMD-check/badge.svg)](https://github.com/dajmcdon/suffpcr/actions)
<!-- badges: end -->

The goal of suffpcr is to estimate sufficient principal component regression
by implementing row-sparse PCA followed by ordinary least squares or logistic
regression.

## Installation

You can install the package with

``` r
remotes::install_github("dajmcdon/suffpcr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(suffpcr)
n <- 100
p <- 50
U <- rnorm(n)
V <- c(rnorm(5), rep(0, p - 5))
V <- V / sqrt(sum(V^2))
bstar <- V * (5 / (5.1))
X <- 5 * tcrossprod(U, V) + 0.1 * matrix(rnorm(n * p), n)
y <- U + rnorm(n)
out <- suffPCR(X, y, d=1:3)
```

