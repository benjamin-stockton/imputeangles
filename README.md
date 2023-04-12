
<!-- README.md is generated from README.Rmd. Please edit that file -->

# imputeangles

<!-- badges: start -->
<!-- badges: end -->

The goal of imputeangles is to provide imputation methods for incomplete
angular data for use with the `mice` package.

## Installation

You can install the development version of imputeangles from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("benjamin-stockton/imputeangles")
```

## Example

As an example of stand-alone use (i.e. without the `mice` package), we
construct a simulated data set with missing angles and impute the
missing angles with projected normal regression.

``` r
library(imputeangles)

N <- 100
x <- rnorm(N)
B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
y <- pnreg_draw(x, B)
mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
y[mis] <- NA
ry <- !is.na(y)

mice.impute.bpnreg(y, ry, x)
#>  [1]  2.023566875  0.207978405  0.448343933 -0.006843422  0.527886315
#>  [6]  0.397138167  0.218616227  0.219616628  0.275926839  0.086872207
#> [11]  0.108079475 -0.598516168  0.422440555  0.138097665  0.073659630
#> [16]  0.382205021 -0.028086712  0.084030305  0.412175245  1.976935211
#> [21]  0.108362910  0.203089151  0.608881309  0.153599343  0.321939535
#> [26]  0.087793539  0.299027309 -0.017373376  0.173728909 -0.013963065
#> [31]  0.512879017  0.332326930  0.519211474  1.842809679 -0.070611426
#> [36]  0.276584662  2.647033281 -0.390190002  0.218481549  3.120744722
#> [41]  0.294276032  0.032664784  0.071610318  0.333768952  0.268626414
#> [46]  0.105351933  0.118026008  0.340359923  0.616408449 -0.029093292
```

For use with `mice`, the package should be loaded and the functions
called as follows:

``` r
library(mice)
library(imputeangles)

N <- 100
x <- rnorm(N)
B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
y <- pnreg_draw(x, B)
mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
y[mis] <- NA
dat <- data.frame("theta" = y, 
                  "X" = x)
                  
imps <- mice(dat, method = c("bpnreg", "norm"), m = 5)
```
