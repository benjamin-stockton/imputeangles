
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
y <- bpnreg_draw(x, B)
mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
y[mis] <- NA
ry <- !is.na(y)

mice.impute.bpnreg(y, ry, x)
#>  [1]  0.0544961020  0.4057977634  0.4417986320  2.6248698724 -0.0349158443
#>  [6] -0.1294799844  0.1093084004  1.8990424176  1.3220155221  0.2349145377
#> [11]  0.6626166956  0.1217145631  0.1931887847  0.0152833940  0.1773839240
#> [16] -0.1092635226  0.0185351675  0.1143577683  1.4168956016 -0.9678250118
#> [21]  2.7292163201 -3.0081450186  0.0683055867  0.3816707621  0.0476781058
#> [26]  0.1642969959  2.9163900182  0.0410910996  1.1447296324  0.1222311727
#> [31] -0.1199470121 -0.0002901086 -0.8966503153  2.6133768588 -2.4208888052
#> [36]  0.2140773084  0.9479779529  0.0616249511 -0.0058890625  0.3611101524
#> [41]  2.0984682283  1.2456913199  0.0743370682  1.1637459246  0.0318694504
#> [46]  0.7791783474  0.1545595812  0.0287298394 -1.4060909593  1.1077332961
```

For use with `mice`, the package should be loaded and the functions
called as follows:

``` r
library(mice)
library(imputeangles)

N <- 100
x <- rnorm(N)
B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
y <- bpnreg_draw(x, B)
mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
y[mis] <- NA
dat <- data.frame("theta" = y, 
                  "X" = x)
                  
imps <- mice(dat, method = c("bpnreg", "norm"), m = 5)
```
