% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mice.impute.bpnreg.R
\name{ppd_draws_bpnreg}
\alias{ppd_draws_bpnreg}
\title{Collect a Posterior Predictive Draw of Projected Normal Regression}
\usage{
ppd_draws_bpnreg(B, ry, x)
}
\arguments{
\item{B}{A numeric matrix size k x 2 of regression coefficients}

\item{ry}{A logical vector of length N indicating whether the ith observation is observed (TRUE) or missing (FALSE).}

\item{x}{Completely observed covariates with dimension N x k.}
}
\value{
theta_ppd A numeric vector of size n_mis.
}
\description{
Collect a Posterior Predictive Draw of Projected Normal Regression
}
\examples{
N <- 100
x <- rnorm(N)
B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
y <- bpnreg_draw(x, B)
mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
y[mis] <- NA
ry <- !is.na(y)
dat <- construct_modeling_data(y, ry, x)
invisible(
  utils::capture.output(
    fit <- bpnreg::bpnr(theta ~ ., data = dat, its = 2000, burn = 1000)
  )
)
B <- posterior_draw_bpnreg(fit)
ppd_draws_bpnreg(B, ry, x)
}
