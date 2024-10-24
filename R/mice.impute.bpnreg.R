#' Impute Incomplete Angular Data with Projected Normal Regression
#'
#' @param y The incomplete vector of N angles to be imputed.
#' @param ry A logical vector of length N indicating whether the ith observation is observed (TRUE) or missing (FALSE).
#' @param x Completely observed covariates with dimension N x k.
#' @param ... Additional parameters (unused)
#'
#' @return A numeric vector of length n_mis.
#' @export
#'
#' @examples
#' N <- 100
#' x <- rnorm(N)
#' B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
#' y <- bpnreg_draw(x, B)
#' mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
#' y[mis] <- NA
#' ry <- !is.na(y)
#'
#' mice.impute.bpnreg(y, ry, x)
mice.impute.bpnreg <- function(y, ry, x, ...) {
  if (sum(ry) == length(y)) {
    stop("No missing data! :)")
  }

  dat <- construct_modeling_data(y, ry, x)
  invisible(
    utils::capture.output(
      fit <- bpnreg::bpnr(theta ~ .,
        data = dat,
        its = 2000,
        burn = 1000
      )
    )
  )

  B <- posterior_draw_bpnreg(fit)

  theta_imp <- ppd_draws_bpnreg(B, ry, x)

  return(theta_imp)
}

#' Draw from Projected Normal Regression
#'
#' @param x A numeric matrix size N x k of covariates. (No leading 1s column.)
#' @param B A numeric matrix size k x 2 of regression coefficients.
#'
#' @return theta A numeric vector size N.
#' @export
#'
#' @examples
#' x <- rnorm(25)
#' B <- matrix(c(1, 5, 0, -2), byrow = TRUE, nrow = 2)
#' (bpnreg_draw(x, B))
bpnreg_draw <- function(x, B) {
  N <- 0
  if (ncol(B) != 2) {
    stop("Coefficient matrix B doesn't have 2 columns!")
  }
  if (is.vector(x) && is.numeric(x) && nrow(B) == 2) {
    # Single Predictor with >= 1 observation
      N <- length(x)
      x <- cbind(rep(1, N), x)
  } else if (is.vector(x) && is.numeric(x) && nrow(B) > 2) {
    # Multiple Predictor with 1 observation
      N <- 1
      x <- matrix(c(1, x), nrow = 1)
  } else if (is.matrix(x) && is.numeric(x)) {
    # Multiple Predictors and observations in a matrix
      N <- nrow(x)
      x <- cbind(rep(1, N), x)
  } else if (is.data.frame(x)) {
    # Multiple Predictors and observations in a dataframe
      N <- nrow(x)
      x <- stats::model.matrix(~., data = x)[, -1]
      x <- cbind(rep(1, N), x)
  } else {
      stop("Data is not numeric!")
  }
  mu <- x %*% B
  y_1 <- stats::rnorm(N, mu[, 1], 1)
  y_2 <- stats::rnorm(N, mu[, 2], 1)
  ssq <- sqrt(y_1^2 + y_2^2)
  U_1 <- y_1 / ssq
  U_2 <- y_2 / ssq
  theta <- atan2(U_2, U_1)
  return(theta)
}

#' Collect Posterior Draws for Coefficients
#'
#' @param fit A fitted object from bpnreg package.
#'
#' @return B A numeric matrix size k x 2 of regression coefficients
#' @export
#'
#' @examples
#' N <- 100
#' x <- rnorm(N)
#' B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
#' y <- bpnreg_draw(x, B)
#' ry <- !is.na(y)
#' dat <- construct_modeling_data(y, ry, x)
#' invisible(
#'   utils::capture.output(
#'     fit <- bpnreg::bpnr(theta ~ ., data = dat, its = 2000, burn = 1000)
#'   )
#' )
#' (posterior_draw_bpnreg(fit))
posterior_draw_bpnreg <- function(fit) {
  b1 <- fit$beta1
  b2 <- fit$beta2
  rand_s <- sample(nrow(b1), size = 1)
  B <- cbind(b1[rand_s, ], b2[rand_s, ])
  return(B)
}

#' Collect a Posterior Predictive Draw of Projected Normal Regression
#'
#' @param B A numeric matrix size k x 2 of regression coefficients
#' @param ry A logical vector of length N indicating whether the ith observation is observed (TRUE) or missing (FALSE).
#' @param x Completely observed covariates with dimension N x k.
#'
#' @return theta_ppd A numeric vector of size n_mis.
#' @export
#'
#' @examples
#' N <- 100
#' x <- rnorm(N)
#' B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
#' y <- bpnreg_draw(x, B)
#' mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
#' y[mis] <- NA
#' ry <- !is.na(y)
#' dat <- construct_modeling_data(y, ry, x)
#' invisible(
#'   utils::capture.output(
#'     fit <- bpnreg::bpnr(theta ~ ., data = dat, its = 2000, burn = 1000)
#'   )
#' )
#' B <- posterior_draw_bpnreg(fit)
#' ppd_draws_bpnreg(B, ry, x)
ppd_draws_bpnreg <- function(B, ry, x) {
  if (is.vector(x)) {
    x_tilde <- x[!ry]
  } else if (is.matrix(x) | is.data.frame(x)) {
    x_tilde <- x[!ry, ]
  }
  theta_ppd <- bpnreg_draw(x_tilde, B)
  return(theta_ppd)
}
