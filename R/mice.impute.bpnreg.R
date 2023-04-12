#' Impute with Projected Normal Regression
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
#' y <- pnreg_draw(x, B)
#' mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
#' y[mis] <- NA
#' ry <- !is.na(y)
#'
#' mice.impute.bpnreg(y, ry, x)
mice.impute.bpnreg <- function(y, ry, x,...) {

    dat <- construct_data_bpnreg(y, ry, x)
    invisible(
        utils::capture.output(
            fit <- bpnreg::bpnr(theta ~ .,
                                data = dat,
                                its = 2000,
                                burn = 1000)
            )
        )

    B <- get_posterior_draws_pnreg(fit)

    if (is.vector(x)) {
        x_tilde <- x[!ry]

    } else if (is.matrix(x) | is.data.frame(x)) {
        x_tilde <- x[!ry,]
    }
    theta_imp <- pnreg_draw(x_tilde, B)

    return(theta_imp)
}


#' Convert Angle to Minus Pi Plus Pi Interval
#'
#' @param theta A numeric vector of angular values.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' theta <- seq(0, 2*pi, length.out = 12)
#' (convert2MinusPiPlusPi(theta))
convert2MinusPiPlusPi <- function(theta) {
    theta <- theta %% (2*pi)
    phi <- ifelse(theta >= 0 & theta < pi, theta, theta - (2 * pi))
    return(phi)
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
#' (pnreg_draw(x, B))
pnreg_draw <- function(x, B) {
    N <- 0
    if (is.vector(x) & is.numeric(x)) {
        N <- length(x)
    } else if (is.matrix(x) & is.numeric(x)) {
        N <- nrow(x)
    } else if (is.data.frame(x)) {
        N <- nrow(x)
        x <- stats::model.matrix(~ ., x)[,-1]
    } else {

    }
    if (ncol(B) != 2) {

    }
    ones <- rep(1, N)
    x <- cbind(ones, x)
    mu <- x %*% B
    y_1 <- stats::rnorm(N, mu[,1], 1)
    y_2 <- stats::rnorm(N, mu[,2], 1)
    ssq <- sqrt(y_1^2 + y_2^2)
    U_1 <- y_1 / ssq
    U_2 <- y_2 / ssq
    theta <- atan2(U_2, U_1)
    return(theta)
}

#' Construct the Data Frame for bpnreg
#'
#' @param y A numeric vector of incomplete angles.
#' @param ry A logical
#' @param x A numeric matrix of observed covariates
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' N <- 100
#' X <- rnorm(N)
#' B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
#' y <- pnreg_draw(X, B)
#' mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
#' y[mis] <- NA
#' ry <- !is.na(y)
#' x <- cbind(rep(1, N), X)
#' (construct_data_bpnreg(y, ry, x))
construct_data_bpnreg <- function(y, ry, x) {
    if (is.vector(x)) {
        dat <- as.data.frame(cbind(y[ry], x[ry]))
    } else if (is.matrix(x) | is.data.frame(x)) {
        dat <- as.data.frame(cbind(y[ry], x[ry,]))
    }

    dat$theta <- convert2MinusPiPlusPi(dat[,1])
    dat <- dat[,-1]
    return(dat)
}

#' Collect Posterior Draws for Coefficients
#'
#' @param fit A fitted object from bpnreg package.
#'
#' @return A numeric matrix size k x 2 of regression coefficients
#' @export
#'
#' @examples
#' N <- 100
#' x <- rnorm(N)
#' B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
#' y <- pnreg_draw(x, B)
#' ry <- !is.na(y)
#' dat <- construct_data_bpnreg(y, ry, x)
#' invisible(
#'     utils::capture.output(
#'         fit <- bpnreg::bpnr(theta ~ ., data = dat, its = 2000, burn = 1000)
#'         )
#'     )
#' (get_posterior_draws_pnreg(fit))
get_posterior_draws_pnreg <- function(fit) {
    b1 <- fit$beta1; b2 <- fit$beta2
    s <- sample(nrow(b1), size = 1)
    B <- cbind(b1[s,], b2[s,])
    return(B)
}
