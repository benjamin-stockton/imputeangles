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

#' Construct the Data Frame for Modeling
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
#' (construct_modeling_data(y, ry, x))
construct_modeling_data <- function(y, ry, x) {
    if (is.vector(x)) {
        dat <- as.data.frame(cbind(y[ry], x[ry]))
    } else if (is.matrix(x) | is.data.frame(x)) {
        dat <- as.data.frame(cbind(y[ry], x[ry,]))
    }

    dat$theta <- convert2MinusPiPlusPi(dat[,1])
    dat <- dat[,-1]
    return(dat)
}

