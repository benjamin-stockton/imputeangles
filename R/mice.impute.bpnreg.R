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
#' X <- rnorm(N)
#' theta <- numeric(N)
#' for (i in 1:N) {
#'     B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = T)
#'     mu_i <- c(1, X[i]) %*% B
#'     Y <- mvtnorm::rmvnorm(N, mu_i, diag(2))
#'     U <- Y / sqrt(sum(Y^2))
#'     theta[i] <- atan2(U[2], U[1])
#' }
#' mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
#' theta[mis] <- NA
#' ry <- !is.na(theta)
#' X1 <- cbind(rep(1, N), X)
#'
#' mice.impute.bpnreg(theta, ry, X1)
mice.impute.bpnreg <- function(y, ry, x,...) {

    dat <- as.data.frame(cbind(y[ry], x[ry,]))
    dat$theta <- as.numeric(circular::minusPiPlusPi(circular::circular(dat[,1])))
    dat <- dat[,-1]

    invisible(utils::capture.output(fit <- bpnreg::bpnr(theta ~ ., data = dat,
                                                 its = 2000, burn = 1000)))

    b1 <- fit$beta1; b2 <- fit$beta2

    x_tilde <- x[!ry,]
    x_tilde <- cbind(rep(1, nrow(x_tilde)), x_tilde)
    theta_ppd <- matrix(NA, nrow = nrow(b1), ncol = nrow(x_tilde))
    for (i in 1:nrow(b1)) {
        mu_1 <- x_tilde %*% b1[i,]
        mu_2 <- x_tilde %*% b2[i,]
        y_1 <- stats::rnorm(ncol(theta_ppd), mu_1, 1)
        y_2 <- stats::rnorm(ncol(theta_ppd), mu_2, 1)
        ssq <- sqrt(y_1^2 + y_2^2)
        U_1 <- y_1 / ssq
        U_2 <- y_2 / ssq

        theta_ppd[i,] <- atan2(U_2, U_1) # %% (2*pi)
    }

    theta_imp <- as.numeric(theta_ppd[sample(nrow(theta_ppd), size = 1),])

    return(theta_imp)
}
