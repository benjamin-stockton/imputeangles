#' Impute Incomplete Angular Data with von Mises Regression (brms)
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
#' mice.impute.vmbrrms(y, ry, x)
mice.impute.vmbrms <- function(y, ry, x,...) {
    if (sum(ry) == length(y)) {
        stop("No missing data! :)")
    }
    if (is.vector(x)) {
        X_mat <- matrix(x[ry], nrow = sum(ry))
        X_ppd_mat <- matrix(x[!ry], nrow = sum(!ry))
    }
    else if (is.matrix(x) || is.data.frame(x)) {
        X_mat <- as.matrix(x[ry,], nrow = sum(ry))
        if (sum(!ry) == 1) {
            X_ppd_mat <- t(as.matrix(x[!ry,], nrow = sum(!ry), ncol = ncol(x)))
        }
        else {
            X_ppd_mat <- as.matrix(x[!ry,], nrow = sum(!ry), ncol = ncol(x))
        }
    }
    invisible(
        utils::capture.output(
    fit <- pnregstan::fit_vm_brms_model(theta = y[ry],
                                       X = X_mat,
                                       X_ppd = X_ppd_mat,
                                       refresh = 0,
                                       show_messages = FALSE,
                                       show_exceptions = FALSE)
        )
    )

    theta_ppd <- as.matrix(posterior::as_draws_df(fit$draws(variables = "theta_ppd")))

    N_ppd <- sum(!ry)
    theta_imp <- theta_ppd[nrow(theta_ppd), 1:N_ppd]

    return(theta_imp)
}
