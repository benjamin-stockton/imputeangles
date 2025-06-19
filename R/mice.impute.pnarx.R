#' Impute with Projected Normal AR with Exogenous Predictors
#'
#' @param y angles
#' @param ry response indicators
#' @param x predictors
#' @param ... other arguments to pass to fit_pn_arx_model()
#'
#' @return A numeric vector of length n_mis.
#' @export
#'
#' @examples
#' ## Not run:
#' N <- 100
#' x <- rnorm(N)
#' B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
#' y <- bpnreg_draw(x, B)
#' mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
#' y[mis] <- NA
#' ry <- !is.na(y)
#'
#' mice.impute.pnarx(y, ry, x)
#' ## End(Not run)
mice.impute.pnarx <- function(y, ry, x, ...) {
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
    # invisible(
    #     utils::capture.output(
    fit <- pnregstan::fit_pn_arx_model(theta = y[ry],
                                          X = X_mat,
                                          X_ppd = X_ppd_mat,
                                          refresh = 0,
                                       show_messages = FALSE,
                                       ...)
    #     )
    # )

    theta_ppd <- as.matrix(posterior::as_draws_df(fit$draws(variables = "theta_ppd")))

    N_ppd <- sum(!ry)
    theta_imp <- theta_ppd[nrow(theta_ppd), 1:N_ppd]

    return(theta_imp)
}
