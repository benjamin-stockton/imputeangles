#' Impute with Projected Normal AR with Exogenous Predictors (Identity Covariance) and Internally Imputed Angles
#'
#' @description
#' The projected normal AR with exogenous predictors and identity covariance is
#' with missing angular observations imputed during the sampling procedure. This
#' integrates out the missingness from the model fitting so if missingness is only
#' on theta, then only one cycle should be necessary.
#'
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
#' \dontrun{
#' N <- 100
#' x <- rnorm(N)
#' B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
#' y <- bpnreg_draw(x, B)
#' mis <- sample(N, size = floor(0.25 * N), replace = FALSE)
#' y[mis] <- NA
#' ry <- !is.na(y)
#'
#' mice.impute.pnarxid(y, ry, x)
#' }
#' ## End(Not run)
mice.impute.pnarxinc <- function(y, ry, x, ...) {
    if (sum(ry) == length(y)) {
        stop("No missing data! :)")
    }
    if (is.vector(x)) {
        X_mat <- matrix(x, nrow = length(y))
        # X_ppd_mat <- matrix(x[!ry], nrow = sum(!ry))
    }
    else if (is.matrix(x) || is.data.frame(x)) {
        X_mat <- as.matrix(x, nrow = length(y))
        # if (sum(!ry) == 1) {
        #     X_ppd_mat <- t(as.matrix(x[!ry,], nrow = sum(!ry), ncol = ncol(x)))
        # }
        # else {
        #     X_ppd_mat <- as.matrix(x[!ry,], nrow = sum(!ry), ncol = ncol(x))
        # }
    }
    invisible(
        utils::capture.output(
            fit <- pnregstan::fit_pn_arx_id_inc_model(theta = y,
                                                  X = X_mat,
                                                  X_ppd = X_mat,
                                                  refresh = 1000,
                                                  show_messages = FALSE,
                                                  show_exceptions = FALSE)
        )
    )

    theta_ppd <- as.matrix(posterior::as_draws_df(fit$draws(variables = "theta_ppd")))

    N_ppd <- sum(!ry)
    id_mis <- which(ry == 0)
    theta_imp <- theta_ppd[nrow(theta_ppd), id_mis]

    return(theta_imp)
}
