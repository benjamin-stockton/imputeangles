#' Impute with Projected Gaussian Process Geostatical Model with Internally Imputed Angles
#'
#' @description
#' The projected Gaussian process geostatistical model with missing angular
#' observations imputed during the sampling procedure. The imputation step
#' integrates out the missingness from the model fitting. Imputes only on
#' the incomplete angular data.
#'
#' @param loc matrix of location coordinates (N x 2)
#' @param theta angles
#' @param x predictors
#' @param M number of imputations
#' @param ... other arguments to pass to fit_pgp_geostat_reg_inc_model()
#'
#' @return A numeric vector of length n_mis.
#' @export
#'
#' @examples
#' ## Not run:
#'   loc <- expand.grid(long = 1:10, lat = 1:10)[,1:2]
#'   loc2 <- expand.grid(long = 5, lat = 5)[,1:2]
#'   X <- pnregstan::gp_geostat_sim_data(N = 10, M = 10, sigma = 1, rho = 5, alpha = 1,
#'                         refresh = 0,
#'                         show_messages = FALSE,
#'                         show_exceptions = FALSE)$X
#'   X <- as.matrix(X, nrow = 100)
#'
#'   df <- pnregstan::pgp_geostat_reg_sim_data(loc = loc, X = X, sigma_w = 1, rho_w = 0, rho = 5,
#'          mu0 = c(1,0), B = matrix(c(-0.5, 1), nrow = 1),
#'          refresh = 0,
#'          show_messages = FALSE,
#'          show_exceptions = FALSE)
#'   ind_miss <- 1:25 * 4
#'
#'   theta <- df$theta
#'   theta[ind_miss] <- NA
#'   rt <- !is.na(theta)
#'
#'   impute_pgpreginc(loc = loc, theta = theta, x = X, M = 2, iter_warmup = 10, iter_sampling = 10)
#'
#' ## End(Not run)
impute_pgpreginc <- function(loc, theta, x, M, ...) {
    loc2 <- pnregstan::create_grid_df(2, 2)

    rt <- !is.na(theta)

    if (sum(rt) == length(theta)) {
        stop("No missing data! :)")
    }
    if (is.vector(x)) {
        X_mat <- matrix(x, nrow = length(theta))
    }
    else if (is.matrix(x) || is.data.frame(x)) {
        X_mat <- as.matrix(x, nrow = length(theta))
    }
    imps <- lapply(1:M, function(m) {
        # invisible(
            utils::capture.output(
                fit <- pnregstan::fit_pgp_geostat_reg_inc_model(loc1 = loc,
                                                                loc2 = loc2,
                                                                theta = theta,
                                                                X = X_mat,
                                                                refresh = 10,
                                                                show_messages = FALSE,
                                                                show_exceptions = FALSE, ...)
            )
        # )
        theta_imp <- posterior::as_draws_matrix(fit$draws(variables = "theta_pred1"))

        N_mis <- sum(!rt)
        id_mis <- which(rt == 0)
        theta_imp <- theta_imp[1, (nrow(loc) - N_mis+1):nrow(loc)]
        theta_obs <- theta[rt]

        df <- data.frame(theta = c(theta_obs, theta_imp),
                         .imp = rep(m, nrow(loc)),
                         .id = 1:nrow(loc))
        loc_srt <- rbind(loc[rt,], loc[!rt,])
        x_srt <- rbind(x[rt,], x[!rt,])
        df <- cbind(df, loc_srt)
        df <- cbind(df, x_srt)
        df$U1 <- cos(df$theta)
        df$U2 <- sin(df$theta)

        return(df)
    })

    return(imps)

}
