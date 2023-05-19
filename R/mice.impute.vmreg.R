#' Impute Incomplete Angular Data with von Mises Regression
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
#' mice.impute.vmreg(y, ry, x)
mice.impute.vmreg <- function(y, ry, x,...) {
    if (sum(ry) == length(y)) {
        stop("No missing data! :)")
    }

    xc <- scale(x)

    dat <- construct_modeling_data(y, ry, xc)

    fit <- circglmbayes::circGLM(theta ~ ., data = dat,
                                 Q = 100, thin = 10, burnin = 25000,
                                 bt_prior_musd = c(mu = 0, sd = 1),
                                 r = 2)

    theta_imp <- ppd_draws_vmreg(fit, ry, xc)

    # prior1 <- brms::prior(brms::normal(0, 5), class = b) +
    #             brms::prior(brms::normal(0, 5), class = Intercept)
    #
    # # intercept in the link function
    # fit <- brms::brm(theta ~ ., data = dat,
    #                  family = brms::von_mises(link = "tan_half",
    #                                           link_kappa = "log"),
    #                  prior = prior1)
    #
    # theta_imp <- brms::posterior_predict(fit, ndraws = 1, summary = FALSE)

    return(theta_imp)
}


#' Collect Posterior Predictive Draws for von Mises Regression
#'
#' @param fit A 'circglm' fit object.
#' @param ry A logical vector of length N indicating whether the ith observation is observed (TRUE) or missing (FALSE).
#' @param x A N x k centered and scaled predictor matrix.
#'
#' @return theta_imp A numeric vector of size n_mis.
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
#' xc <- scale(x)
#' dat <- construct_modeling_data(y, ry, xc)
#' fit <- circglmbayes::circGLM(theta ~ ., data = dat,
#'                              Q = 100, thin = 10, burnin = 25000,
#'                              bt_prior_musd = c(mu = 0, sd = 1),
#'                              r = 2)
#' ppd_draws_vmreg(fit, ry, x)
ppd_draws_vmreg <- function(fit, ry, x) {
    all_chains <- fit$all_chains

    K <- nrow(coef(fit)) - 2
    N <- sum(!ry)

    if (K == 1 && N >= 1) {
        x_tilde <- x[!ry]
    } else if (K > 1 && N == 1) {
        x_tilde <- x[!ry,]
    } else if (K > 1 && N > 1) {
        x_tilde <- x[!ry,]
    }

    s <- sample(nrow(all_chains), size = 1)
    beta_pd <- all_chains[s, 3:(K+2)]

    theta_imp <- numeric(N)
    if (is.vector(x_tilde)) {
        eta_i <- x_tilde * beta_pd
    } else if (is.matrix(x_tilde) | is.data.frame(x_tilde)) {
        eta_i <- (x_tilde %*% beta_pd)[,1]
    }

    for (i in 1:N) {
        mu_i <- circular::circular(all_chains[s, "b0_chain"] + 2 * atan(eta_i[i]))
        theta_imp[i] <- circular::rvonmises(1, mu_i, all_chains[s, "kp_chain"])
    }
    theta_imp <- as.numeric(theta_imp)
    return(theta_imp)
}
