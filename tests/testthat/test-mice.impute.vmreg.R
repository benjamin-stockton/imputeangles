####################################
# Test 1: Imputation by PN Regression works
####################################

test_that("vM Imputation is correct length", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    y_imp <- mice.impute.vmreg(y, ry, x)
    expect_length(y_imp, N - sum(ry))
})

test_that("vM Imputation isn't run when the data are complete", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    ry <- !is.na(y)
    expect_error(mice.impute.vmreg(y, ry, x))

})

test_that("vM Imputation for a single missing observation", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    mis <- sample(N, size = 1, replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    y_imp <- mice.impute.vmreg(y, ry, x)
    expect_length(y_imp, N - sum(ry))

})

test_that("vM Imputation for a single missing observation with a single predictor", {

    N <- 30
    x <- matrix(rnorm(N), ncol = 1)
    B <- matrix(c(3.5, 1, -3, 0), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    mis <- sample(N, size = 1, replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    y_imp <- mice.impute.vmreg(y, ry, x)
    expect_length(y_imp, N - sum(ry))

})

####################################
# Test 2: Posterior Predictive Draws
####################################

test_that("PPD draws for vM reg work with a vector covariate", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    xc <- scale(x)
    dat <- construct_modeling_data(y, ry, xc)

    fit <- circglmbayes::circGLM(theta ~ ., data = dat,
                                 Q = 100, thin = 10, burnin = 25000,
                                 bt_prior_musd = c(mu = 0, sd = 1),
                                 r = 2)

    expect_length(ppd_draws_vmreg(fit, ry, x), N - sum(ry))
})

test_that("PPD draws for vM reg work with a covariate matrix", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    xc <- scale(x)
    dat <- construct_modeling_data(y, ry, xc)

    fit <- circglmbayes::circGLM(theta ~ ., data = dat,
                                 Q = 100, thin = 10, burnin = 25000,
                                 bt_prior_musd = c(mu = 0, sd = 1),
                                 r = 2)

    expect_length(ppd_draws_vmreg(fit, ry, x), N - sum(ry))
})
