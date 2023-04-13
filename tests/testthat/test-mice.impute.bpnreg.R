######################################
# Test 1: PN Regression Draws
######################################

test_that("PN Regression sampling works", {
    N <- 100
    x <- rnorm(N)
    B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)

    expect_true(is.numeric(y))
    expect_length(y, N)
    expect_true(max(y) < pi)
    expect_true(min(y) >= -pi)
})


test_that("PN Regression works with numeric predictor matrix", {

    N <- 100
    x <- rnorm(N)
    B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
    x2 <- rnorm(N, sd = 2)
    B2 <- rbind(B, c(3, -.5))
    x <- cbind(x, x2)
    y <- pnreg_draw(x, B2)

    expect_true(ncol(x) == 2)
    expect_true(is.numeric(y))
    expect_length(y, N)
})


test_that("PN regression throws an error for three column B", {
    N <- 100
    x <- rnorm(N)
    B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
    x2 <- rnorm(N, sd = 2)
    B2 <- rbind(B, c(3, -.5))
    B3 <- cbind(B2, rep(1, 3))

    expect_error(pnreg_draw(x2, B3))
})

test_that("PN regression can draw for a single observation", {
    N <- 100
    x <- rnorm(N)
    B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)

    expect_length(pnreg_draw(3, B), 1)
})


####################################
# Test 2: Collect Posterior Draws from bpnr 'fit' object
####################################

test_that("Posterior draws have the correct dimensions", {
    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    ry <- !is.na(y)
    dat <- construct_modeling_data(y, ry, x)
    iters <- 1000
    invisible(
        utils::capture.output(
            fit <- bpnreg::bpnr(theta ~ ., data = dat, its = iters, burn = 1000)
            )
        )
    expect_equal(ncol(posterior_draw_pnreg(fit)), 2)
    expect_equal(nrow(posterior_draw_pnreg(fit)), 3)
})

####################################
# Test 3: Imputation by PN Regression works
####################################

test_that("PN Imputation is correct length", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    y_imp <- mice.impute.bpnreg(y, ry, x)
    expect_length(y_imp, N - sum(ry))
})

test_that("PN Imputation isn't run when the data are complete", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    ry <- !is.na(y)
    expect_error(mice.impute.bpnreg(y, ry, x))

})

####################################
# Test 4: Posterior Predictive Draws
####################################

test_that("PPD draws for PN reg work with a vector covariate", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    expect_length(ppd_draws_pnreg(B, ry, x), N - sum(ry))
})

test_that("PPD draws for PN reg work with a covariate matrix", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    expect_length(ppd_draws_pnreg(B, ry, x), N - sum(ry))
})
