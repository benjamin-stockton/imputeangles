##############################
# Test 1: Angle Conversion Works
##############################

test_that("Angle domain conversion works", {
    theta <- c(0, pi, 2*pi)

    expect_true(min(convert2MinusPiPlusPi(theta)) == -pi)
    expect_true(max(convert2MinusPiPlusPi(theta)) == 0)
    expect_true(convert2MinusPiPlusPi(pi) == -pi)
})

test_that("Angle conversion wraps inline data", {
    x <- c(-2*pi, pi, 4*pi)

    expect_false(max(x) - min(x) <= 2*pi)
    expect_true(max(convert2MinusPiPlusPi(x)) < pi)
    expect_true(min(convert2MinusPiPlusPi(x)) >= -pi)
    expect_true(max(convert2MinusPiPlusPi(x)) - min(convert2MinusPiPlusPi(x)) <= 2*pi)
})

######################################
# Test 2: PN Regression Draws
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

######################################
# Test 3: Constructor for bpnreg data frame with mice syntax
######################################

test_that("Constucted data for bpnreg has 'theta' column", {
    N <- 100
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    x <- cbind(rep(1, N), x)

    expect_true("theta" %in% colnames(construct_data_bpnreg(y, ry, x)))
})


####################################
# Test 5: Collect Posterior Draws from bpnr 'fit' object
####################################

test_that("Posterior draws have the correct dimensions", {
    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    ry <- !is.na(y)
    dat <- construct_data_bpnreg(y, ry, x)
    iters <- 1000
    invisible(
        utils::capture.output(
            fit <- bpnreg::bpnr(theta ~ ., data = dat, its = iters, burn = 1000)
            )
        )
    expect_equal(ncol(get_posterior_draws_pnreg(fit)), 2)
    expect_equal(nrow(get_posterior_draws_pnreg(fit)), 3)
})
