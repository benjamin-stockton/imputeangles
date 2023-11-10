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

test_that("PN Regression works with data frame", {

    N <- 100
    x <- rnorm(N)
    B <- matrix(c(3.5, 1, -3, 0), nrow = 2, byrow = TRUE)
    x2 <- rnorm(N, sd = 2)
    B2 <- rbind(B, c(3, -.5))
    x <- data.frame(x1 = x,
                    x2 = x2)
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

test_that("PN Imputation works with data frame input", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    x <- data.frame(x1 = x[,1],
                     x2 = x[,2])
    y_imp <- mice.impute.bpnreg(y, ry, x)
    expect_length(y_imp, N - sum(ry))
})

test_that("PN Imputation works for single missing obervation", {
    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    mis <- sample(N, size = 1, replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    y_imp <- mice.impute.bpnreg(y, ry, x)
    expect_length(y_imp, N - sum(ry))
})

# test_that("MICE works when missing on multiple variables", {
#     N <- 30
#     x <- matrix(rnorm(2 * N), ncol = 2)
#     B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
#     y <- pnreg_draw(x, B)
#     mis <- sample(N, size = 5, replace = FALSE)
#     y[mis] <- NA
#     mis2 <- sample(N, size = 5, replace = FALSE)
#     x[mis2,1] <- NA
#
#     df <- data.frame(
#         theta = y,
#         x1 = x[,1],
#         x2 = x[,2],
#         u1 = cos(y),
#         u2 = sin(y)
#     )
#
#     pred_mat <- matrix(c(0, 1, 1, 0, 0,
#                          0, 0, 1, 1, 1,
#                          0, 1, 0, 1, 1,
#                          1, 1, 1, 0, 0,
#                          1, 1, 1, 0, 0),
#                        byrow = TRUE, ncol = 5)
#
#     expect_no_error(mice::mice(df, m = 1,
#                                method = c("bpnreg", "pmm", "pmm", "~cos(theta)", "~sin(theta)"),
#                                predictorMatrix = pred_mat,
#                                printFlag = FALSE))
# })

####################################
# Test 4: Posterior Predictive Draws
####################################

test_that("PPD draws for PN reg work with a vector covariate", {

    N <- 30
    x <- matrix(rnorm(N), ncol = 1)
    B <- matrix(c(3.5, 1, -3, 0), ncol = 2, byrow = TRUE)
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
