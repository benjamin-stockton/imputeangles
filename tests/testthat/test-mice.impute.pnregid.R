
####################################
# Test 3: Imputation by PN Regression with Stan works
####################################

test_that("pnregid Imputation is correct length", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- bpnreg_draw(x, B)
    mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    y_imp <- mice.impute.pnregid(y, ry, x)
    expect_length(y_imp, N - sum(ry))
})

test_that("pnregid isn't run when the data are complete", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- bpnreg_draw(x, B)
    ry <- !is.na(y)
    expect_error(mice.impute.pnregid(y, ry, x))

})

test_that("pnregid works with data frame input", {

    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- bpnreg_draw(x, B)
    mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    x <- data.frame(x1 = x[,1],
                    x2 = x[,2])
    y_imp <- mice.impute.pnregid(y, ry, x)
    expect_length(y_imp, N - sum(ry))
})

test_that("pnregid works for single missing obervation", {
    N <- 30
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- bpnreg_draw(x, B)
    mis <- 15
    y[mis] <- NA
    ry <- !is.na(y)
    y_imp <- mice.impute.pnregid(y, ry, x, show_message = TRUE, show_exceptions = TRUE)
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
