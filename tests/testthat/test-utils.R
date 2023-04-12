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
# Test 2: Constructor for bpnreg data frame with mice syntax
######################################

test_that("Constucted data for modeling has 'theta' column", {
    N <- 100
    x <- matrix(rnorm(2 * N), ncol = 2)
    B <- matrix(c(3.5, 1, -3, 0, 3, -0.5), ncol = 2, byrow = TRUE)
    y <- pnreg_draw(x, B)
    mis <- sample(N, size = floor(0.5 * N), replace = FALSE)
    y[mis] <- NA
    ry <- !is.na(y)
    x <- cbind(rep(1, N), x)

    expect_true("theta" %in% colnames(construct_modeling_data(y, ry, x)))
})
