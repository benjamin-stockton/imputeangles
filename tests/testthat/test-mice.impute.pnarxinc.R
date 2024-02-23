####################################
# Test 1: Imputation by PN ARX works
####################################

test_that("PN ARX Incomplete Imputation is correct length", {
    N <- 100
    df <- pnregstan::pn_arx_sim_data(N = N, ar_X1 = c(0.24), ma_X1 = numeric(0),
                                     ar_X2 = c(0.75), ma_X2 = c(-0.25, 0.25))
    mis <- sample(N, size = floor(0.25 * N), replace = FALSE)
    y <- df$theta
    x <- df[,c("X1", "X2")]
    y[mis] <- NA
    ry <- !is.na(y)
    y[mis] <- mean(y, na.rm = TRUE)
    y_imp <- mice.impute.pnarxinc(y, ry, x)
    expect_length(y_imp, N - sum(ry))
})
