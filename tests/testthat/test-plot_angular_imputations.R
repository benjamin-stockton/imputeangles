test_that("Plot works", {
    N <- 30
    df <- pnregstan::pnreg_sim_data(N = N, mu_X = 0, sigma_X = 2)

    amps <- mice::ampute(df, patterns = matrix(c(0,0,0,1), nrow = 1))
    df_mis <- amps$amp

    pred_mat <- matrix(c(0, 0, 0, 1,
                         1, 0, 0, 0,
                         1, 0, 0, 0,
                         0, 1, 1, 0),
                       byrow = TRUE, ncol = 4)
    colnames(pred_mat) <- colnames(df)
    rownames(pred_mat) <- colnames(df)

    imps <- mice::mice(df_mis, m = 5, maxit = 1,
                       method = c("bpnreg", "~cos(theta)", "~sin(theta)", ""),
                       predictorMatrix = pred_mat,
                       printFlag = FALSE)

  expect_no_error(plot_angular_imputations(imps))
})
