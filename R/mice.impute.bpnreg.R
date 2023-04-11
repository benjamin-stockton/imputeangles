mice.impute.bpnreg <- function(y, ry, x,...) {

    dat <- as.data.frame(cbind(y[ry], x[ry,]))
    dat$theta <- as.numeric(circular::minusPiPlusPi(circular(dat[,1])))
    dat <- dat[,-1]

    invisible(capture.output(fit <- bpnreg::bpnr(theta ~ ., data = dat,
                                                 its = 2000, burn = 1000)))

    b1 <- fit$beta1; b2 <- fit$beta2

    x_tilde <- x[!ry,]
    x_tilde <- cbind(rep(1, nrow(x_tilde)), x_tilde)
    theta_ppd <- matrix(NA, nrow = nrow(b1), ncol = nrow(x_tilde))
    for (i in 1:nrow(b1)) {
        mu_1 <- x_tilde %*% b1[i,]
        mu_2 <- x_tilde %*% b2[i,]
        y_1 <- rnorm(ncol(theta_ppd), mu_1, 1)
        y_2 <- rnorm(ncol(theta_ppd), mu_2, 1)
        ssq <- sqrt(y_1^2 + y_2^2)
        U_1 <- y_1 / ssq
        U_2 <- y_2 / ssq

        theta_ppd[i,] <- atan2(U_2, U_1) # %% (2*pi)
    }

    theta_imp <- as.numeric(theta_ppd[sample(nrow(theta_ppd), size = 1),])

    return(theta_imp)
}
