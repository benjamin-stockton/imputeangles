#' Plot Angular Imputations
#'
#' @description
#' Create a plot of the angular imputations produced by mice. The plot displays a unit circle for each imputation with observed points plotted as blue points on the circle and imputations plotted as pink points on the circle. This plot is similar in nature to the stripplot included in the mice package.
#'
#' @param imps A mids object produced by mice
#' @param r numeric Radius for unit circles
#' @param alpha numeric opacity for the points
#' @param by_id bool Switch for plots of unit circles for each imputation or plots of vectors by id and by imputation
#' @param overlay bool Swithc for whether angles should be plotted from the same origin
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#'    N <- 30
#     df <- pnreg_sim_data(N = N, mu_X = 0, sigma_X = 2)
#
#     pred_mat <- matrix(c(0, 0, 0, 1, 1,
#                          1, 0, 0, 0, 0,
#                          1, 0, 0, 0, 0,
#                          0, 1, 1, 0, 1,
#                          0, 1, 1, 1, 0),
#                        byrow = TRUE, ncol = 5)
#
#     imps <- mice::mice(df, m = 5,
#                                method = c("bpnreg", "~cos(theta)", "~sin(theta)", "pmm", "pmm"),
#                                predictorMatrix = pred_mat,
#                                printFlag = FALSE)
#     plot_angular_imputations(imps)
plot_angular_imputations <- function(imps, r = 0.35, alpha = 0.75, by_id = FALSE, overlay = FALSE) {
    c_imps <- mice::complete(imps, "long", include = TRUE)
    mis_ind <- which(imps$where[,"theta"])
    num_imps <- imps$m

    c_imps$Type <- "Observed"
    c_imps[c_imps$.id %in% mis_ind & c_imps$.imp > 0,"Type"] <- "Imputed"
    if (!by_id) {
        (p1 <- ggplot2::ggplot(c_imps, ggplot2::aes(.imp, 0)) +
             ggforce::geom_circle(ggplot2::aes(x0 = .imp, y0 = 0, r = r), linetype = "dashed") +
             ggplot2::geom_point(ggplot2::aes(x = .imp + r * cos(theta), y = r * sin(theta), color = Type), alpha = alpha) +
            ggplot2::scale_x_continuous(breaks = 0:num_imps, labels = 0:num_imps) +
            ggplot2::coord_fixed(ylim = c(-1/3 * (num_imps+1), 1/3 * (num_imps+1))) +
             ggplot2::scale_color_manual(values = c("#B61A51B3","#006CC2B3")) +
            ggplot2::labs(x = "Imputation Number", y = "") +
            ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
                           panel.grid = ggplot2::element_blank(),
                           axis.text.y = ggplot2::element_blank(),
                           axis.ticks.length.y = ggplot2::unit(0, "cm")))
    }
    else if (by_id && !overlay) {
        mis_imps <- c_imps[c_imps$.id %in% mis_ind & c_imps$.imp > 0,]
        (p1 <- ggplot2::ggplot(mis_imps, ggplot2::aes(.imp, 0)) +
                ggplot2::geom_point(color = "#B61A51B3") +
                ggplot2::geom_segment(ggplot2::aes(x = .imp, xend = .imp + r * cos(theta),
                                                   y = 0, yend = r * sin(theta)),
                                      alpha = alpha, arrow = ggplot2::arrow(length = ggplot2::unit(0.15,"cm"))) +
                ggplot2::coord_fixed(ylim = c(-1/6 * (num_imps+1), 1/6 * (num_imps+1))) +
                ggplot2::labs(x = "Imputation Number", y = "") +
                ggplot2::facet_wrap(as.factor(.id)~.) +
                ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
                               panel.grid = ggplot2::element_blank(),
                               axis.text.y = ggplot2::element_blank(),
                               axis.ticks.length.y = ggplot2::unit(0, "cm")))
    }
    else if (overlay && by_id) {
        mis_imps <- c_imps[c_imps$.id %in% mis_ind & c_imps$.imp > 0,]
        (p1 <- ggplot2::ggplot(mis_imps, ggplot2::aes(.id, 0)) +
                ggplot2::geom_point(color = "#B61A51B3") +
                ggplot2::geom_segment(ggplot2::aes(x = .id,
                                                   xend = .id + r * cos(theta),
                                                   y = 0, yend = r * sin(theta)),
                                      alpha = alpha, arrow = ggplot2::arrow(length = ggplot2::unit(0.15,"cm"))) +
                ggplot2::coord_fixed(ylim = c(-r/6 * (num_imps+1), r/6 * (num_imps+1))) +
                ggplot2::labs(x = "Imputation Number", y = "") +
                ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
                               panel.grid = ggplot2::element_blank(),
                               axis.text.y = ggplot2::element_blank(),
                               axis.ticks.length.y = ggplot2::unit(0, "cm")))
    }

    return(p1)
}
