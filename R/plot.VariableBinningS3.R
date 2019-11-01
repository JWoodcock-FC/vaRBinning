#' plot.VariableBinningS3
#'
#' Implements the \code{plot} method for \code{VariableBinningS3}.
#' @param obj An object of type \code{VariableBinningS3}.
#' @export
#' @examples
#' plot()


plot.VariableBinningS3 <- function(obj) {

    df2 <- obj$WOE_Table
    varName <- colnames(df2)[1]
    colnames(df2)[1] <- "Var"

    if (max(abs(df2$WOE), na.rm = T) > 1) {
        range <- round(max(abs(df2$WOE)) + 0.1, 1)
    } else {
        range <- 1
    }

    plot <- ggplot(data = df2, aes(x = Var, y = WOE, fill = as.factor(sign(WOE)))) +
        geom_bar(stat = "identity") +
        labs(x = "Value", y = "Weight of Evidence",
             title = varName,
             subtitle = "Weight of Evidence Plot",
             caption = paste("Information Value =", round(sum(df2$IV),2))) +
        geom_text(aes(label = round(`Bad Rate`, 3)), position = position_stack(vjust = 0.5)) +
        theme_bw() +
        theme(legend.position = "none") +
        scale_fill_brewer(palette = 1) +
        ylim(-range, range)

    plot
}
