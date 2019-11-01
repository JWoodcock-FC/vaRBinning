#' predict.VariableBinningS3
#'
#' Implements the \code{predict} method for \code{VariableBinningS3}.
#' @param obj An object of type \code{VariableBinningS3}.
#' @export
#' @examples
#' plot()

predict.VariableBinningS3 <- function(obj, variableVector) {

    mappingTable <- obj$WOE_Table
    varName <- colnames(mappingTable)[1]
    colnames(mappingTable)[1] <- "Var"

    df2 <- data.frame(variable = as.vector(variableVector))

    cuts <- obj$Cuts

    if (is.list(cuts)) {

        df2 <- listCuts(df2, cuts)
    } else if (is.vector(cuts)) {

        if (class(cuts) == "numeric") {

            df2 <- vectorCuts(df2, cuts)
        } else {

            stop("Vector elements are not numeric.")
        }
    } else {

        stop("Cuts is neither a vector nor a list.")
    }

    df2 %>%
        left_join(mappingTable %>% select(Var, WOE), by = c("bin" = "Var"))

}
