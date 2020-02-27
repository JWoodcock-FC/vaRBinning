#' Wight of Evidence Function
#'
#' This function takes a data frame , along with a target variable name, independent variable name, and user defined
#' groupings in order to produce Weight of Evidence bandings and transformations.
#' It returns an S3 object which implements plot and predict methods.
#' @param data The dataframe of interest.
#' @param target A string value containing the name of the binary target variable.
#' @param variable A string value containing the name of the independent variable.
#' @param cuts Either a vector of continuous cutpoints, or a list of categorical groupings by which to group the independent variable.
#' @export
#' @examples
#' cat_function()

weightOfEvidence <- function(data, target, variable, cuts) {

    df <- data.frame(variable = data[[variable]],
                     bad = data[[target]])

    if (is.list(cuts)) {

        df2 <- listCuts(df, cuts)
    } else if (is.vector(cuts)) {

        if (class(cuts) == "numeric") {

            df2 <- vectorCuts(df, cuts)
        } else {

            stop("Vector elements are not numeric.")
        }
    } else {

        stop("Cuts is neither a vector nor a list.")
    }

    df3 <- df2 %>%
        dplyr::mutate(Response_Char = ifelse(bad == 1, "Bad", "Good")) %>%
        dplyr::select(-bad) %>%
        dplyr::rename(Response = Response_Char) %>%
        group_by(bin, Response) %>%
        summarise(Count = n()) %>%
        tidyr::spread(Response, Count) %>%
        ungroup() %>%
        mutate(Total = Good + Bad,
               Percent = Total/sum(Total),
               BadPercent = Bad/Total,
               GoodPercent = Good/Total,
               PercentOfBad = Bad/sum(Bad),
               PercentOfGood = Good/sum(Good),
               WOE = log(PercentOfGood/PercentOfBad),
               IV = (PercentOfGood - PercentOfBad) * WOE) %>%
        select(bin,
               `Number of Bads` = Bad,
               `Number of Goods` = Good,
               `Volume` = Total,
               `Volume %` = Percent,
               `Bad Rate` = BadPercent,
               WOE,
               IV)

    colnames(df3)[1] <- variable

    sumIV <- sum(df3$IV)

    outObject <- list(WOE_Table = df3,
                      IV = sumIV,
                      Cuts = cuts)

    class(outObject) <-  "VariableBinningS3"

    outObject

}
