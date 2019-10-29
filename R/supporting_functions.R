listCuts <- function(df, cuts) {

    numberOfListElements <- length(cuts)

    df$bin <- NA

    for (i in 1:numberOfListElements) {
        df <- df %>%
            mutate(bin = ifelse(variable %in% cuts[[i]], paste(cuts[[i]], collapse = ", "), bin))
    }

    df <- df %>%
        mutate(bin = case_when(
            !is.na(bin) ~ as.character(bin),
            is.na(variable) & is.na(bin) ~ "Missing",
            !is.na(variable) & is.na(bin) ~ "Else",
            T ~ "Undefined"
        ))

    df

}

vectorCuts <- function(df, cuts) {

    df <- df %>%
        mutate(bin = cut(variable, cuts)) %>%
        mutate(bin = case_when(
            !is.na(bin) ~ as.character(bin),
            is.na(variable) & is.na(bin) ~ "Missing",
            T ~ "Undefined"
        ))

    df

}

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

giniOfBinning <- function(obj, df) {

    df$WOE <- predict(obj, df$variable)$WOE
    2*glmnet::auc(df$bad, df$WOE) - 1

}
