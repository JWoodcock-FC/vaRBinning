#' Provides a summary of the independent variable versus the response.
#'
#' Provides a summary of the independent variable versus the response.
#' @param data The dataframe of interest.
#' @param target A string value containing the name of the binary target variable.
#' @param variable A string value containing the name of the independent variable.
#' @export
#' @examples
#' variableSummary()

variableSummary <- function(data, target, variable) {

    df <- data.frame(variable = data[[variable]],
                     bad = data[[target]])

    nUnique <- length(unique(df$variable))
    nMissing <- sum(is.na(df$variable))

    df2 <- df %>%
        filter(!is.na(variable))

    if (nUnique <= 10 | !is.numeric(df2[["variable"]])) {
        df3 <- df2 %>%
            group_by(variable) %>%
            summarise(Bads = sum(bad),
                      BadRate = mean(bad),
                      Volume = n(),
                      Upper = stats::prop.test(Bads, Volume, conf.level = 0.9)$conf.int[2],
                      Lower = stats::prop.test(Bads, Volume, conf.level = 0.9)$conf.int[1]) %>%
            arrange(BadRate) %>%
            rename(Bin = variable) %>%
            mutate(Colour = ifelse(Volume < nrow(df2) * 0.05, 1, 0))
        print("this one")
    } else {
        df3 <- df2 %>%
            mutate(Bin = cut(variable, unique(quantile(variable, seq(0,1,0.2))), include.lowest = T)) %>%
            group_by(Bin) %>%
            summarise(Bads = sum(bad),
                      BadRate = mean(bad),
                      Volume = n(),
                      Upper = stats::prop.test(Bads, Volume, conf.level = 0.9)$conf.int[2],
                      Lower = stats::prop.test(Bads, Volume, conf.level = 0.9)$conf.int[1]) %>%
            arrange(Bin) %>%
            mutate(Colour = ifelse(Volume < nrow(df2) * 0.05, 1, 0))
    }

    plot <- ggplot(data = df3, aes(x = Bin, y = BadRate, fill = as.factor(Colour), colour = as.factor(Colour))) +
        geom_point() +
        geom_errorbar(aes(ymax = Upper, ymin = Lower)) +
        theme_bw() +
        labs(title = paste0(variable),
             subtitle = "Response Rate Plot",
             x = paste0(variable),
             y = "Bad Rate") +
        theme(legend.position = "none")

    plot

}
