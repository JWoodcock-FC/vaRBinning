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
