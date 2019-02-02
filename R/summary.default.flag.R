#' @title Summary by Default Flag
#'
#' @description
#' This function allows calculation of the summary statistics of a given variable based on good and bad observations.
#'
#' @param data A data set needs to be specified.
#' @param default_flag The default flag need to specified.
#' @param variable A variable need to be specified to calculate summary statistics.
#' @keywords creditR
#' @export
#' @examples
#' default_f <- c('1','0','0', '1')
#' birth_year <- c(1980, 1985, 1971, 1990)
#' salary <- c(20000,NA,10000, 10050)
#' example_data <- data.frame(default_f,birth_year,salary)
#' summary.default.flag(example_data, "default_f", "birth_year")


summary.default.flag <- function(data, default_flag, variable ) {
    number <- which( colnames(data)==default_flag )
    print("Good Observations")
    print(summary(subset(data, data[,number] =="0")[,variable]))
    print("Bad Observations")
    print(summary(subset(data, data[,number] =="1")[,variable]))
}


