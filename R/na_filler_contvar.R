#' @title Variable NA Filler
#'
#' @description
#' This function allows you to fill the missing values of a continuous  variable for a given data set. According to the Kolmogorov-Smirnov test, if the variable is distributed normally, the NA values are filled with the mean value, otherwise they are filled with the median value.
#'
#' @param data A data set needs to be defined.
#' @param variable A variable need to be defined as a string.
#' @param pvalue A p-value threshold need to be defined.
#' @keywords creditR
#' @export
#' @examples
#' name <- c('John Doe','Peter Gynn','Jolie Hope')
#' birth_year <- c(1980, 1985, 1971)
#' salary <- c(20000,NA,10000)
#' example_data <- data.frame(name,birth_year,salary)
#' na_filler_contvar(example_data,"salary")

na_filler_contvar <- function(data, variable, pvalue = 0.05){
  avg <- mean(data[,variable], na.rm = TRUE)
  median <- median(data[,variable], na.rm = TRUE)
  normal_dist <- rnorm(length(data[,variable]) - sum(is.na(data[,variable])))
  data_nasiz <- na.omit(select(data,variable))
  ks_result <- ks.test(data_nasiz,normal_dist)
  median_avg <- ifelse(ks_result$p.value < pvalue,median,avg)
  data[,variable] <- ifelse(is.na(data[,variable]) == TRUE, median_avg, data[,variable])
  return(data)
}


