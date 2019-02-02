#' @title NA Checker
#'
#' @description
#' This function controls the presence of NA values in variables for a given data set. For the results returned as TRUE, the corresponding variable has NA observation/observations.
#'
#' @param data A data set needs to be defined.
#' @keywords creditR
#' @export
#' @examples
#' name <- c('John Doe','Peter Gynn','Jolie Hope')
#' birth_year <- c(1980, 1985, 1971)
#' salary <- c(20000,NA,10000)
#' example_data <- data.frame(name,birth_year,salary)
#' na_checker(example_data)


na_checker <-function(data){
  str(lapply(data, function(col) any(is.na(col))))
}


