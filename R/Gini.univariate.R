#' @title Univariate Gini
#'
#' @description
#' This function performs the calculation of the Gini from the estimated values calculated by logistic regression of a variable. It should be noted that the Gini values calculated by logistic regression.
#'
#' @param data A data set needs to be specified.
#' @param default_flag  The default flag need to specified as string.
#' @param variable The name of the variable need to specified to calculate Gini value.
#' @keywords creditR
#' @import MLmetrics
#' @export
#' @examples
#' default_f <- c('1','0','0', '1','1','0','0','1','1')
#' birth_year <- c(1980, 1985, 1971,1971,1985,1971,1980,1980,1985)
#' job <- c(1,1,2, 2,2,3,3,2,3)
#' example_data <- data.frame(default_f,birth_year,job)
#' Gini.univariate(example_data, "default_f", "birth_year")


Gini.univariate <- function(data, default_flag, variable){
  number <- which( colnames(data)==default_flag)
  number1 <- which( colnames(data)==variable)
  univ_model <- glm(formula = data[,number] ~ data[,number1], family = binomial(link = "logit"), data = data )
  univ_gini <- Gini(y_pred = univ_model$fitted.values, y_true = as.numeric(as.character(data[,default_flag])))
  return(univ_gini)


}
