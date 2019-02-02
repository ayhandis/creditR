#' @title Scaled Score
#'
#' @description
#' This function allows the creation of scaled score for a given data.
#'
#' @param data A data set needs to be specified including PD variable.
#' @param PD PD variable need to be specified.
#' @param ceiling_score, A ceiling score needs to be specified for transformation.
#' @param increase, An increase level needs to be specified for transformation.
#' @keywords creditR
#' @export
#' @examples
#' default_f <- c('1','0','0', '1','1','0','0','1','1')
#' birth_year <- c(1980, 1985, 1971,1971,1985,1971,1980,1980,1985)
#' PD <- c(0.1,0.12, 0.2, 0.23, 0.28,0.33,0.39,0.45, O.54)
#' example_data <- data.frame(default_f,birth_year,PD)
#' scaled.score(example_data, "PD", 1000, 15)


scaled.score <- function(data, PD, ceiling_score, increase){
  odds <- (1-data[,PD])/data[,PD]
  factor <- increase/log(2)
  offset_val <- ceiling_score - (factor* log(increase))
  data$scaled_score <- offset_val + factor * log(odds)
  return(data)
}

