#' @title Calcuate the System Stability Index for a Given Variable
#'
#' @description
#' This function allows the calculation of the SSI for a given variable.
#' @param main_data A data set needs to be specified.
#' @param second_data  A second data set needs to be specified.
#' @param variable A variable needs to be specified.
#' @keywords creditR
#' @export
#' @examples
#' SSI.calc(data1, data2, "catvar")

SSI.calc <- function(main_data, second_data, variable){
  y <- as.data.frame(table(main_data[,variable]))
  u <- as.data.frame(table(second_data[,variable]))
  merged <- merge(x = y, y = u, by="Var1", all = TRUE)
  merged$percenty <- merged$Freq.y/(sum(merged$Freq.y))
  merged$percentx <- merged$Freq.x/(sum(merged$Freq.x))
  merged$SSI <- (merged$percentx-merged$percenty)*(log(merged$percentx/merged$percenty))
  print(merged)
  return(sum(merged$SSI))
}


