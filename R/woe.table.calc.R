#' @title Calculate WOE Table
#'
#' @description
#' This function allows the calculation of the WOE table for a given variable. It can be useful for manual interventions in WOE transformations.
#'
#' @param data A data set needs to be specified.
#' @param default_flag  The default flag need to specified as string.
#' @param variable A variable need to be defined as a string.
#' @keywords packagename
#' @export
#' @examples
#' default_f <- c('1','0','0', '1','1','0','0','1')
#' birth_year <- c(1980, 1985, 1971,1971,1985,1971,1980,1980)
#' salary <- c(20000,2000,10000, 10050,2000,21500,12422,10521)
#' example_data <- data.frame(default_f,birth_year,salary)
#' woe.table.calc(example_data, "default_f", "birth_year")


woe.table.calc <- function(data,default_flag, variable){
  number <- which( colnames(data)==default_flag )
  good = subset(data, data[,number] == "0")
  bad = subset(data, data[,number] == "1")
  x <- table(good[,variable])
  y <- as.data.frame(x)
  z <- table(bad[,variable])
  u <- as.data.frame(z)
  merged = merge(x = y, y = u, by="Var1", all = TRUE)
  merged$percentx = merged$Freq.x/(sum(merged$Freq.x))
  merged$percenty = merged$Freq.y/(sum(merged$Freq.y))
  merged$WOE = log(merged$percenty/merged$percentx)
  return(merged)


}

