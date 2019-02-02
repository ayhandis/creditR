#' @title Missing Ratio
#'
#' @description
#' This function allows you to calculate the missing ratios of variables for a given data set.
#'
#' @param data A data set needs to be defined.
#' @keywords creditR
#' @export
#' @examples
#' name <- c('John Doe','Peter Gynn','Jolie Hope')
#' birth_year <- c(1980, 1985, 1971)
#' salary <- c(20000,NA,10000)
#' example_data <- data.frame(name,birth_year,salary)
#' missing_ratio(example_data)


missing_ratio <- function(data){
  column_names <- matrix(data=NA,nrow=1,ncol=length(data))
  missing_ratio <- matrix(data=NA,nrow=1,ncol=length(data))
  for(i in 1:length(data)){
    column_names[,i]<-colnames(data[i])
    missing_ratio[,i]<-mean(!complete.cases(data[,i]))
  }
  missing_ratio<-as.data.frame(cbind(t(column_names), t(missing_ratio)))
  colnames(missing_ratio) <- c("Variable","Missing_Ratio")
  missing_ratio$Missing_Ratio <- as.numeric(as.character(missing_ratio$Missing_Ratio))
  missing_ratio$Completeness <- 1-missing_ratio$Missing_Ratio
  return(missing_ratio)


}

