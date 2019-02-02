#' @title Calcuate the System Stability Index for a Given Data
#'
#' @description
#' This function allows the calculation of the SSI for a given data.
#'
#' @param main_data A main data set needs to be specified.
#' @param second_data  A second data set needs to be specified.
#' @param default_flag The default flag need to specified as string to exlude from the data.
#' @keywords creditR
#' @export
#' @examples
#' SSI.calc.data(data1, data2, "default_f")

SSI.calc.data <- function(main_data,second_data, default_flag){

  SSI.calc <- function(main_data, second_data, variable){
    y <- as.data.frame(table(main_data[,variable]))
    u <- as.data.frame(table(second_data[,variable]))
    merged <- merge(x = y, y = u, by="Var1", all = TRUE)
    merged$percenty <- merged$Freq.y/(sum(merged$Freq.y))
    merged$percentx <- merged$Freq.x/(sum(merged$Freq.x))
    merged$SSI <- (merged$percentx-merged$percenty)*(log(merged$percentx/merged$percenty))
    return(sum(merged$SSI))
  }
  SSI_column_names <- matrix(data=NA,nrow=1,ncol=length(main_data))
  SSI_Values <- matrix(data=NA,nrow=1,ncol=length(main_data))
  for(i in 1:length(main_data)){
    SSI_Value <- SSI.calc(main_data = main_data,second_data = second_data, colnames(main_data)[i])
    SSI_Values[,i] <- SSI_Value
    SSI_column_names[,i] <- colnames(main_data)[i]
  }
  SSI_summary<-as.data.frame(cbind(t(SSI_column_names), t(SSI_Values)))
  colnames(SSI_summary) <- c("Variable","SSI")
  SSI_summary <- SSI_summary[SSI_summary[,1] != default_flag,]
  rownames(SSI_summary) <- c(1:length(SSI_summary[,1]))
  return(SSI_summary)
}
