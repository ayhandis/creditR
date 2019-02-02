#' @title Calculate Kolmogorov-Smirnov Statistic for a Given Scoring Data
#'
#' @description
#' This function allows the calculation of the KS for a given scoring data.
#'
#' @param data A data set needs to be specified.
#' @param default_flag  The default flag needs to specified as string.
#' @param PD The PD variable needs to be specified.
#' @keywords creditR
#' @export
#' @examples
#' Kolmogorov.Smirnov(model_data, "Default_f", "PD")

Kolmogorov.Smirnov <- function(data, default_flag, PD){
  Events  <- data.frame(data[data[,default_flag]==1,PD])
  NonEvents <- data.frame(data[data[,default_flag]==0,PD])
  colnames(Events) 	<- c(PD)
  colnames(NonEvents)  <- c(PD)
  Kstest 			 <- ks.test(as.vector(t(Events[1])),as.vector(t(NonEvents[1])))
  kstest_pValue    <- Kstest$p.value
  kstest_Statistic <- Kstest$statistic*100

  kstest_result			 <- data.frame(kstest_Statistic,
                                 kstest_pValue)
  colnames(kstest_result)  <- c("KS Statistic (%)",
                                "P-Value")
  print(kstest_result)
  return(Kstest)
}
