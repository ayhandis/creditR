#' @title Adjusted Herfindahl-Hirschman Index
#'
#' @description
#' This function allows the calculation of Adjusted-HHI for master scales.
#'
#' @param data A data set needs to be specified.
#' @param total_observations  The total observations variable needs to specified.
#' @keywords creditR
#' @export
#' @examples
#' Adjusted.Herfindahl.Hirschman.Index(master_scale_data, "Tot_obs")
Adjusted.Herfindahl.Hirschman.Index <- function (data, total_observations)
{
  data$SumTotal <- sum(data[,total_observations])
  data$concentration <- data[,total_observations] / data$SumTotal
  data$HHI <- data$concentration ^2
  print(data)
  HI <- (sum(data$HHI))
  AdjustedHHI <- (HI-1/length(data$SumTotal))/(1-1/length(data$SumTotal))
  return(AdjustedHHI)
}

