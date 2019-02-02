#' @title Herfindahl-Hirschman Index
#'
#' @description
#' This function allows the calculation of HHI for master scales.
#'
#' @param data A data set needs to be specified.
#' @param total_observations  The total observations variable needs to specified.
#' @keywords creditR
#' @export
#' @examples
#' Herfindahl.Hirschman.Index(master_scale_data, "Tot_obs")

Herfindahl.Hirschman.Index <- function (data, total_observations)
{
  data$SumTotal <- sum(data[,total_observations])
  data$concentration <- data[,total_observations] / data$SumTotal
  data$HHI <- data$concentration ^2
  print(data)
  return(sum(data$HHI))
}

