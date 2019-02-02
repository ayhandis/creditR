#' @title Anchor Point
#'
#' @description
#' This function allows the make Anchor point test for a given master scale data.
#' @param master_scale_data A master scale data set needs to be specified.
#' @param PD  A PD variable needs to be specified as string.
#' @param total_observations A total observations variable needs to be specified as string.
#' @param central_tendecy A  central tendency needs to be specified for testing.
#' @param upper_green An upper green threshold needs to be defined. The default value is 1.2.
#' @param upper_red An upper red threshold needs to be defined. The default value is 1.3.
#' @param lower_green A lower green threshold needs to be defined. The default value is 0.8.
#' @param lower_red A lower red threshold needs to be defined. The default value is 0.7.
#' @keywords creditR
#' @export
#' @examples
#' Anchor.point(final_scale, "PD","Tot_obs", 0.6)

Anchor.point <- function(master_scale_data, PD, total_observations, central_tendency, upper_green = 1.2, upper_red = 1.3, lower_green = 0.8, lower_red = 0.7 ){
  master_scale_data$concentration <- master_scale_data[,total_observations]/sum(master_scale_data[,total_observations])
  avg_pd <- sum(master_scale_data[,PD] * master_scale_data[,total_observations])/sum(master_scale_data[,total_observations])
  upper_g = avg_pd * upper_green
  upper_r = avg_pd * upper_red
  lower_g = avg_pd * lower_green
  lower_r = avg_pd * lower_red
  data <- as.data.frame(cbind(central_tendency, avg_pd, lower_r, lower_g, upper_g, upper_r))
  data$test_result <- ifelse(central_tendency > lower_g & central_tendency < upper_g, "Green", ifelse(central_tendency < lower_r || central_tendency > upper_r, "Red","Yellow"))
  return(data)
}



