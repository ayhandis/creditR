#' @title Chi Square Test
#'
#' @description
#' This function allows to perform chi square test on master scale data.
#'
#' @param data A master scale data set needs to be specified.
#' @param PD PD variable needs to be specified.
#' @param observed_bad Observed defaults needs to be specified.
#' @param total_observations Total observations variable needs to be specified
#' @param confidence_level A confidence level needs to be set. Default value is 0.95.
#' @keywords creditR
#' @export
#' @examples
#' chisquare.test(master_scale_data, "PD","Bad_obs","Tot_obs",0.90)

chisquare.test <- function(data, PD, observed_bad, total_observations, confidence_level = 0.95){
  data$expected.bad <- data[,total_observations] * data[,PD]
  data$chi.square <- (((data$expected.bad - data[,observed_bad])^2)/ data$expected.bad)
  chi2 <- sum(((data$expected.bad - data[,observed_bad])^2)/ data$expected.bad)
  pchi <- pchisq(chi2,df=1,lower.tail=TRUE)
  cl <- 1-confidence_level
  result <- ifelse(pchi > cl, paste0("The rating scale did not pass the test ",round(pchi,3), " > ",cl), paste0("The rating scale passed the test.", round(pchi,3), " < ",cl))
  returnchisq <- list()
  returnchisq$data = data
  returnchisq$p_value <- pchi
  returnchisq$result <- result
  return(returnchisq)
  print(result)
}

