#' @title Binomial Test
#'
#' @description
#' This function allows to perform binomial test on master scale data.
#'
#' @param rating_scale_data A master scale data set needs to be specified.
#' @param total_observations Total observations variable needs to be specified
#' @param PD PD variable needs to be specified.
#' @param DR Default rate variable needs to be specified.
#' @param confidence_level A confidence level needs to be set. Default value is 0.90.
#' @param tail Tail preference of the test needs to be specified.The default value is "one". The other value could be defined as "two".
#' @keywords creditR
#' @export
#' @examples
#' Binomial.test(master_scale_data,"Tot_obs", "PD", "Default_rate", 0.95, "two")

Binomial.test <- function(rating_scale_data, total_observations, PD,DR,confidence_level = 0.90, tail = "one"){
  if(tail =="one"){
    Binomial.test.one.tail <- function(rating_scale_data, total_observations, PD, DR, confidence_level = 0.90){
      rating_scale_data <- as.data.frame(rating_scale_data)
      numberto <- which( colnames(rating_scale_data)==total_observations )
      numberpd <- which( colnames(rating_scale_data)==PD )
      numberdr <- which( colnames(rating_scale_data)==DR )
      rating_scale_data$BadObs <- rating_scale_data[,numberto]* rating_scale_data[,numberdr]
      rating_scale_data$BadEstimations <- rating_scale_data[,total_observations]* rating_scale_data[,PD]
      Binomial_Table <- rating_scale_data
      Binomial_Table$TestEstimation  <- (Binomial_Table[,PD]*Binomial_Table[,total_observations]+qnorm(confidence_level)*sqrt((Binomial_Table[,PD]*Binomial_Table[,total_observations])*(1-Binomial_Table[,PD])))
      dif_TestEstimation <- as.numeric(Binomial_Table$TestEstimation) - as.numeric(Binomial_Table$BadObs)
      Binomial_Table$Test_Result <- cut(dif_TestEstimation,c(-Inf,0,Inf),
                                        labels = c("Target Value Underestimated",
                                                   "Target Value Correct"),
                                        include.lowest = TRUE, right = FALSE)

      return(Binomial_Table)

    }

    Binomial.test.one.tail(rating_scale_data, total_observations, PD, DR, confidence_level)
  }

  else if(tail == "two"){
    Binomial.test.two.tail <- function(rating_scale_data, total_observations, PD, DR, confidence_level = 0.90){
      rating_scale_data <- as.data.frame(rating_scale_data)
      numberto <- which( colnames(rating_scale_data)==total_observations )
      numberpd <- which( colnames(rating_scale_data)==PD )
      numberdr <- which( colnames(rating_scale_data)==DR )
      rating_scale_data$BadObs <- rating_scale_data[,numberto]* rating_scale_data[,numberdr]
      rating_scale_data$BadEstimations <- rating_scale_data[,total_observations]* rating_scale_data[,PD]
      Binomial_Table <- rating_scale_data
      Binomial_Table$TestEstimationUpper  <- (Binomial_Table[,PD]*Binomial_Table[,total_observations]+qnorm(confidence_level)*sqrt((Binomial_Table[,PD]*Binomial_Table[,total_observations])*(1-Binomial_Table[,PD])))
      Binomial_Table$TestEstimationLower  <- (Binomial_Table[,PD]*Binomial_Table[,total_observations]-qnorm(confidence_level)*sqrt((Binomial_Table[,PD]*Binomial_Table[,total_observations])*(1-Binomial_Table[,PD])))
      Binomial_Table$Test_Result <- ifelse(as.numeric(Binomial_Table$BadObs)> Binomial_Table$TestEstimationUpper, "Target Value Underestimated",
                                           ifelse(as.numeric(Binomial_Table$BadObs)<Binomial_Table$TestEstimationLower, "Target Value Overestimated","Target Value Correct"))

      return(Binomial_Table)

    }

    Binomial.test.two.tail(rating_scale_data, total_observations, PD, DR, confidence_level)





  }
  else print("Tail can not take any value other than one or two.")

}







