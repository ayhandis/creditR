#' @title Adjusted Binomial Test
#'
#' @description
#' This function allows to perform adjusted binomial test on master scale data.
#'
#' @param rating_scale_data A master scale data set needs to be specified.
#' @param total_observations Total observations variable needs to be specified
#' @param PD PD variable needs to be specified.
#' @param DR Default rate variable needs to be specified.
#' @param confidence_level A confidence level needs to be set. Default value is 0.90.
#' @param tail Tail preference of the test needs to be specified.The default value is "one". The other value could be defined as "two".
#' @param r The default correlation between rating grades needs to be specified. The default value is 0.40.
#' @keywords creditR
#' @export
#' @examples
#' Adjusted.Binomial.test(master_scale_data,"Tot_obs", "PD", "Default_rate", 0.95, "two", 0.30)

Adjusted.Binomial.test <- function(rating_scale_data, total_observations, PD,DR,confidence_level = 0.90, tail = "one", r = 0.40){
  if(tail =="one"){
    Adjusted.Binomial.test.one.tail <- function(rating_scale_data, total_observations, PD, DR, confidence_level = 0.90, r = 0.40){
      rating_scale_data <- as.data.frame(rating_scale_data)
      numberto <- which( colnames(rating_scale_data)==total_observations )
      numberpd <- which( colnames(rating_scale_data)==PD )
      numberdr <- which( colnames(rating_scale_data)==DR )
      rating_scale_data$BadObs <- rating_scale_data[,numberto]* rating_scale_data[,numberdr]
      rating_scale_data$BadEstimations <- rating_scale_data[,total_observations]* rating_scale_data[,PD]
      Binomial_Adjusted_Table <- rating_scale_data
      Binomial_Adjusted_Table["Default_Correlation"] = r
      t <- qnorm(as.numeric(Binomial_Adjusted_Table[,PD]))
      Q_param <- pnorm((sqrt(r)*qnorm(confidence_level)+t)/sqrt(1-r))
      Adjusted_result	<- Q_param + 1/(2*as.numeric(Binomial_Adjusted_Table[,total_observations]))*(2*Q_param - 1 + (Q_param*(1-Q_param))/dnorm((sqrt(r)*qnorm(1-confidence_level)-t)/sqrt(1-r))*((2*r-1)*qnorm(1-confidence_level)-t*sqrt(r))/sqrt(r*(1-r)))
      Binomial_Adjusted_Table$TestEstimation <- round(Adjusted_result*as.numeric(Binomial_Adjusted_Table[,total_observations]), 2)
      dif_TestEstimation <- as.numeric(Binomial_Adjusted_Table$TestEstimation) - as.numeric(Binomial_Adjusted_Table$BadObs)
      Binomial_Adjusted_Table$Test_Result <- cut(dif_TestEstimation,c(-Inf,0,Inf),
                                                 labels = c("Target Value Underestimated",
                                                            "Target Value Correct"),
                                                 include.lowest = TRUE, right = FALSE)
      return(Binomial_Adjusted_Table)

    }


    Adjusted.Binomial.test.one.tail(rating_scale_data, total_observations, PD, DR, confidence_level,r)
  }

  else if(tail == "two"){

    Adjusted.Binomial.test.two.tail <- function(rating_scale_data, total_observations, PD, DR, confidence_level = 0.90, r = 0.40){
      rating_scale_data <- as.data.frame(rating_scale_data)
      numberto <- which( colnames(rating_scale_data)==total_observations )
      numberpd <- which( colnames(rating_scale_data)==PD )
      numberdr <- which( colnames(rating_scale_data)==DR )
      rating_scale_data$BadObs <- rating_scale_data[,numberto]* rating_scale_data[,numberdr]
      rating_scale_data$BadEstimations <- rating_scale_data[,total_observations]* rating_scale_data[,PD]
      Binomial_Adjusted_Table <- rating_scale_data
      Binomial_Adjusted_Table["Default_Correlation"] = r
      t <- qnorm(as.numeric(Binomial_Adjusted_Table[,PD]))
      Q_param <- pnorm((sqrt(r)*qnorm(confidence_level)+t)/sqrt(1-r))
      Adjusted_resultUpper	<- Q_param + 1/(2*as.numeric(Binomial_Adjusted_Table[,total_observations]))*(2*Q_param - 1 + (Q_param*(1-Q_param))/dnorm((sqrt(r)*qnorm(1-confidence_level)-t)/sqrt(1-r))*((2*r-1)*qnorm(1-confidence_level)-t*sqrt(r))/sqrt(r*(1-r)))
      Adjusted_resultLower	<- Q_param + 1/(2*as.numeric(Binomial_Adjusted_Table[,total_observations]))*(2*Q_param - 1 - (Q_param*(1-Q_param))/dnorm((sqrt(r)*qnorm(1-confidence_level)-t)/sqrt(1-r))*((2*r-1)*qnorm(1-confidence_level)-t*sqrt(r))/sqrt(r*(1-r)))
      Binomial_Adjusted_Table$TestEstimationUpper <- round(Adjusted_resultUpper*as.numeric(Binomial_Adjusted_Table[,total_observations]), 2)
      Binomial_Adjusted_Table$TestEstimationLower <- round(Adjusted_resultLower*as.numeric(Binomial_Adjusted_Table[,total_observations]), 2)
      Binomial_Adjusted_Table$Test_Result <- ifelse(as.numeric(Binomial_Adjusted_Table$BadObs)> Binomial_Adjusted_Table$TestEstimationUpper, "Target Value Underestimated",
                                                    ifelse(as.numeric(Binomial_Adjusted_Table$BadObs)<Binomial_Adjusted_Table$TestEstimationLower, "Target Value Overestimated","Target Value Correct"))

      return(Binomial_Adjusted_Table)

    }


    Adjusted.Binomial.test.two.tail(rating_scale_data, total_observations, PD, DR, confidence_level,r)





  }
  else print("Tail can not take any value other than one or two.")

}





