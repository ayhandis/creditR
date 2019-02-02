#' @title Univariate Gini
#'
#' @description
#' This function performs the calculation of the Gini (by time) from the estimated values calculated by logistic regression for a given data set. It should be noted that the Gini values calculated by logistic regression.
#'
#' @param data A data set needs to be specified.
#' @param default_flag  The default flag need to specified as string.
#' @param PD The PD variable need to be specified as string
#' @param time The time variable need to be specified as string
#' @keywords creditR
#' @import MLmetrics
#' @export
#' @examples
#' default_f <- c('1','0','0', '1','1','0','0','1','1')
#' birth_year <- c(1980, 1985, 1971,1971,1985,1971,1980,1980,1985)
#' job <- c(1,1,2, 2,2,3,3,2,3)
#' pd <- c(0.5, 0.2,0.4,0.5,0.7,0.9,0.2,0.3,0.3,0.6)
#' example_data <- data.frame(default_f,birth_year,job, pd)
#' time.series.gini(example_data, "default_f","pd","birth_year")
time.series.gini <- function(data,default_flag,PD,time){
  times <- matrix(data=NA,nrow=1,ncol=length(unique(data[,time])))
  time_gini <- matrix(data=NA,nrow=1,ncol=length(unique(data[,time])))
  for(i in 1:length(unique(data[,time]))){
    time_data <- subset(data,data[,time] == unique(data[,time])[i])
    times[,i] = unique(data[,time])[i]
    time_gini[,i] = Gini(y_pred = time_data[,PD], y_true = as.numeric(as.character(time_data[,default_flag])))
  }
 time_return = as.data.frame(cbind(t(times), t(time_gini)))
 colnames(time_return) <- c("time", "Gini")
 time_return <- rbind(time_return, c("Average", sum(time_return$Gini)/length(time_return$Gini)))
 return(time_return)
}
