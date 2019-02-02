#' @title Univariate Gini
#'
#' @description
#' This function performs the calculation of the Gini from the estimated values calculated by logistic regression for a given data set. It should be noted that the Gini values calculated by logistic regression.
#'
#' @param data A data set needs to be specified.
#' @param default_flag  The default flag need to specified as string.
#' @keywords creditR
#' @import MLmetrics
#' @export
#' @examples
#' default_f <- c('1','0','0', '1','1','0','0','1','1')
#' birth_year <- c(1980, 1985, 1971,1971,1985,1971,1980,1980,1985)
#' job <- c(1,1,2, 2,2,3,3,2,3)
#' example_data <- data.frame(default_f,birth_year,job)
#' Gini.univariate.data(example_data, "default_f")


Gini.univariate.data <- function(data, default_flag){
  number <- which( colnames(data)==default_flag)
  gini_column_names <- matrix(data=NA,nrow=1,ncol=length(data))
  gini_values <- matrix(data=NA,nrow=1,ncol=length(data))
  gini<-matrix(data=NA,nrow=1,ncol=length(data))

  for(i in 1:length(data)){
    univ_model <- glm(formula = data[,number] ~ data[,i], family = binomial(link = "logit"), data = data)
    gini_values[,i]<- Gini(y_pred = univ_model$fitted.values, y_true = as.numeric(as.character(data[,default_flag])))
    gini_column_names[,i]<-colnames(data[i])
  }

  gini<-cbind(t(gini_column_names), t(gini_values))
  gini_df <- as.data.frame(gini)
  colnames(gini_df) <- c("Variable","Gini")
  gini_df$Gini <- as.numeric(as.character(gini_df$Gini))
  ordered_gini_df<-gini_df[order(-gini_df$Gini),]
  ordered_gini_df <- ordered_gini_df[ordered_gini_df[,1] != default_flag,]
  rownames(ordered_gini_df) <- c(1:length(ordered_gini_df[,1]))
  return(ordered_gini_df)


}

