#' @title Gini Elimination
#'
#' @description
#' This function allows you to eliminate variables which have Gini less than a given threshold for a given data set.
#'
#' @param data A data set needs to be defined.
#' @param gini_threshold The IV threshold must be specified in order to make an elimination.
#' @param default_flag Default flag must be specified as a string.
#' @keywords creditR
#' @import MLmetrics
#' @export
#' @examples
#' default_f <- c('1','0','0', '1','1','0','0','1','1')
#' birth_year <- c(1980, 1985, 1971,1971,1985,1971,1980,1980,1985)
#' job <- c(1,1,2, 2,2,3,3,2,3)
#' example_data <- data.frame(default_f,birth_year,job)
#' Gini_elimination(example_data,"default_f" ,0.15)


Gini_elimination <- function(data, default_flag, gini_threshold){
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

  gini_table <- Gini.univariate.data(data,default_flag)
  elimination_list <- as.vector(subset(gini_table, Gini < gini_threshold)$Variable)
  eliminated_data <- data[,!names(data) %in% elimination_list]
  return(eliminated_data)
}

