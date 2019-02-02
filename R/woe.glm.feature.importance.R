#' @title Woe Glm Feature Importance
#'
#' @description
#' This function allows the calculate feature importance for a given glm model with WOE transformation method.
#'
#' @param model_data A model data set needs to be specified.
#' @param model A GLM model built by WOE transformation should be given.
#' @param default_flag  The default flag need to specified as string.
#' @keywords creditR
#' @export
#' @examples
#' woe.glm.feature.importance(model_data_v1, my_model,"default_f")

woe.glm.feature.importance <- function(model_data, model, default_flag){
  model_importance_std <- matrix(data=NA, ncol=1, nrow = (length(model_data)-1))
  model_importance_colnames <- matrix(data=NA, ncol=1, nrow = (length(model_data)-1))
  number <- which(colnames(model_data) == default_flag)
  imp_data <- model_data[,-number]
  coef <- as.data.frame(model$coefficients)
  for(i in 1:length(imp_data)){
    colname <- colnames(imp_data[i])
    number_row <- which(rownames(coef) == colname)
    model_importance_std[i,] <-  coef[number_row,]*sd(imp_data[,colname])
    model_importance_colnames[i,] <- colnames(imp_data[i])
  }
  return_data <- as.data.frame(cbind(model_importance_colnames, model_importance_std))
  colnames(return_data) <- c("Variables", "beta_std")
  return_data$beta_std <- as.numeric(as.character(return_data$beta_std))
  return_data$importance <- return_data$beta_std / sum(return_data$beta_std)
  return(return_data)

}




