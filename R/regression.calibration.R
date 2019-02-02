#' @title Regression Calibration
#'
#' @description
#' This function allows to make model calibration for a model with logistic regression.
#'
#' @param model The scoring model needs to be specified.
#' @param calibration_data A calibration data needs to be specified.
#' @param default_flag  The default flag need to specified as string.
#' @keywords creditR
#' @export
#' @examples
#' regression.calibration(scoring_model, calibration_sample, "default_f")

regression.calibration <- function(model,calibration_data, default_flag){
  number <- which( colnames(calibration_data)==default_flag )
  calibration_data$modelpd = predict(model, type = 'response', newdata = calibration_data)
  calibration_data$modelscore = log(calibration_data$modelpd/(1-calibration_data$modelpd))
  calibration_model = glm(formula = calibration_data[,number] ~ calibration_data$modelscore, family = binomial(link = "logit"), data = calibration_data)
  calibration_data$calibrated_pd = calibration_model$fitted.values
  calibration_data$calibrated_score = log(calibration_data$calibrated_pd/(1-calibration_data$calibrated_pd))

  varNames <- colnames(model.matrix(model))
  equationStr <- paste(round(coef(model),7),varNames,sep="*",collapse=" + ")
  equationStr <- gsub("*(Intercept)","",equationStr,fixed=TRUE)
  equationStr <- paste(model$terms[[2]],"=",equationStr)

  varNamescal <- colnames(model.matrix(calibration_model))
  equationStrcal <- paste(round(coef(calibration_model),7),varNames,sep="*",collapse=" + ")
  equationStrcal <- gsub("*(Intercept)","",equationStrcal,fixed=TRUE)
  equationStrcal <- paste(model$terms[[2]],"=",equationStrcal)
  calibration_formula <- paste0("modelscore formula ::: ", equationStr,"calibrated_score formula :::",equationStrcal, "Calibration Formula to get calibrated_pd ::: 1/(1 + exp(-calibrated_score)) ")
  return_list <- list()
  return_list$calibration_data <- calibration_data
  return_list$calibration_model <- calibration_model
  return_list$calibration_formula <- calibration_formula
  return(return_list)
}


