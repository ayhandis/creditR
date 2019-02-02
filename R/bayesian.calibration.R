#' @title Bayesian Calibration
#'
#' @description
#' This function allows to make model calibration for a model with bayesian method.
#'
#' @param data The master scale data needs to be specified.
#' @param average_score Average score variable in master scale data needs to be specified.
#' @param total_observations Total observations variable in master scale data needs to be specified.
#' @param PD PD variable in master scale data needs to be specified.
#' @param central_tendency The central tendency which is the calibration target needs to be specified.
#' @param calibration_data The scoring model data needs to be specified.
#' @param calibration_data_score The score variable in calibration data needs to be specified to apply calibration.
#' @keywords creditR
#' @export
#' @examples
#' bayesian.calibration(master_scale_data,"Score", "Total_obs", "PD", 0.06, model_data, "score")

bayesian.calibration <- function(data,average_score, total_observations, PD, central_tendency, calibration_data,calibration_data_score){
    numberavg <- which( colnames(data)==average_score)
    numbertot <- which( colnames(data)==total_observations)
    numberpd <-  which( colnames(data)==PD)
    avg_pd <- sum(data[,numbertot]*data[,numberpd])/sum(data[,numbertot])
    data["calibrated_pd"] <- (data[,numberpd]*central_tendency/avg_pd)/(data[,numberpd]*central_tendency/avg_pd+(1-data[,numberpd])*((1-central_tendency)/(1-avg_pd)))
    data["OddRatio"] <- (1-data$calibrated_pd)/data$calibrated_pd
    data <- as.data.frame(data)
    calibration_model <- lm(formula = log(data$OddRatio) ~ data[,numberavg], data = data)
    intercept <- calibration_model$coefficients[1]
    scorecoef <- calibration_model$coefficients[2]
    data[,numberpd] <- round(data[,numberpd],4)
    data$calibrated_pd <- round(data$calibrated_pd,4)
    data$OddRatio <- round(data$OddRatio,4)
    ns <-  which( colnames(calibration_data)==calibration_data_score)
    calibration_data$calibrated_pd <- round(1/(1+exp(intercept + calibration_data[,ns]*scorecoef)),4)
    calibrationlist <- list()
    calibrationlist$Calibration.model <- calibration_model
    calibrationlist$Calibration.formula <- paste0("Calibration method can be applied with: 1/(1+exp(Intercept + Score * Coefficient))  formula. Numerically : 1/(1+exp(",calibration_model$coefficients[1],"+Score *", calibration_model$coefficients[2],"))")
    calibrationlist$data <- data
    calibrationlist$calibration_data <- calibration_data
    return(calibrationlist)
}
