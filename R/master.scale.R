#' @title Master Scale
#'
#' @description
#' This function creates a master scale that best describes the target variable according to the given parameters.
#'
#' @param data A data set needs to be specified including PD variable.
#' @param default_flag  The default flag need to specified as string.
#' @param PD PD variable need to be specified.
#' @param stop.limit Stops WOE based merging of the predictor's classes/levels in case the resulting information value (IV) decreases more than x percent (e.g. 0.05 = 5 percent) compared to the preceding binning step. stop.limit=0 will skip any WOE based merging. Increasing the stop.limit will simplify the binning solution and may avoid overfitting. Accepted range: 0-0.5; default: 0.1.
#' @param min.perc.total For numeric variables this parameter defines the number of initial classes before any merging is applied. For example min.perc.total=0.05 (5 percent) will result in 20 initial classes. For factors the original levels with a percentage below this limit are collected in a 'miscellaneous' level before the merging based on the min.perc.class and on the WOE starts. Increasing the min.perc.total parameter will avoid sparse bins. Accepted range: 0.0001-0.2; default: 0.05.
#' @param min.perc.class If a column percentage of one of the target classes within a bin is below this limit (e.g. below 0.01=1 percent) then the respective bin will be joined with others. In case of numeric variables adjacent predictor classes are merged. For factors respective levels (including sparse NAs) are assigned to a 'miscellaneous' level. Setting min.perc.class>0 may provide more reliable WOE values. Accepted range: 0-0.2; default: 0, i.e. no merging with respect to sparse target classes is applied.
#' @keywords creditR
#' @import woeBinning
#' @export
#' @examples
#' master.scale(example_data, "default_f","Probability")


master.scale <- function(data,default_flag, PD, stop.limit = 0.0, min.perc.total = 0.025, min.perc.class = 0.01){
  binrules <- woe.binning(df = data, target.var = default_flag, pred.var = PD, stop.limit= stop.limit, event.class = 1, min.perc.total = min.perc.total, min.perc.class = min.perc.class)
  bintable <- woe.binning.table(binrules)
  df_rating <- bintable[[1]]
  df_rating <- df_rating[-length(df_rating[,1]),]
  deploy_pd <- as.data.frame(woe.binning.deploy(data,binrules))
  number_pd <- which( colnames(data)== PD )
  avg_pd <- aggregate(deploy_pd[,number_pd]~deploy_pd[,length(deploy_pd)], deploy_pd, mean)
  df_rating$PD <- round(avg_pd[,2],5)
  df_rating$Score <- log(df_rating$PD/(1-df_rating$PD))
  returnlist <- list()
  df_rating = cbind(df_rating[,1:8],df_rating[,11], df_rating[,12])
  colnames(df_rating) <- c("Final.PD.Range", "Total.Observations", "Total.Distr","Good.Count","Bad.Count","Good.Distr","Bad.Distr","Bad.Rate","PD","Score")
  return(df_rating)
}


