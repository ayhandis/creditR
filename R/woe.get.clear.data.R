#' @title Get Clear Data After WOE binning
#'
#' @description
#' This function allows you to get a clear data after using woe.binning.deploy function.
#'
#' @param data A data set needs to be defined.
#' @param default_flag Default flag must be specified as a string.
#' @param prefix Prefix must be specified as a string. (e.g. ("woe."))
#' @keywords creditR
#' @export
#' @examples
#' woe.get.clear.data(example_data, "default_f","woe.")

woe.get.clear.data <- function(data,default_flag, prefix){
  number <- which( colnames(data)==default_flag )
  data_clear <- data[, grepl(prefix, names(data))]
  data_clear[,default_flag] = data[,number]
  return(data_clear)
}

