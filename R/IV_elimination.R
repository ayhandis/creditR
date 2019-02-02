#' @title Information Value Elimination
#'
#' @description
#' This function allows you to eliminate variables which have information value less than a given threshold for a given data set.
#'
#' @param data A data set needs to be defined.
#' @param iv_threshold The IV threshold must be specified in order to make an elimination.
#' @param default_flag Default flag must be specified as a string.
#' @keywords creditR
#' @export
#' @examples
#' default_f <- c('1','0','0', '1','1','0','0','1','1')
#' birth_year <- c(1980, 1985, 1971,1971,1985,1971,1980,1980,1985)
#' job <- c(1,1,2, 2,2,3,3,2,3)
#' example_data <- data.frame(default_f,birth_year,job)
#' IV_elimination(example_data,"default_f" ,0.50)

IV_elimination <- function(data, default_flag,iv_threshold){
  IV.calc.data <- function(data,default_flag){
    IV.calc <- function(data, default_flag,variable){
      number <- which( colnames(data)==default_flag )
      good = subset(data,data[,number] == "0")
      bad = subset(data, data[,number] == "1")
      x <- table(good[,variable])
      y <- as.data.frame(x)
      z <- table(bad[,variable])
      u <- as.data.frame(z)
      merged = merge(x = y, y = u, by="Var1", all = TRUE)
      merged$percentx = merged$Freq.x/(sum(merged$Freq.x))
      merged$percenty = merged$Freq.y/(sum(merged$Freq.y))
      merged$IV = (merged$percentx-merged$percenty)*(log(merged$percentx/merged$percenty))
      IV_RESULT <- sum(merged$IV)
      return(IV_RESULT)
    }
    iv_column_names <- matrix(data=NA,nrow=1,ncol=length(data))
    Information_Values <- matrix(data=NA,nrow=1,ncol=length(data))
    for(i in 1:length(data)){
      Information_Value <- IV.calc(data,default_flag,colnames(data)[i])
      Information_Values[,i] <- Information_Value
      iv_column_names[,i] <- colnames(data)[i]
    }
    iv_summary<-as.data.frame(cbind(t(iv_column_names), t(Information_Values)))
    colnames(iv_summary) <- c("Variable","IV")
    iv_summary$IV <- as.numeric(as.character(iv_summary$IV))
    iv_summary <- iv_summary[iv_summary[,1] != default_flag,]
    rownames(iv_summary) <- c(1:length(iv_summary[,1]))
    return(iv_summary)
  }
  iv_table <- IV.calc.data(data,default_flag)
  elimination_list <- as.vector(subset(iv_table, IV < iv_threshold)$Variable)
  eliminated_data <- data[,!names(data) %in% elimination_list]

  return(eliminated_data)
}
