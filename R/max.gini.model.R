#' @title Maximum Gini Model
#'
#' @description
#' This function allows the finding the model which gives the maximum gini value. Statistical requirements will not be provided. Can only be used to give an inference.
#'
#' @param data A data set needs to be specified.
#' @param default_flag  The default flag need to specified as string.
#' @param seed_value A seed value need to specified for replicability.
#' @keywords creditR
#' @import MLmetrics
#' @export
#' @examples
#' default_f <- c('1','0','0', '1','1','0','0','1','1')
#' birth_year <- c(1980, 1985, 1971,1971,1985,1971,1980,1980,1985)
#' job <- c(1,1,2, 2,2,3,3,2,3)
#' example_data <- data.frame(default_f,birth_year,job)
#' max.gini.model(example_data, "default_f",10)

max.gini.model <- function(data,default_flag, seed_value = 1){
  model.combinations <- function(data, default_flag, seed_value = 1){
    set.seed(seed_value)
    ColNam <- names(data)
    ColNam<-ColNam[!ColNam %in% default_flag]
    n<-length(ColNam)

    id1<-unlist(lapply(1:n,function(x)combn(1:n,x,simplify=F)),recursive=F)
    f1<-lapply(id1,function(x)
      paste(default_flag,"~",paste(ColNam[x],collapse="+")))
    res1<- lapply(f1,function(x) glm(as.formula(x),data=data, family = binomial(link = "logit")))
    return(res1)
  }
  models <- model.combinations(data,default_flag, seed_value)
  gini_model <- matrix(data=NA,nrow=1,ncol=length(models))
  gini_values <- matrix(data=NA,nrow=1,ncol=length(models))
  gini<-matrix(data=NA,nrow=1,ncol=length(models))
  for(i in c(1:length(models))){
    gini_model[,i] = i
    gini_values[,i] = Gini(y_pred = models[[i]]$fitted.values, y_true = as.numeric(as.character(data[,default_flag])))

  }
  gini<-cbind(t(gini_model), t(gini_values))
  gini_df <- as.data.frame(gini)
  colnames(gini_df) <- c("Model","Gini")
  gini_df$Gini <- as.numeric(as.character(gini_df$Gini))
  ordered_gini_df<-gini_df[order(-gini_df$Gini),]
  ordered_gini_df <- ordered_gini_df[ordered_gini_df[,1] != default_flag,]
  rownames(ordered_gini_df) <- c(1:length(ordered_gini_df[,1]))
  max_gini <- ordered_gini_df[1,1]
  return(models[[max_gini]])
}



