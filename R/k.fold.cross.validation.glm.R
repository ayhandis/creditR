#' @title K Fold Cross Validation Gini
#'
#' @description
#' This function allows to create k fold cross validation data sets.
#'
#' @param model_data A data set needs to be specified.
#' @param default_flag  The default flag needs to specified as string.
#' @param folds The number of folds desired needs to be specified.
#' @param seed_value A seed value needs to specified for replicability.
#' @keywords creditR
#' @import MLmetrics
#' @export
#' @examples
#' default_f <- c('1','0','0', '1','1','0','0','1','1')
#' birth_year <- c(1980, 1985, 1971,1971,1985,1971,1980,1980,1985)
#' job <- c(1,1,2, 2,2,3,3,2,3)
#' example_data <- data.frame(default_f,birth_year,job)
#' k.fold.cross.validation.glm(example_data, "default_f",10, 1)

k.fold.cross.validation.glm <- function(model_data, default_flag ,folds, seed_value){
  set.seed(seed_value)
  #Randomly shuffle the data
  model_data<-model_data[sample(nrow(model_data)),]

  #Create 10 equally size folds
  folds_data <- cut(seq(1,nrow(model_data)),breaks=folds,labels=FALSE)
  gini_fold_train <- matrix(data=NA,nrow=1,ncol=folds)
  gini_fold_test <- matrix(data=NA,nrow=1,ncol=folds)
  gini_foldname <- matrix(data=NA,nrow=1,ncol=folds)
  #Perform 10 fold cross validation
  for(i in 1:folds){
    #Segment your data by fold using the which() function
    testIndexes <- which(folds_data==i,arr.ind=TRUE)
    testData <- model_data[testIndexes, ]
    trainData <- model_data[-testIndexes, ]
    number <- which( colnames(trainData)==default_flag)
    ColNam <- names(model_data)
    ColNam<-ColNam[!ColNam %in% default_flag]
    foldmodel <- glm(formula = paste(default_flag,"~",paste(ColNam,collapse="+")), family = binomial(link = "logit"), data = trainData)
    gini_foldname[,i] <- i
    trainpred <- foldmodel$fitted.values
    testpred <- predict(foldmodel, type = 'response', newdata = testData)
    ginitrain <- Gini(trainpred, as.numeric(as.character(trainData[,number])))
    ginitest <- Gini(testpred, as.numeric(as.character(testData[,number])))
    gini_fold_train[,i] <- ginitrain
    gini_fold_test[,i] <- ginitest
    #Use the test and train data partitions however you desire...
  }
  foldresult <- cbind(t(gini_foldname), t(gini_fold_train), t(gini_fold_test))
  foldresult <- as.data.frame(foldresult)
  colnames(foldresult) <- c("Fold", "GiniTrain","GiniTest")
  foldresult$GiniTrain <- as.numeric(as.character(foldresult$GiniTrain))
  foldresult$GiniTest <- as.numeric(as.character(foldresult$GiniTest))
  foldresult <- rbind(foldresult, c("Average", sum(foldresult$GiniTrain)/folds, sum(foldresult$GiniTest)/folds))

  return(foldresult)


}

