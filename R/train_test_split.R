#' @title Train Test Split
#'
#' @description
#' This function separates the given data set as train and test dataaccording to the determined seed value and ratio. .
#'
#' @param data The data set which is intended to be seperated into train and test should be specified.
#' @param seed_value In order to ensure repeatability, random number must be assigned in a particular order. The seed value must be specified in order to perform the repeatability process.
#' @param ratio The percentage of train data should be specified.
#' @keywords creditR
#' @export
#' @examples
#' random_column <- data.frame(runif(100, min = 0, max = 1000))
#' colnames(random_column) = "random_column"
#' datasets <- train_test_split(random_column, seed_value = 1, ratio = 0.70)
#' train = datasets$train
#' test = datasets$test

train_test_split <- function(data,seed_value = 1,ratio){
  set.seed(seed_value)
  train_test <- list()
  sample.split <- function (Y, SplitRatio = 2/3, group = NULL)
  {
    nSamp = length(Y)
    nGroup = length(group)
    if (nGroup > 0 && nGroup != nSamp)
      stop("Error in sample.split: Vectors 'Y' and 'group' have to have the same length")
    BinOne = logical(nSamp)
    SplitRatio = abs(SplitRatio)
    if (SplitRatio >= nSamp)
      stop("Error in sample.split: 'SplitRatio' parameter has to be i [0, 1] range or [1, length(Y)] range")
    U = unique(Y)
    nU = length(U)
    if (2 * nU > nSamp | nU == 1) {
      n = if (SplitRatio >= 1)
        SplitRatio
      else SplitRatio * nSamp
      rnd = runif(nSamp)
      if (nGroup)
        split(rnd, group) <- lapply(split(rnd, group), mean)
      ord = order(rnd)
      BinOne[ord[1:n]] = TRUE
    }
    else {
      rat = if (SplitRatio >= 1)
        SplitRatio/nSamp
      else SplitRatio
      for (iU in 1:nU) {
        idx = which(Y == U[iU])
        n = round(length(idx) * rat)
        rnd = runif(length(idx))
        if (nGroup) {
          grp = group[idx]
          split(rnd, grp) <- lapply(split(rnd, grp), mean)
        }
        ord = order(rnd)
        BinOne[idx[ord[1:n]]] = TRUE
      }
    }
    if (SplitRatio >= 1) {
      n = sum(BinOne) - SplitRatio
      if (n > 0)
        BinOne[sample(which(BinOne), n)] = FALSE
      else if (n < 0)
        BinOne[sample(which(!BinOne), -n)] = TRUE
    }
    return(BinOne)
  }
  sample = sample.split(data[,1], SplitRatio = ratio)
  train_test$train = subset(data, sample == TRUE)
  train_test$test  = subset(data, sample == FALSE)
  return(train_test)
}


