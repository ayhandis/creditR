#' @title Train and Test Balanced Split
#'
#' @description
#' This function allows you to separate your data set in a balanced manner as train and test.
#'
#' @param data A data set needs to be specified.
#' @param default_flag  The default flag need to specified as string.
#' @param balance_count Desired number of good ar bad observations need to specified.
#' @param seed_value A seed value need to be specified for replicability.
#' @param ratio  The percentage of train data should be specified.
#' @keywords creditR
#' @export
#' @examples
#' default_f <- c('1','0','0', '1','1','0','0','1','1')
#' birth_year <- c(1980, 1985, 1971,1971,1985,1971,1980,1980,1985)
#' PD <- c(0.1,0.12, 0.2, 0.23, 0.28,0.33,0.39,0.45, O.54)
#' example_data <- data.frame(default_f,birth_year,PD)
#' train_test_balanced_split(example_data,default_flag ="default_f", balance_count = 2,seed_value = 1, ratio = 0.90 )




train_test_balanced_split <- function(data,default_flag,balance_count,seed_value = 1, ratio){
  srswor <- function (n, N)
  {
    s <- rep(0, times = N)
    s[sample(N, n)] <- 1
    s
  }

  strata <- function (data, stratanames = NULL, size, method = c("srswor",
                                                                 "srswr", "poisson", "systematic"), pik, description = FALSE)
  {
    if (missing(method)) {
      warning("the method is not specified; by default, the method is srswor")
      method = "srswor"
    }
    if (!(method %in% c("srswor", "srswr", "poisson", "systematic")))
      stop("the method name is not in the list")
    if (method %in% c("poisson", "systematic") & missing(pik))
      stop("the vector of probabilities is missing")
    if (missing(stratanames) | is.null(stratanames)) {
      if (length(size) > 1)
        stop("the argument giving stratification variable is missing. The argument size should be a value.")
      if (method == "srswor")
        result = data.frame((1:nrow(data))[srswor(size, nrow(data)) ==
                                             1], rep(size/nrow(data), size))
      if (method == "srswr") {
        s = srswr(size, nrow(data))
        st = s[s != 0]
        l = length(st)
        result = data.frame((1:nrow(data))[s != 0])
        result = cbind.data.frame(result, st, prob = rep(1 -
                                                           (1 - 1/nrow(data))^size, l))
        colnames(result) = c("ID_unit", "Replicates", "Prob")
      }
      if (method == "poisson") {
        pikk = inclusionprobabilities(pik, size)
        s = (UPpoisson(pikk) == 1)
        if (length(s) > 0)
          result = data.frame((1:nrow(data))[s], pikk[s])
        if (description)
          cat("\nPopulation total and number of selected units:",
              nrow(data), sum(s), "\n")
      }
      if (method == "systematic") {
        pikk = inclusionprobabilities(pik, size)
        s = (UPsystematic(pikk) == 1)
        result = data.frame((1:nrow(data))[s], pikk[s])
      }
      if (method != "srswr")
        colnames(result) = c("ID_unit", "Prob")
      if (description & method != "poisson")
        cat("\nPopulation total and number of selected units:",
            nrow(data), sum(size), "\n")
    }
    else {
      data = data.frame(data)
      index = 1:nrow(data)
      m = match(stratanames, colnames(data))
      if (any(is.na(m)))
        stop("the names of the strata are wrong")
      data2 = cbind.data.frame(data[, m], index)
      colnames(data2) = c(stratanames, "index")
      x1 = data.frame(unique(data[, m]))
      colnames(x1) = stratanames
      result = NULL
      for (i in 1:nrow(x1)) {
        if (is.vector(x1[i, ]))
          data3 = data2[data2[, 1] == x1[i, ], ]
        else {
          as = data.frame(x1[i, ])
          names(as) = names(x1)
          data3 = merge(data2, as, by = intersect(names(data2),
                                                  names(as)))
        }
        y = sort(data3$index)
        if (description & method != "poisson") {
          cat("Stratum", i, "\n")
          cat("\nPopulation total and number of selected units:",
              length(y), size[i], "\n")
        }
        if (method != "srswr" & length(y) < size[i]) {
          stop("not enough obervations in the stratum ",
               i, "\n")
          st = c(st, NULL)
        }
        else {
          if (method == "srswor") {
            st = y[srswor(size[i], length(y)) == 1]
            r = cbind.data.frame(data2[st, ], rep(size[i]/length(y),
                                                  size[i]))
          }
          if (method == "systematic") {
            pikk = inclusionprobabilities(pik[y], size[i])
            s = (UPsystematic(pikk) == 1)
            st = y[s]
            r = cbind.data.frame(data2[st, ], pikk[s])
          }
          if (method == "srswr") {
            s = srswr(size[i], length(y))
            st = rep(y[s != 0], s[s != 0])
            l = length(st)
            r = cbind.data.frame(data2[st, ], prob = rep(1 -
                                                           (1 - 1/length(y))^size[i], l))
          }
          if (method == "poisson") {
            pikk = inclusionprobabilities(pik[y], size[i])
            s = (UPpoisson(pikk) == 1)
            if (any(s)) {
              st = y[s]
              r = cbind.data.frame(data2[st, ], pikk[s])
              if (description) {
                cat("Stratum", i, "\n")
                cat("\nPopulation total and number of selected units:",
                    length(y), length(st), "\n")
              }
            }
            else {
              if (description) {
                cat("Stratum", i, "\n")
                cat("\nPopulation total and number of selected units:",
                    length(y), 0, "\n")
              }
              r = NULL
            }
          }
        }
        if (!is.null(r)) {
          r = cbind(r, i)
          result = rbind.data.frame(result, r)
        }
      }
      if (method == "srswr")
        colnames(result) = c(stratanames, "ID_unit", "Prob",
                             "Stratum")
      else colnames(result) = c(stratanames, "ID_unit", "Prob",
                                "Stratum")
      if (description) {
        cat("Number of strata ", nrow(x1), "\n")
        if (method == "poisson")
          cat("Total number of selected units", nrow(result),
              "\n")
        else cat("Total number of selected units", sum(size),
                 "\n")
      }
    }
    result
  }

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
  train_balanced<- strata(train_test$train, stratanames = default_flag, size = c(balance_count,balance_count), method = "srswor")
  train_balanced_data <- train_test$train[train_balanced$ID_unit,]
  train_test$train_balanced = train_balanced_data
  return(train_test)
}




