#' @title Calcuate the Population Stability Index for a Given Variable
#' @description
#' \code{psi} calculates the popolation stability index.
#'
#' @param main_data The main data set of a measurement, should be a factor or numeric
#' @param secon_data  The second data set of a measurement, should be a factor or numeric
#' @param variable A variable needs to be specified. It won't work if main data and second data are factors, and it cannot be NULL if main data and current data are numerical. This function uses this argument to bin \code{main_data} and \code{second_data} with left-closed right-open intervals.
#' @param bin The desired number of bins should be specified. Default value is 10.
#' @keywords creditR
#' @return a \code{psi} object
#'
#' @details psi measures the stablity of the population. Usually we can believe the population stays the same as the past if psi is less than 0.1, and a significant shift can be recognised if psi is greater than 0.25. The outcome of this function is a numeric, with details stored as attributes. You can use \code{summary} function to see all of the detailed information. Fot the situation where some of the levels has no element in either original population or current population and the psi does not exist for such levels, the empty levels will not be taken into account and a warning will inform you of this. Again, by using \code{summary} you could know everything inside.
#'
#' @examples
#' data("iris")
#' train <- sample(nrow(iris), nrow(iris) * .7)
#' train.species <- iris$Species[train]
#' test.species <- iris$Species[-train]
#' p <- PSI.calc(train.species, test.species)
#' p
#' summary(p)
#'
#' @import magrittr
#' @export


PSI.calc <- function(main_data, second_data, variable, bin = 10){
  psi <- function(original,
                  current,
                  cut.points = NULL,
                  cut.levels = ifelse(is.null(cut.points), 5L, NULL)) {

    if (!is.null(cut.points)) {
      cut.levels <- NULL
    }

    if (!is.null(cut.levels)) {
      if (cut.levels < 3) {
        warning("cut.levels must be an interger greater than 3")
        cut.levels <- 3L
      }
    }

    # binning numeric
    label.numeric <- function(x, cuts, na.level = NULL) {
      cuts <- sort(cuts)
      n_cut <- length(cuts)
      level.names <- paste0('<= ', cuts) %>% c(paste0('> ', cuts[n_cut]))

      if (any(is.null(na.level))) {
        na.level <- any(is.na(x))
      }
      if (na.level) level.names <- c('Missing', level.names)
      else {
        if(any(is.na(x))) stop('x has NA value while na.level is FALSE')
      }

      y <- vector('integer', length(x))

      for (i in n_cut:1) {
        y[x <= cuts[i]] <- i
      }
      y[x > cuts[n_cut]] <- n_cut + 1

      if (na.level) {
        y <- level.names[y + 1]
        y[is.na(x)] <- 'Missing'
      } else {
        y <- level.names[y]
      }

      factor(y, level.names)
    }


    # try to convert original & current to factor
    if (is.numeric(original) & is.numeric(current)) {
      if (is.null(cut.points)) {
        cut.points <- unname(quantile(original,
                                      seq(0, 1, length.out = cut.levels),
                                      type = 1,
                                      na.rm = TRUE))
        cut.points <- cut.points[-c(1, length(cut.points))]
      }
      na.level <- any(is.na(c(original, current)))
      # attr(res, 'original.num') <- original
      # attr(res, 'current.num') <- current
      original <- label.numeric(original, cut.points, na.level)
      current  <- label.numeric(current, cut.points, na.level)
    }

    if (!is.factor(original) | !is.factor(current)) {
      stop('original and current should be numeric or factor simultaneously.')
    }
    if (any(levels(original) != levels(current))) {
      common_lv <- union(levels(original), levels(current))
      original <- factor(original, levels = common_lv)
      current  <- factor(current,  levels = common_lv)

    }


    levels.name <- levels(original)
    org.stat.tbl <- tapply(X = original,
                           INDEX = original,
                           FUN = length,
                           simplify = TRUE) %>%
      sapply(function(x) ifelse(is.na(x), 0, x))
    cur.stat.tbl <- tapply(X = current,
                           INDEX = current,
                           FUN = length,
                           simplify = TRUE)%>%
      sapply(function(x) ifelse(is.na(x), 0, x))

    tbl <- data.frame(Levels = levels.name,
                      OrgCnt = org.stat.tbl,
                      CurCnt = cur.stat.tbl,
                      OrgPct = org.stat.tbl / sum(org.stat.tbl),
                      CurPct = cur.stat.tbl / sum(cur.stat.tbl))
    tbl$Index <- (tbl$CurPct - tbl$OrgPct) * log(tbl$CurPct / tbl$OrgPct)

    psi <- sum(tbl$Index[tbl$OrgCnt != 0 & tbl$CurCnt != 0])
    res <- psi
    tbl <- rbind(tbl, data.frame(Levels = 'Total',
                                 OrgCnt = sum(org.stat.tbl),
                                 CurCnt = sum(cur.stat.tbl),
                                 OrgPct = 1,
                                 CurPct = 1,
                                 Index = psi))
    rownames(tbl) <- NULL

    tbl$OrgPct <- round(tbl$OrgPct, 4)
    tbl$CurPct <- round(tbl$CurPct, 4)
    tbl$Index  <- round(tbl$Index,  4)

    attr(res, 'tbl') <- tbl
    # attr(res, 'original') <- original
    # attr(res, 'current')  <- current
    if (any(tbl$OrgCnt == 0 | tbl$CurCnt == 0)) {
      attr(res, 'Empty Levels') <- tbl$Levels[tbl$OrgCnt == 0 | tbl$CurCnt == 0] %>% as.character()
      warning('Some of the levels are empty, and PSI may be inaccurate. Please use `summary` to see the details.')
    }
    class(res) <- c('psi', 'numeric')
    res
  }


  #' @export
  print.psi <- function(x, ...) {
    cat('PSI :', round(x, 4), '\n')
    # NextMethod('print')
  }

  #' @export
  summary.psi <- function(object, ...) {
    cat('PSI:', round(object, 4), '\n\n')
    print(attr(object, 'tbl'))

    if (!is.null(attr(object, 'Empty Levels'))) {
      cat('\nEmpty Levels: ', paste0(attr(object, 'Empty Levels'), collapse = ', '), '\n')
    }
    # NextMethod('summary')
  }



  bin_range = as.data.frame(quantile(main_data[,variable], prob = seq(0, 1, length = (bin + 1)), type = 5))
  colnames(bin_range) = c("bound")
  bin_range$percentile= rownames(bin_range)
  bin_range <- bin_range[-1,]
  bin_range <- bin_range[-length(bin_range[,1]),]
  bound <- as.vector(bin_range$bound)
  PSI_result <- psi(main_data[,variable], second_data[,variable], cut.points = bound)
  print(summary(psi(main_data[,variable], second_data[,variable], cut.points = bound)))
  return(PSI_result[1])


}

