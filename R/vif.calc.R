#' @title Variance Inflation Factor
#'
#' @description
#' This function allows the calculation VIF for a given model.
#'
#' @param mod A model needs to be specified.
#' @keywords creditR
#' @export
#' @examples
#' default_f <- c('1','0','0', '1','1','0','0','1','1')
#' birth_year <- c(1980, 1985, 1971,1971,1985,1971,1980,1980,1985)
#' job <- c(1,1,2, 2,2,3,3,2,3)
#' example_data <- data.frame(default_f,birth_year,job)
#' vif.calc(glm( formula  = default_f~., family = binomial(link = "logit"), data = example_data))


vif.calc <- function (mod)
{
  if (any(is.na(coef(mod))))
    stop("there are aliased coefficients in the model")
  v <- vcov(mod)
  assign <- attr(model.matrix(mod), "assign")
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  }
  else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2)
    stop("model contains fewer than 2 terms")
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs,
                                                                       -subs]))/detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1))
    result <- result[, 1]
  else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  result
}


