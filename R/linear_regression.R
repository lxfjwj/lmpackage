#' @title linear_regression
#'
#' @description main function to return result of lm()
#'
#' @details you can use this function to do linear regression and return a list of results.
#'
#' @param fomula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment containing the variables in the model. If not found in data, the variables are taken from environment(formula)
#' @param subset optional vector specifying a subset of observations to be used in the fitting process.
#' @return 'linear_regression' returns a list of results containing at least the following components:
#' @return coefficients: a named vector of coefficients
#' @return residuals: the residuals, that is response minus fitted values.
#' @return fitted.values: the fitted mean values.
#' @return rank: the numeric rank of the fitted linear model.
#' @return df.residual: the residual degrees of freedom.
#' @return call: the matched call.
#' @return y:if requested, the response used.
#' @return x: if requested, the model matrix used.
#' @return model: if requested (the default), the model frame used.
#' @export
#' @examples x=1;linear_regression(x)

linear_regression <- function (formula, data, subset, weights, na.action, method = "qr",
                             model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
                             contrasts = NULL, offset, ...)  {
  ret.x <- x
  ret.y <- y
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(x = c("formula", "data", "subset", "weights", "na.action", "offset"),
             table = names(mf), nomatch = 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(expr = mf, envir = parent.frame())
  # get fulldata matrix: mf
  term = attr(mf,"terms")
  intercept = attr(term,"intercept")
  result = model_fit(mf, intercept)
  result$formula = formula
  return (result)
}

