#' @title linear_regression
#'
#' @description main function to return result of lm()
#'
#' @details you can use this function to do linear regression and return a list of results.
#'
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment containing the variables in the model. If not found in data, the variables are taken from environment(formula)
#' @param subset optional vector specifying a subset of observations to be used in the fitting process.
#' @param weights this can be used to specify an a priori known component weights of predictors to be included in the fitting. This should be NULL or a numeric vector or matrix of extents matching those of the response.
#' @param model logical. If TRUE the the model frame is returned.
#' @param x logical. If TRUE the the model matrix is returned.
#' @param y logical. If TRUE the response is returned.
#' @param qr logical. If TRUE the the QR is returned.
#' @param offset this can be used to specify an a priori known component to be included in the linear predictor during fitting. This should be NULL or a numeric vector or matrix of extents matching those of the response.
#' @return 'linear_regression' returns a list of results containing at least the following components:
#' @return coefficients: a named vector of coefficients
#' @return residuals: the residuals, that is response minus fitted values.
#' @return fitted.values: the fitted mean values.
#' @return rank: the numeric rank of the fitted linear model.
#' @return df.residual: the residual degrees of freedom.
#' @return call: the matched call.
#' @return y: if requested, the response used.
#' @return x: if requested, the model matrix used.
#' @return model: if requested (the default), the model frame used.
#' @return offset: if not null, a n*1 matrix containing the offset values.
#' @return weights: if not null, a n*1 matrix containing the weights values.
#' @return terms: the formula of fitted model.
#' @export
#' @examples #NOT RUN
#' @examples data_range = matrix(c(0,0,0,1,1,1),3,2)
#' @examples offsets_range = c(0,1)
#' @examples weights_range = c(0,1)
#' @examples coefficients = c(1,2,3)
#' @examples data = simulate_data(10, 3, error_range = c(0,1), data_range, offsets_range, weights_range,coefficients)
#' @examples model = linear_regression(V1~V2+V3+V4, data = data, weights = data$`(weights)`,offset = data$`(offsets)`)



linear_regression <- function (formula, data, subset, weights,
                             model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
                             offset)  {
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
  result = model_fit(mf, intercept, model, x, y, qr, offset, weights)
  result$terms = formula
  return (result)
}

