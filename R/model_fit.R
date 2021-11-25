#' @title model_fit
#'
#' @description function to fit a linear model
#'
#' @details you can use this function to fit for a linear model return a list of results.
#'
#' @param mf a dataframe of complete data
#' @return 'model_fit' returns a list of results containing the following components:
#' @return y_hat: a n*1 matrix containing the fitted value
#' @return beta_hat: a n*1 matrix containing the fitted coefficients
#' @return residuals: a n*1 matrix containing the residuals (observed values - fitted values)
#' @export
#' @examples x = as.data.frame(matrix(c(2,4,6,8,1.1,1.9,3.1,4.2),4,2)); model_fit(x)

model_fit <- function (mf)  {
  completedata = as.matrix(mf)
  add_col = matrix(1,dim(completedata)[1],1)
  X = cbind(add_col, completedata[,2:dim(completedata)[2]])
  Y = completedata[,1]
  A = t(X)%*%X
  A_inv = solve(A)
  beta_hat = A_inv%*%(t(X))%*%Y
  H = X%*%A_inv%*%(t(X))
  y_hat = H%*%Y
  result = list()
  result$fitted.values= as.numeric(unlist(y_hat))
  result$coefficients = as.numeric(unlist(beta_hat))
  result$residuals = as.numeric(unlist(Y-y_hat))
  result$qr = qr(X)
  effects = qr.qty(result$qr, y)
  result$effects = as.numeric(unlist(effects))
  result$rank = result$qr$rank
  result$df.residual = dim(completedata)[1]-result$rank
  result$model = as.data.frame(completedata)
  return (result)
}
