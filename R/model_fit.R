#' @title model_fit
#'
#' @description function to fit a linear model
#'
#' @details you can use this function to fit for a linear model return a list of results.
#'
#' @param mf a dataframe of complete data
#' @param intercept an interger equals to 0 or 1. If 0, intercept should not be included in the model
#' @param model along with x, y, and qr are logicals. If TRUE the corresponding components of the fit (the model frame, the model matrix, the response, the QR decomposition) are returned.
#' @param offset 'offset' is a vector with the same length of data. If null, the data frame "mf" does not contain an offset column, else, the last column is offset vector
#' @return 'model_fit' returns a list of results containing the following components:
#' @return fitted.values: a n*1 matrix containing the fitted value
#' @return beta_hat: a n*1 matrix containing the fitted coefficients
#' @return coefficients: a n*1 matrix containing the residuals (observed values - fitted values)
#' @return residuals: a n*1 matrix containing the residuals (observed values - fitted values)
#' @return offset: if not null, a n*1 matrix containing the offset values.
#' @export
#' @examples x = as.data.frame(matrix(c(2,4,6,8,1.1,1.9,3.1,4.2,1,1,1,1),4,3)); m = model_fit(x,1,TRUE, TRUE, TRUE, TRUE, offset = c(1,1,1,1))

model_fit <- function (mf, intercept, model = TRUE, x=FALSE, y=FALSE, qr=TRUE, offset){
  result = list()
  completedata = as.matrix(mf)
  n_col = dim(completedata)[2]
  X_range = n_col
  if (! missing(offset)){
    offdata = completedata[,n_col]
    Y = completedata[,1]-offdata
    X_range = X_range-1
  }
  else{
    Y = completedata[,1]
  }
  if (intercept == 1){
    add_col = matrix(1,dim(completedata)[1],1)
    X = cbind(add_col, completedata[,2:X_range])
  }
  else{
    X = completedata[,2:X_range]
  }
  A = t(X)%*%X
  A_inv = solve(A)
  beta_hat = A_inv%*%(t(X))%*%Y
  H = X%*%A_inv%*%(t(X))
  y_hat = H%*%Y
  result$fitted.values= as.numeric(unlist(y_hat))
  result$coefficients = as.numeric(unlist(beta_hat))
  result$residuals = as.numeric(unlist(Y-y_hat))
  qrx = qr(X)
  effects = qr.qty(qrx, Y)
  result$effects = as.numeric(unlist(effects))
  result$rank = qr(X)$rank
  result$df.residual = dim(completedata)[1]-result$rank
  if (model == TRUE){
  }
  if (x == TRUE){
    result$x = X
  }
  if (y == TRUE){
    result$y = Y
  }
  if (qr == TRUE){
    result$qr = qrx
  }
  if (! missing(offset)){
    result$fitted.values = result$fitted.values+offdata
    result$offset = as.numeric(unlist(offdata))
  }
  result$assign = (abs(intercept-1):(result$rank-intercept))
  result$model = mf
  return (result)
}
