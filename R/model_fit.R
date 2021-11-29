#' @title model_fit
#'
#' @description function to fit a linear model
#'
#' @details you can use this function to fit for a linear model return a list of results.
#'
#' @param mf a named dataframe of complete data
#' @param intercept an interger equals to 0 or 1. If 0, intercept should not be included in the model
#' @param model logical. If TRUE the the model frame is returned.
#' @param x logical. If TRUE the the model matrix is returned.
#' @param y logical. If TRUE the response is returned.
#' @param qr logical. If TRUE the the QR is returned.
#' @param offset 'offset' is a vector with the same length of data. If null, the data frame "mf" does not contain an offset column, else, the offset column is offset vector
#' @param weights 'weights' is a vector with the same length of data. If null, the data frame "mf" does not contain an weights column, else, the weights column is offset vector
#' @return 'model_fit' returns a list of results containing the following components:
#' @return fitted.values: a n*1 matrix containing the fitted value
#' @return beta_hat: a n*1 matrix containing the fitted coefficients
#' @return coefficients: a n*1 matrix containing the residuals (observed values - fitted values)
#' @return residuals: a n*1 matrix containing the residuals (observed values - fitted values)
#' @return offset: if not null, a n*1 matrix containing the offset values.
#' @return weights: if not null, a n*1 matrix containing the weights values.
#' @export
#' @examples x = as.data.frame(matrix(c(2,4,6,8,1.1,1.9,3.1,4.2,1,1,1,1,1,2,3,4),4,4))
#' @examples names(x) <- c("O", "x", "(offsets)","(weights)")
#' @examples m = model_fit(x,1,TRUE, TRUE, TRUE, TRUE, offset = c(1,1,1,1), weights = c(1:4))

model_fit <- function (mf, intercept,
                       model = TRUE, x=FALSE, y=FALSE, qr=TRUE,
                       offset, weights){
  result = list()
  if (model == TRUE){
    result$model = mf
  }
  if (!missing(weights)){
    # if there are weights, extract weights from data
    if (!is.matrix(weights)){
      mf = subset(mf, select = -c(`(weights)`))
      result$weights = weights
    }
  }
  completedata = as.matrix(mf)
  n_col = dim(completedata)[2]
  n_row = dim(completedata)[1]
  X_range = n_col
  if (! missing(offset)){
    # if there are offsets, the last column of input would be offsets
    if (!is.matrix(offset)){
      offdata = completedata[,n_col]
      Y = completedata[,1]-offdata
      X_range = X_range-1
    }
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
  # create datavec for outcome(Y) and data matrix (X).
  if (!missing(weights)){
    if (!is.matrix(weights)){
      X_origin =  X
      Y_origin = Y
      rw = sqrt(weights)
      X = rw*X
      Y = rw*Y
      # if there are weights, the design matrix and outcome matrix to calculate the coeffieients should  times by sqrt(weight)

    }
  }
  TX = t(X)
  A = TX%*%X
  A_inv = solve(A)
  beta_hat = A_inv%*%(TX)%*%Y
  H = X%*%A_inv%*%(TX)
  # calculate the H matrix for linear regression.
  y_hat = H%*%Y
  result$fitted.values= as.numeric(unlist(y_hat))
  result$coefficients = as.numeric(unlist(beta_hat))
  result$residuals = as.numeric(unlist(Y-y_hat))
  qrx = qr(X)
  effects = qr.qty(qrx, Y)
  result$effects = as.numeric(unlist(effects))
  result$rank = qr(X)$rank
  result$df.residual = dim(completedata)[1]-result$rank
  if (x == TRUE){
    result$x = X
  }
  if (y == TRUE){
    result$y = Y
  }
  if (qr == TRUE){
    result$qr = qrx
  }
  if (!missing(weights)){
    if (!is.matrix(weights)){
      # if there are weights, extract weights from data
      real_fitted.value = X_origin%*%beta_hat
      result$fitted.values = as.numeric(unlist(real_fitted.value))
    }
  }
  if (! missing(offset)){
    if (!is.matrix(offset)){
      # if there are offsets, fitted values should add offsets
      result$fitted.values = result$fitted.values+offdata
      result$offset = as.numeric(unlist(offdata))
    }
  }
  result$residuals = as.numeric(unlist(completedata[,1]))-result$fitted.values
  assign_range1 = abs(intercept-1)
  assign_range2 = result$rank-intercept
  result$assign = (assign_range1:assign_range2)
  return (result)
}
