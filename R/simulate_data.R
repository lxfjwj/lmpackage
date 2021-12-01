#' @title simulate_data
#'
#' @description function to generate simulation data for test
#'
#' @details you can use this function to create random datasets for testing.
#'
#' @param datasize1 a integer represents the amounts of every variables .
#' @param datasize2 a integer represents the numbers of variables.
#' @param error_range the range of errors of outcomes. This should be NULL or a numeric vector of size datasize1
#' @param data_range a datasize2*2 matrix: every row represents the range of every variable.
#' @param offsets_range the range of expected offsets. This should be NULL or a numeric vector of size datasize1.
#' @param weights_range the range of expected weights. This should be NULL or a numeric vector of size datasize1.
#' @param coefficients a numeric vector with length datasize1: the simulated coefficient for every variable.
#' @return 'simulate' returns a dataframe of results containing the following components:
#' @return V1: the simulated Outcome. It should be a numeric vector with length of datasize1
#' @return V2, V3,...: the simulated Predictors. It should be a numeric vector with length of datasize1
#' @return (weights): If not NULL, the simulated weights. It should be a numeric vector with length of datasize1
#' @return (offsets): If not NULL, the simulated offsets. It should be a numeric vector with length of datasize1
#' @export
#' @examples data_range = matrix(c(0,0,0,1,1,1),3,2)
#' @examples offsets_range = c(0,1)
#' @examples weights_range = c(0,1)
#' @examples coefficients = c(1,2,3)
#' @examples data = simulate_data(10, 3, error_range= c(0,1), data_range, offsets_range, weights_range,coefficients)


simulate_data = function(datasize1, datasize2,
                    error_range, data_range, offsets_range, weights_range,
                    coefficients){
  data_matrix = matrix(0,nrow = datasize1, ncol = datasize2+1)
  for (i in 1:datasize2){
    range_min = data_range[i,1]
    range_max = data_range[i,2]
    data_matrix[,i+1] = runif(datasize1,range_min,range_max)
  }
  # generate variables
  for (i in 1:datasize2){
    data_matrix[,1] = data_matrix[,1]+coefficients[i]*data_matrix[,i+1]
  }
  if (!missing(error_range)){
    error_min = error_range[1]
    error_max = error_range[2]
    error = runif(datasize1,error_min,error_max)
    data_matrix[,1] = data_matrix[,1] + error
  }
  # generate outcome
  data_frame = as.data.frame(data_matrix)
  if (! missing(offsets_range)){
    if (!is.matrix(offsets_range)){
      offsets_min = offsets_range[1]
      offsets_max = offsets_range[2]
      offsets = runif(datasize1,offsets_min,offsets_max)
      data_frame$`(offset)` = offsets
    }
  }
  if (! missing(weights_range)){
    if (!is.matrix(weights_range)){
      weights_min = weights_range[1]
      weights_max = weights_range[2]
      weights = runif(datasize1,weights_min,weights_max)
      data_frame$`(weights)` = weights
    }
  }
  # if weights/offsets is not NULL, generate them
  return (data_frame)
}
