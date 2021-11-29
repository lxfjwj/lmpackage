datasize1 = 10
datasize2 = 3
data_range = matrix(c(0,0,0,1,1,1),3,2)
offsets_range = c(0,1)
weights_range = c(0,1)
coefficients = c(1,2,3)
data = simulate(datasize1, datasize2,
                error_range = c(0,1), data_range, offsets_range, weights_range,
                coefficients)
testresult = lm(V1~V2+V3+V4, data = data, weights = data$`(weights)`, offset = data$`(offset)`)
test_that("", {
  expect_equal(linear_regression(V1~V2+V3+V4, data = data, weights = data$`(weights)`, model = TRUE, x = TRUE, y = TRUE, qr=TRUE, offset = data$`(offset)`)$fitted.values, testresult$fitted.values)
  expect_equal(linear_regression(V1~V2+V3+V4, data = data, weights = data$`(weights)`, model = TRUE, x = TRUE, y = TRUE, qr=TRUE, offset = data$`(offset)`)$coefficients, unname(testresult$coefficients))
  expect_equal(linear_regression(V1~V2+V3+V4, data = data, weights = data$`(weights)`, model = TRUE, x = TRUE, y = TRUE, qr=TRUE, offset = data$`(offset)`)$rank, testresult$rank)
  expect_equal(linear_regression(V1~V2+V3+V4, data = data, weights = data$`(weights)`, model = TRUE, x = TRUE, y = TRUE, qr=TRUE, offset = data$`(offset)`)$residuals, testresult$residuals)
  expect_equal(linear_regression(V1~V2+V3+V4, data = data, weights = data$`(weights)`, model = TRUE, x = TRUE, y = TRUE, qr=TRUE, offset = data$`(offset)`)$effects, unname(testresult$effects))
  expect_equal(linear_regression(V1~V2+V3+V4, data = data, weights = data$`(weights)`, model = TRUE, x = TRUE, y = TRUE, qr=TRUE, offset = data$`(offset)`)$df.residual, testresult$df.residual)
})
