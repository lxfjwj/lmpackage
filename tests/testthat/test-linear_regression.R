data = list()
y = c(2,4,6,8)
x = c(1.1,1.9,3.1,4.2)
off = c(1,1,1,1)
data$x = x
data$y = y
data$off = off
testresult = lm(y~x,data=data, offset = off)
test_that("", {
  expect_equal(linear_regression(y~x,data, offset = off)$fitted.values, testresult$fitted.values)
  expect_equal(linear_regression(y~x,data, offset = off)$coefficients, unname(testresult$coefficients))
  expect_equal(linear_regression(y~x,data, offset = off)$rank, testresult$rank)
  expect_equal(linear_regression(y~x,data, offset = off)$residuals, testresult$residuals)
  expect_equal(linear_regression(y~x,data, offset = off)$effects, unname(testresult$effects))
  expect_equal(linear_regression(y~x,data, offset = off)$df.residual, testresult$df.residual)
})
