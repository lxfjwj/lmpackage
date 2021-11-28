testdata = as.data.frame(matrix(c(2,4,6,8,1.1,1.9,3.1,4.2,1,1,1,1),4,3))
testresult = lm(testdata[,1]~testdata[,2],offset = c(1,1,1,1))
test_that("", {
  expect_equal(model_fit(testdata,intercept=1,offset = c(1,1,1,1))$fitted.values, unname(testresult$fitted.values))
  expect_equal(model_fit(testdata,intercept=1,offset = c(1,1,1,1))$coefficients, unname(testresult$coefficients))
  expect_equal(model_fit(testdata,intercept=1,offset = c(1,1,1,1))$rank, testresult$rank)
  expect_equal(model_fit(testdata,intercept=1,offset = c(1,1,1,1))$residuals, unname(testresult$residuals))
})
