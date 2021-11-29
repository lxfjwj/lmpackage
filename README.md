# lmpackage

## Badges
<!-- badges: start -->
  [![Travis build status](https://travis-ci.com/lxfjwj/lmpackage.svg?branch=main)](https://travis-ci.com/lxfjwj/lmpackage)
  [![Codecov test coverage](https://codecov.io/gh/lxfjwj/lmpackage/branch/main/graph/badge.svg)](https://app.codecov.io/gh/lxfjwj/lmpackage?branch=main)
<!-- badges: end -->

## Overview
lmpackage is a R package to simulate the functions of lm().

```linear_regression()``` the main funcion the main funciton that returns the result of linear regression.

```model_fit()``` an assistant function for model fitting of a given data set.

```simulate()``` an assistant function for testing data simulation.

You can learn more about how to use them in the following "Usage" part or in the "my-vignette.Rmd" file, which also includes the comparasion with lm() function.

## Usage
```{r}
library(lmpackage)
datasize1 = 5
datasize2 = 3
data_range = matrix(c(0,0,0,1,1,1),3,2)
offsets_range = c(0,1)
weights_range = c(0,1)
coefficients = c(1,2,3)
data = simulate(datasize1, datasize2, 
                error_range = c(1,2), data_range, offsets_range, weights_range,
                coefficients)
# V1         V2          V3        V4   (offset)  (weights)
# 1  3.433210 0.67290520 0.085142442 0.3721877 0.02740092 0.87368450
# 2  2.332600 0.19175256 0.071466435 0.2849875 0.49231630 0.38044609
# 3  5.439885 0.58043133 0.559331076 0.6247749 0.78856636 0.38346669
# 4  4.056214 0.02185524 0.380990321 0.7567662 0.37115253 0.84489410
# 5  3.818819 0.44317418 0.004957652 0.7013312 0.61570799 0.37972039

model = linear_regression(V1~V2+V3+V4, data = data, weights = data$`(weights)`,offset = data$`(offsets)`)
```

