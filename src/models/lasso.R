lasso_fit <- function(x, y, params) {
  require(glmnet)
  x_mat <- model.matrix(y ~ ., data = x)
  glmnet(
    x_mat, y,
    family = params$dist,
    alpha = 1,
    lambda = params$lambda
  )
}

model_predict.glmnet <- function(object, newdata, params) {
  predict(
    object, newdata,
    type = "response"
  )
}

cv_lasso_fit <- function(x, y, params) {
  require(glmnet)
  x_mat <- model.matrix(y ~ ., data = x)
  cv.glmnet(
    x_mat, y,
    type.measure = params$type_measure,
    alpha = 1
  )
}

model_predict.cv.glmnet <- function(object, newdata, params) {
  newdata_mat <- model.matrix(~ ., data = newdata)
  predict(
    object, newdata_mat,
    s = params$lambda,
    type = "response"
  )
}
