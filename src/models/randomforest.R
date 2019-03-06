randomforest_fit <- function(x, y, params) {
  require(randomForest)
  randomForest(
    y ~ ., data = x,
    ntree = params$n_trees
  )
}

model_predict.randomForest <- function(object, newdata, params) {
  if (is.null(params$type)) {
    if (object$type == "regression") {
      params$type <- "response"
    } else if (object$type == "classification") {
      params$type <- "prob"
    }
  }
  predict(
    object, newdata,
    type = params$type
  )
}
