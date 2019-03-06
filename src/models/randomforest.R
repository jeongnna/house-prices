randomforest_fit <- function(x, y, params) {
  require(randomForest)
  randomForest(
    y ~ ., data = x,
    ntree = params$n_trees
  )
}

model_predict.randomForest <- function(object, newdata, params) {
  predict(
    object, newdata,
    type = "prob"
  )
}
