gbm_fit <- function(x, y, params) {
  require(gbm)
  gbm(
    y ~ ., data = x,
    distribution = params$dist,
    n.trees = params$n_trees,
    interaction.depth = params$depth,
    shrinkage = params$learning_rate
  )
}

model_predict.gbm <- function(object, newdata, params) {
  predict(
    object, newdata,
    n.trees = params$n_trees
  )
}
