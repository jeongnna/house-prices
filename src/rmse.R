rmse <- function(pred, y) {
  sqrt(mean((pred - y)^2))
}
