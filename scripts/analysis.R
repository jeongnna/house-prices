library(tidyverse)
library(glmnet)
library(randomForest)
library(gbm)


# Functions ---------------------------------------------------------------

rmse <- function(y, yhat) {
  sqrt(mean((y - yhat)^2))
}

pred_plot <- function(y, yhat, window = NULL) {
  if (is.null(window)) {
    window <- c(0, 1)
  }
  window <- floor(window * length(y))
  window[1] <- max(window[1], 0)
  bind_cols(y = y, yhat = yhat) %>% 
    mutate(id = 1:n()) %>% 
    slice(window[1]:window[2]) %>%
    # gather(key = type, value = val, 1:2) %>% 
    # ggplot(aes(x = id, y = val, group = type, col = type)) +
    ggplot() +
    geom_point(aes(x = id, y = y), col = "black") +
    geom_line(aes(x = id, y = y), col = "black") +
    geom_point(aes(x = id, y = yhat), col = "red") +
    geom_line(aes(x = id, y = yhat), col = "red") +
    # geom_point() +
    # geom_line() +
    # scale_color_manual(name = NULL,
    #                    labels = c("True", "Prediction"),
    #                    values = c("blue", "red")) +
    labs(x = "Obs", y = "SalePrice") +
    theme_bw()
}


# Preparation -------------------------------------------------------------

# Load data
data <- read_csv("../data/processed/train.csv")

# Remove incomplete cases
sum(complete.cases(data)) / nrow(data)  # 0.992
data <- data %>% na.omit()

# One-hot encoding
data_mat <- model.matrix(SalePrice ~ ., data = data)
data_mat <- data_mat[, -1]  # Exclude intercept term
colnames(data_mat) <- str_replace(colnames(data_mat), " ", "_")

# Partition data into training & validation set
set.seed(123)
n <- nrow(data_mat)
valid_ind <- sample(1:n, 0.3 * n, replace = FALSE)
valid_x <- data_mat[valid_ind, ]
train_x <- data_mat[-valid_ind, ]
valid_y <- data$SalePrice[valid_ind]
train_y <- data$SalePrice[-valid_ind]

excluded_cols <- which(apply(train_x, 2, n_distinct) == 1)
train_x <- train_x[, -excluded_cols]
valid_x <- valid_x[, -excluded_cols]

# Scale features
# num_cols <- sapply(data, typeof) != "character"
# num_cols <- names(num_cols)[num_cols] %>% setdiff("SalePrice")
x_min <- apply(train_x, 2, min)
x_range <- apply(train_x, 2, function(x) {max(x) - min(x)})
train_x <- scale(train_x, center = x_min, scale = x_range)
valid_x <- scale(valid_x, center = x_min, scale = x_range)
y_min <- min(train_y)
y_range <- max(train_y) - min(train_y)
train_y <- scale(train_y, center = y_min, scale = y_range)
# valid_y <- scale(valid_y, center = y_min, scale = y_range)


# Fit models --------------------------------------------------------------

## lm
# lm_fit <- lm(train_y ~ train_x)
# lm_fit
# lm_pred <- predict(lm_fit, newdata = as_tibble(valid_x))

## LASSO
lasso_fit <- cv.glmnet(train_x, train_y)
plot(lasso_fit)
lasso_pred <- predict(lasso_fit, s = "lambda.min", newx = valid_x)
lasso_pred <- lasso_pred * y_range + y_min
pred_plot(valid_y, lasso_pred, window = c(0, .2))
lasso_rmse <- rmse(valid_y, lasso_pred) %>% set_names("LASSO")

## Random Forest
# rf_fit <- randomForest(train_y ~ ., data = as_tibble(train_x))
rf_fit <- randomForest(train_x, train_y)
rf_fit
plot(rf_fit)
varImpPlot(rf_fit)
rf_pred <- predict(rf_fit, newdata = valid_x)
rf_pred <- rf_pred * y_range + y_min
pred_plot(valid_y, rf_pred, window = c(0, .2))
rf_rmse <- rmse(valid_y, rf_pred) %>% set_names("RF")

## Boosting
# gbm_fit <- gbm(train_y ~ train_x, data = as_tibble(train_x),
#                n.trees = 100, shrinkage = 0.1)


# Model selection ---------------------------------------------------------

rmse_list <- c(err_lasso, err_rf)
signif(rmse_list, 3)
rmse_list[which.min(rmse_list)]
