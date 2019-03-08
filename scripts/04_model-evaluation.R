library(tidyverse)
source("src/source_dir.R")
source("src/preprocessing_utils.R")
source("src/rmse.R")
source_dir("src/models")


# Preparation -------------------------------------------------------------

# random seed number
seed <- 123

# load data
train <- read_csv("data/processed/train.csv")
valid <- read_csv("data/processed/valid.csv")

# split x and y
x_train <- train %>% select(-SalePrice)
y_train <- train$SalePrice
x_val <- valid %>% select(-SalePrice)
y_val <- valid$SalePrice

# adjust data shape
cat_cols <- sapply(x_train, typeof) == "character"
cat_cols <- names(cat_cols)[cat_cols]
num_cols <- colnames(x_train) %>% setdiff(c(cat_cols, "SalePrice"))
x_train <- x_train %>% mutate_if(is.character, as.factor)
x_val <- x_val %>% fit_shape(reference = x_train, cols = cat_cols)

# impute missing values
x_train <- na_impute(x_train, num_cols, cat_cols)
x_val <- na_impute(x_val, num_cols, cat_cols)

# normalize features
x_mean <- sapply(x_train[num_cols], mean)
x_sd <- sapply(x_train[num_cols], sd)
x_train[num_cols] <- scale(x_train[num_cols], x_mean, x_sd)
x_val[num_cols] <- scale(x_val[num_cols], x_mean, x_sd)

y_mean <- mean(y_train)
y_sd <- sd(y_train)
y_train <- scale(y_train, y_mean, y_sd)


# Model fitting -----------------------------------------------------------

# LASSO: score 0.1612084 in validation set
set.seed(seed)
lasso_params <- list(type_measure = "mse")
lasso_fitted <- cv_lasso_fit(x_train, y_train, lasso_params)
lasso_params$lambda <- lasso_fitted$lambda.min
lasso_pred <- model_predict(lasso_fitted, x_val, lasso_params)
lasso_pred <- lasso_pred * y_sd + y_mean
lasso_rmse <- rmse(lasso_pred, y_val) %>% set_names("LASSO")

# Random Forest: score 0.1367729 in validation set
set.seed(seed)
rf_params <- list(n_trees = 500)
rf_fitted <- randomforest_fit(x_train, y_train, rf_params)
rf_pred <- model_predict(rf_fitted, x_val, rf_params)
rf_pred <- rf_pred * y_sd + y_mean
rf_rmse <- rmse(rf_pred, y_val) %>% set_names("RF")

# Gradient Boosting: score 0.1235108 in validation set
# hyperparameter tuning
set.seed(seed)
depths <- c(4, 6, 8) %>% sort()
learning_rates <- 10^runif(5, min = -3, max = -1) %>% sort()
n_trees <- 10^runif(10, min = 2, max = 4) %>% sort()
n_tree_max <- max(n_trees)

rmses <- NULL
best_rmse <- 1e+5
best_gbm <- NULL

for (depth in depths) {
  for (lr in learning_rates) {
    set.seed(seed)
    gbm_params <- list(
      dist = "gaussian",
      n_trees = n_tree_max,
      depth = depth,
      learning_rate = lr
    )
    gbm_fitted <- gbm_fit(x_train, y_train, gbm_params)
    for (n_tree in n_trees) {
      gbm_params$n_trees <- n_tree
      gbm_pred <- model_predict(gbm_fitted, x_val, gbm_params)
      gbm_pred <- gbm_pred * y_sd + y_mean
      gbm_rmse <- rmse(gbm_pred, y_val)
      rmses <- c(rmses, gbm_rmse)
      cat("depth: ", depth, ", lr: ", lr, ", n_tree: ", n_tree,
          " ---> rmse: ", gbm_rmse, "\n", sep = "")
      if (gbm_rmse < best_rmse) {
        best_rmse <- gbm_rmse
        best_gbm <- gbm_fitted
      }
    }
  }
}
# apply the best parameter
set.seed(seed)
gbm_params <- list(
  dist = "gaussian",
  n_trees = 6093,
  depth = 4,
  learning_rate = 0.006575879
)
gbm_fitted <- gbm_fit(x_train, y_train, gbm_params)
gbm_pred <- model_predict(gbm_fitted, x_val, gbm_params)
gbm_pred <- gbm_pred * y_sd + y_mean
gbm_rmse <- rmse(gbm_pred, y_val) %>% set_names("GBM")


# Model ensemble ----------------------------------------------------------

(rmse_list <- c(lasso_rmse, rf_rmse, gbm_rmse))

ensemble_pred <-
  list(rf_pred, gbm_pred) %>%
  bind_cols() %>%
  apply(1, mean)
(ensemble_rmse <- rmse(valid$SalePrice, ensemble_pred))
