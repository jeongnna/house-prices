library(tidyverse)
library(glmnet)
library(randomForest)
library(gbm)
source("../src/functions.R")


set.seed(seed)

# Load data
train_full <- read_csv("../data/processed/train_full.csv")

# Hyperparameters for Gradient Boosting
depths <- 4:8
learning_rates <- c(.05, .10, .15)
n_trees <- c(1000, 5000, 10000)

# Construct indices for cross-validation
n_folds <- 5
n_train <- nrow(train_full)
full_idx <- 1:n_train
idx_list <- list()
for (k in 1:n_folds) {
  temp_idx <- sample(full_idx, n_train / n_folds, replace = FALSE)
  idx_list[[k]] <- temp_idx
  full_idx <- setdiff(full_idx, temp_idx)
}


# Cross-validation --------------------------------------------------------

pred_list <- list()

for (k in 1:n_folds) {
  idx <- idx_list[[k]]
  valid = train_full[idx, ]
  train = train_full[-idx, ]
  
  cat_cols <- sapply(train, typeof) == "character"
  cat_cols <- names(cat_cols)[cat_cols]
  num_cols <- colnames(train) %>% setdiff(c(cat_cols, "SalePrice"))
  train <- train %>% mutate_if(is.character, as.factor)
  valid <- valid %>% fit_shape_tbl(reference = train, cols = cat_cols)
  
  # Remove incomplete cases
  train <- train %>% na.omit()
  cpt <- complete.cases(valid)
  
  # Create dummy variables for LASSO
  train_mat <- model.matrix(SalePrice ~ ., data = train)[, -1]
  valid_mat <- model.matrix(SalePrice ~ ., data = valid)[, -1]
  
  # Normalize features
  x_min <- sapply(train[num_cols], min)
  x_range <- sapply(train[num_cols], function(x) diff(range(x)))  # max - min
  train[num_cols] <- scale(train[num_cols], x_min, x_range)
  valid[num_cols] <- scale(valid[num_cols], x_min, x_range)
  train_mat[, num_cols] <- scale(train_mat[, num_cols], x_min, x_range)
  valid_mat[, num_cols] <- scale(valid_mat[, num_cols], x_min, x_range)
  
  y_mean <- mean(train$SalePrice)
  y_min <- min(train$SalePrice)
  y_range <- diff(range(train$SalePrice))  # max - min
  train$SalePrice <- scale(train$SalePrice, y_min, y_range)
  scale_revert <- list("center" = y_min, "scale" = y_range)
  
  # Model: LASSO
  lasso_fit <- cv.glmnet(train_mat, train$SalePrice)
  lasso_pred <- predict2(lasso_fit, newdata = valid_mat,
                         scale_revert = scale_revert,
                         missing = !cpt, replace_value = y_mean)
  pred_list[["lasso"]] <- c(pred_list[["lasso"]], lasso_pred)
  
  # Model: Random Forest
  rf_fit <- randomForest(SalePrice ~ ., data = train)
  rf_pred <- predict2(rf_fit, newdata = valid[cpt, ],
                      scale_revert = scale_revert,
                      missing = !cpt, replace_value = y_mean)
  pred_list[["rf"]] <- c(pred_list[["rf"]], rf_pred)
  
  # Model: Gradient boosting
  for (depth in depths) {
    for (lr in learning_rates) {
      gbm_fit <- gbm(SalePrice~ ., distribution = "gaussian", data = train,
                     n.trees = 1e4, interaction.depth = depth, shrinkage = lr)
      for (n_tree in n_trees) {
        gbm_pred <- predict2(gbm_fit, newdata = valid[cpt, ],
                             n_trees = n_tree, scale_revert = scale_revert,
                             missing = !cpt, replace_value = y_mean)
        name <- str_c("gbm_depth", depth, "_lr", lr, "_tree", n_tree)
        pred_list[[name]] <- c(pred_list[[name]], gbm_pred)
      }
    }
  }
}


# Compute RMSE ------------------------------------------------------------

rmse_list <- NULL
y <- train_full$SalePrice[unlist(idx_list)]
for (i in seq_along(pred_list)) {
  rmse_list[i] <- rmse(y, pred_list[[i]])
}
rmse_list <- rmse_list %>% set_names(names(pred_list))
rmse_list
