library(tidyverse)
library(glmnet)
library(randomForest)
library(gbm)
source("../src/functions.R")


# Functions ---------------------------------------------------------------

pred_plot <- function(y, yhat, window = NULL) {
  if (is.null(window)) {
    window <- c(0, 1)
  }
  window <- floor(window * length(y))
  window[1] <- max(window[1], 0)
  bind_cols(y = y, yhat = yhat) %>% 
    mutate(id = 1:n()) %>% 
    slice(window[1]:window[2]) %>%
    ggplot() +
    geom_point(aes(x = id, y = y), col = "black") +
    geom_line(aes(x = id, y = y), col = "black") +
    geom_point(aes(x = id, y = yhat), col = "red") +
    geom_line(aes(x = id, y = yhat), col = "red") +
    labs(x = "Obs", y = "SalePrice") +
    theme_bw()
}

rmse <- function(y, yhat) {
  sqrt(mean((y - yhat)^2))
}


# Preparation -------------------------------------------------------------

# Random seed number
seed <- 123

# Load data
train <- read_csv("../data/processed/train.csv")
valid <- read_csv("../data/processed/valid.csv")

cat_cols <- sapply(train, typeof) == "character"
cat_cols <- names(cat_cols)[cat_cols]
num_cols <- colnames(train) %>% setdiff(c(cat_cols, "SalePrice"))
train <- train %>% mutate_if(is.character, as.factor)
valid <- valid %>% fit_shape_tbl(reference = train, cols = cat_cols)

# Remove incomplete cases
sum(complete.cases(train)) / nrow(train)  # 99.2%
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


# Model fitting -----------------------------------------------------------

# LASSO
set.seed(seed)
lasso_fit <- cv.glmnet(train_mat, train$SalePrice)
lasso_pred <- predict2(lasso_fit, newdata = valid_mat,
                       scale_revert = scale_revert,
                       missing = !cpt, replace_value = y_mean)
lasso_rmse <- rmse(valid$SalePrice, lasso_pred) %>% set_names("LASSO")

# Random Forest
set.seed(seed)
rf_fit <- randomForest(SalePrice ~ ., data = train)
rf_pred <- predict2(rf_fit, newdata = valid[cpt, ],
                    scale_revert = scale_revert,
                    missing = !cpt, replace_value = y_mean)
rf_rmse <- rmse(valid$SalePrice, rf_pred) %>% set_names("RF")

# Gradient Boosting
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
    gbm_fit <- gbm(SalePrice ~ ., data = train, distribution = "gaussian",
                   n.trees = n_tree_max, 
                   interaction.depth = depth, 
                   shrinkage = lr)
    for (n_tree in n_trees) {
      gbm_pred <- predict2(gbm_fit, newdata = valid[cpt, ], n_trees = n_tree,
                           scale_revert = scale_revert,
                           missing = !cpt, replace_value = y_mean)
      gbm_rmse <- rmse(valid$SalePrice, gbm_pred) %>% set_names("GBM")
      rmses <- c(rmses, gbm_rmse)
      
      cat("depth: ", depth, ", lr: ", lr, ", n_tree: ", n_tree,
          " ---> rmse: ", gbm_rmse, "\n", sep = "")
      
      if (gbm_rmse < best_rmse) {
        best_rmse <- gbm_rmse
        best_gbm <- gbm_fit
      }
    }
  }
}

depths <- c(4, 6, 6, 6)
learning_rates <- c(0.006575879, 0.003759716, 0.006575879, 0.05834919)
n_trees <- c(8197, 8197, 6093, 818)
gbm_pred_list <- list()
gbm_rmse_list <- list()

for (i in 1:4) {
  depth <- depths[i]
  lr <- learning_rates[i]
  n_tree <- n_trees[i]
  gbm_fit <- gbm(SalePrice ~ ., data = train, distribution = "gaussian",
                 n.trees = n_tree_max, 
                 interaction.depth = depth, 
                 shrinkage = lr)
  gbm_pred <- predict2(gbm_fit, newdata = valid[cpt, ], n_trees = n_tree,
                       scale_revert = scale_revert,
                       missing = !cpt, replace_value = y_mean)
  gbm_rmse <- rmse(valid$SalePrice, gbm_pred) %>% set_names("GBM")
  
  gbm_pred_list <- gbm_pred_list %>% append(list(gbm_pred))
  gbm_rmse_list <- gbm_rmse_list %>% append(list(gbm_rmse))
}


# Model ensemble ----------------------------------------------------------

(rmse_list <- c(lasso_rmse, rf_rmse, unlist(gbm_rmse_list)))

ensemble_pred <- 
  gbm_pred_list %>% 
  append(list(lasso_pred, rf_pred)) %>% 
  .[c(1:4, 6)] %>% 
  bind_cols() %>% 
  apply(1, mean)

rmse(valid$SalePrice, ensemble_pred)
