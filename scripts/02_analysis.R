library(tidyverse)
library(glmnet)
library(randomForest)
library(gbm)
source("src/functions.R")


# Preparation -------------------------------------------------------------

# random seed number
seed <- 123

# load data
train <- read_csv("data/processed/train.csv")
valid <- read_csv("data/processed/valid.csv")

# adjust data shape
cat_cols <- sapply(train, typeof) == "character"
cat_cols <- names(cat_cols)[cat_cols]
num_cols <- colnames(train) %>% setdiff(c(cat_cols, "SalePrice"))
train <- train %>% mutate_if(is.character, as.factor)
valid <- valid %>% fit_shape_tbl(reference = train, cols = cat_cols)

# impute missing values
train <- impute_na(train, num_cols, cat_cols)
valid <- impute_na(valid, num_cols, cat_cols)

# remove incomplete cases
# sum(complete.cases(train)) / nrow(train)  # 99.2%
# train <- train %>% na.omit()
cpt <- complete.cases(valid)

# normalize features
x_mean <- sapply(train[num_cols], mean)
x_sd <- sapply(train[num_cols], sd)
train[num_cols] <- scale(train[num_cols], x_mean, x_sd)
valid[num_cols] <- scale(valid[num_cols], x_mean, x_sd)

y_mean <- mean(train$SalePrice)
y_sd <- sd(train$SalePrice)
train$SalePrice <- scale(train$SalePrice, y_mean, y_sd)
scale_revert <- list("center" = y_mean, "scale" = y_sd)

# PCA
# loading <- train %>% get_pc_loading(num_cols, threshold = .8)
# train <- train %>% apply_pc_loading(num_cols, loading)
# valid <- valid %>% apply_pc_loading(num_cols, loading)

# create dummy variables for LASSO
train_mat <- model.matrix(SalePrice ~ ., data = train)[, -1]
valid_mat <- model.matrix(SalePrice ~ ., data = valid)[, -1]


# Model fitting -----------------------------------------------------------

# LASSO: 0.1723852 in validation set
set.seed(seed)
lasso_fit <- cv.glmnet(train_mat, train$SalePrice)
lasso_pred <- predict2(lasso_fit, newdata = valid_mat,
                       scale_revert = scale_revert,
                       missing = !cpt, replace_value = y_mean)
lasso_rmse <- rmse(valid$SalePrice, lasso_pred) %>% set_names("LASSO")

# random forest: 0.1366698 in validation set
set.seed(seed)
rf_fit <- randomForest(SalePrice ~ ., data = train)
rf_pred <- predict2(rf_fit, newdata = valid[cpt, ],
                    scale_revert = scale_revert,
                    missing = !cpt, replace_value = y_mean)
rf_rmse <- rmse(valid$SalePrice, rf_pred) %>% set_names("RF")

# gradient boosting: 0.1177367 in validation set
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

set.seed(seed)
depth <- 6
lr <- 0.003759716
n_tree <- 6093
# depth <- 6
# lr <- 0.006575879
# n_tree <- 2265
gbm_fit <- gbm(SalePrice ~ ., data = train, distribution = "gaussian",
               n.trees = n_tree,
               interaction.depth = depth,
               shrinkage = lr)
gbm_pred <- predict2(gbm_fit, newdata = valid[cpt, ], n_trees = n_tree,
                     scale_revert = scale_revert,
                     missing = !cpt, replace_value = y_mean)
gbm_rmse <- rmse(valid$SalePrice, gbm_pred) %>% set_names("GBM")


# Model ensemble ----------------------------------------------------------

(rmse_list <- c(lasso_rmse, rf_rmse, gbm_rmse))

ensemble_pred <-
  list(rf_pred, gbm_pred) %>%
  bind_cols() %>%
  apply(1, mean)
(ensemble_rmse <- rmse(valid$SalePrice, ensemble_pred))
