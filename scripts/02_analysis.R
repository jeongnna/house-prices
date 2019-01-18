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
plot(lasso_fit)
lasso_pred <- predict2(lasso_fit, newdata = valid_mat,
                       scale_revert = scale_revert,
                       missing = !cpt, replace_value = y_mean)
pred_plot(valid$SalePrice, lasso_pred, window = c(0, .2))
lasso_rmse <- rmse(valid$SalePrice, lasso_pred) %>% set_names("LASSO")

# Random Forest
set.seed(seed)
rf_fit <- randomForest(SalePrice ~ ., data = train)
rf_fit
plot(rf_fit)
varImpPlot(rf_fit)
rf_pred <- predict2(rf_fit, newdata = valid[cpt, ],
                    scale_revert = scale_revert,
                    missing = !cpt, replace_value = y_mean)
pred_plot(valid$SalePrice, rf_pred, window = c(0, .2))
rf_rmse <- rmse(valid$SalePrice, rf_pred) %>% set_names("RF")

# Gradient Boosting
set.seed(seed)
gbm_fit <- gbm(SalePrice~ ., data = train, distribution = "gaussian",
               n.trees = 1e4, interaction.depth = 4, shrinkage = 1e-3)
gbm_pred <- predict2(gbm_fit, newdata = valid[cpt, ], n_trees = 10000,
                     scale_revert = scale_revert,
                     missing = !cpt, replace_value = y_mean)
pred_plot(valid$SalePrice, gbm_pred, window = c(0, .2))
gbm_rmse <- rmse(valid$SalePrice, gbm_pred) %>% set_names("GBM")


# Model selection ---------------------------------------------------------

rmse_list <- c(lasso_rmse, rf_rmse, gbm_rmse)
signif(rmse_list, 3)
rmse_list[which.min(rmse_list)]
