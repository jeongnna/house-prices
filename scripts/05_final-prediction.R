library(tidyverse)
source("src/source_dir.R")
source("src/preprocessing_utils.R")
source("src/rmse.R")
source_dir("src/models")


# Preparation -------------------------------------------------------------

# random seed number
seed <- 123

# load data
train <- read_csv("data/processed/train_full.csv")
test <- read_csv("data/processed/test.csv")

# split x and y
x_train <- train %>% select(-SalePrice)
y_train <- train$SalePrice
x_test <- test

# adjust data shape
cat_cols <- sapply(x_train, typeof) == "character"
cat_cols <- names(cat_cols)[cat_cols]
num_cols <- colnames(x_train) %>% setdiff(c(cat_cols, "SalePrice"))
x_train <- x_train %>% mutate_if(is.character, as.factor)
x_test <- x_test %>% fit_shape(reference = x_train, cols = cat_cols)

# impute missing values
x_train <- na_impute(x_train, num_cols, cat_cols)
x_test <- na_impute(x_test, num_cols, cat_cols)

# normalize features
x_mean <- sapply(x_train[num_cols], mean)
x_sd <- sapply(x_train[num_cols], sd)
x_train[num_cols] <- scale(x_train[num_cols], x_mean, x_sd)
x_test[num_cols] <- scale(x_test[num_cols], x_mean, x_sd)

y_mean <- mean(y_train)
y_sd <- sd(y_train)
y_train <- scale(y_train, y_mean, y_sd)


# Prediction --------------------------------------------------------------

# gradient boosting: score 0.12992 in test set
set.seed(seed)
gbm_params <- list(
  dist = "gaussian",
  n_trees = 6093,
  depth = 4,
  learning_rate = 0.006575879
)
gbm_fitted <- gbm_fit(x_train, y_train, gbm_params)
gbm_pred <- model_predict(gbm_fitted, x_test, gbm_params)
gbm_pred <- gbm_pred * y_sd + y_mean

# submission
id <- 1461:2919
submission <- tibble("Id" = id, "SalePrice" = exp(gbm_pred))
write_csv(submission, "submission.csv")
