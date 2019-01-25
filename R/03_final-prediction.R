library(tidyverse)
library(gbm)
source("./functions.R")


# Preparation -------------------------------------------------------------

# Random seed number
seed <- 123

# Load data
train <- read_csv("../data/processed/train_full.csv")
test <- read_csv("../data/processed/test.csv")

# Adjust data shape
cat_cols <- sapply(train, typeof) == "character"
cat_cols <- names(cat_cols)[cat_cols]
num_cols <- colnames(train) %>% setdiff(c(cat_cols, "SalePrice"))
train <- train %>% mutate_if(is.character, as.factor)
test <- test %>% fit_shape_tbl(reference = train, cols = cat_cols)

# Impute missing values
train <- impute_na(train, num_cols, cat_cols)
test <- impute_na(test, num_cols, cat_cols)

# Normalize features
x_mean <- sapply(train[num_cols], mean)
x_sd <- sapply(train[num_cols], sd)
train[num_cols] <- scale(train[num_cols], x_mean, x_sd)
test[num_cols] <- scale(test[num_cols], x_mean, x_sd)

y_mean <- mean(train$SalePrice)
y_sd <- sd(train$SalePrice)
train$SalePrice <- scale(train$SalePrice, y_mean, y_sd)
scale_revert <- list("center" = y_mean, "scale" = y_sd)


# Prediction --------------------------------------------------------------

# Gradient Boosting: score 0.12736 in test set
set.seed(seed)
depth <- 6
lr <- 0.003759716
n_tree <- 6093
gbm_fit <- gbm(SalePrice ~ ., data = train, distribution = "gaussian",
               n.trees = n_tree,
               interaction.depth = depth,
               shrinkage = lr)
gbm_pred <- predict2(gbm_fit, newdata = test, n_trees = n_tree,
                     scale_revert = scale_revert)

# Submission
id <- 1461:2919
submission <- tibble("Id" = id, "SalePrice" = exp(gbm_pred))
write.csv(submission, "../submission.csv", row.names = FALSE)
