library(tidyverse)


# Load data
train <- read_csv("../data/processed/train.csv")
valid <- read_csv("../data/processed/valid.csv")

# Remove incomplete cases
sum(complete.cases(train)) / nrow(train)  # 0.992
sum(complete.cases(valid)) / nrow(valid)  # 0.993
train <- train %>% na.omit()
valid <- valid %>% na.omit()

num_cols <- sapply(train, typeof) != "character"
scale_center <- sapply(train[num_cols], mean)
scale_sd <- sapply(train[num_cols], sd)
# train[num_cols] <- scale(train[num_cols])


## lm
lm_fit <- lm(SalePrice ~ ., data = train)
lm_fit
lm_pred <- predict(lm_fit, newdata = valid %>% select(-SalePrice))

lm_fit2 <- lm(train$SalePrice ~ train_mat)
lm_fit2

## lm (PCA)

## LASSO


## LASSO (PCA)

## RF

## Boosting



train_mat <- model.matrix(SalePrice ~ ., data = train)[, -1]
dim(train_mat)
train_mat[1:3, 1:4]
