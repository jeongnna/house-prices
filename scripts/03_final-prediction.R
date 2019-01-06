library(tidyverse)
library(randomForest)
source("../src/functions.R")


# Preparation -------------------------------------------------------------

# Random seed number
seed <- 123

# Load data
train <- read_csv("../data/processed/train_full.csv")
test <- read_csv("../data/processed/test.csv")

cat_cols <- sapply(train, typeof) == "character"
cat_cols <- names(cat_cols)[cat_cols]
num_cols <- colnames(train) %>% setdiff(c(cat_cols, "SalePrice"))
train <- train %>% mutate_if(is.character, as.factor)
test <- test %>% fit_shape_tbl(reference = train, cols = cat_cols)

# Remove incomplete cases
sum(complete.cases(train)) / nrow(train)  # 99.2%
train <- train %>% na.omit()
cpt <- complete.cases(test)

# Normalize features
x_min <- sapply(train[num_cols], min)
x_range <- sapply(train[num_cols], function(x) diff(range(x)))  # max - min
train[num_cols] <- scale(train[num_cols], x_min, x_range)
test[num_cols] <- scale(test[num_cols], x_min, x_range)

y_mean <- mean(train$SalePrice)
y_med <- median(train$SalePrice)
y_min <- min(train$SalePrice)
y_range <- diff(range(train$SalePrice))  # max - min
train$SalePrice <- scale(train$SalePrice, y_min, y_range)
scale_revert <- list("center" = y_min, "scale" = y_range)


# Prediction --------------------------------------------------------------

# Random Forest
set.seed(seed)
rf_fit <- randomForest(SalePrice ~ ., data = train)
rf_fit
plot(rf_fit)
varImpPlot(rf_fit)
rf_pred <- predict2(rf_fit, newdata = test[cpt, ],
                    scale_revert = scale_revert,
                    missing = !cpt, replace_value = y_mean)

pred_tbl <- tibble(Id = 1461:2919, SalePrice = exp(rf_pred))
write.csv(pred_tbl, "../submission.csv", row.names = FALSE)
