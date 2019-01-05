library(tidyverse)


fit_shape <- function(target, reference) {
  if (!is.matrix(target)) {
    stop("Error: `target` must be a matrix")
  }
  if (!is.matrix(reference)) {
    stop("Error: `reference` must be a matrix")
  }
  tar_names <- colnames(target)
  ref_names <- colnames(reference)
  in_ref <- tar_names %in% ref_names  # Names both in target & in ref
  out_ref <- setdiff(ref_names, tar_names)  # Names in target but not in ref
  if (length(out_ref) > 0) {
    temp_mat <- matrix(0, nrow = nrow(target), ncol = length(out_ref))
    colnames(temp_mat) <- out_ref
    target <- cbind(target, temp_mat)
  }
  target[, ref_names]
}


preprocess <- function(data) {
  if ("SalePrice" %in% colnames(data)) {
    data$SalePrice <- log(data$SalePrice)
  }
  data$MSSubClass <- str_c("X", data$MSSubClass)
  data$Alley[is.na(data$Alley)] <- "No"
  data$YearRemodAdd <- data$YearRemodAdd - data$YearBuilt
  bsmt_cat <- str_c("Bsmt",
                    c("Qual", "Cond", "Exposure", "FinType1", "FinType2"))
  no_bsmt <- is.na(data$BsmtQual)
  data[no_bsmt, bsmt_cat] <- "No"
  garage_cat <- str_c("Garage",
                      c("Type", "Finish", "Qual", "Cond"))
  no_garage <- is.na(data$GarageQual)
  data[no_garage, garage_cat] <- "No"
  data$BsmtFullBath[data$BsmtFullBath > 1] <- 1
  data$FireplaceQu[is.na(data$FireplaceQu)] <- "No"
  data$PoolQC <- ifelse(is.na(data$PoolQC), 1, 0)
  data$Fence[is.na(data$Fence)] <- "No"
  
  excluded <- 
    c(
      "Id",
      "LotFrontage",  # Meaningless and hard to handle missing data
      "Street",  # Extremely imbalanced
      "Utilities",  # Extremely imbalanced
      "Condition1",  # Condition 1 & 2 ...
      "Condition2",  # Seemingly meaningless
      "LotConfig",  # Imbalanced and meaningless
      "LandSlope",  # Imbalanced and meaningless
      "RoofMatl",  # Extremely imbalanced
      "ExterCond",  # Extremely imbalanced
      "Heating",  # Extremely imbalanced
      "Functional",  # Imbalanced and meaningless
      "GarageYrBlt",  # Meaningless and hard to handle missing data
      "GarageQual",  # Imbalanced and meaningless
      "GarageCond",  # Imbalanced and meaningless
      "BsmtHalfBath",  # Meaningless
      "PoolArea",  # Meaningless
      "MiscFeature",  # Extremely imbalanced,
      "MiscVal",  # Imbalanced and meaningless
      "MoSold",  # Meaningless
      "YrSold"  # Meaningless
    )
  data <- data %>% select(-excluded)
  
  start_with_digit <- colnames(data) %>% str_detect("^[:digit:]")
  colnames(data)[start_with_digit] <- 
    str_c("X", colnames(data)[start_with_digit])
  
  data
}


in_path <- "../data/raw/"
out_path <- "../data/processed/"

# for (name in c("train.csv", "test.csv")) {
#   read_csv(file = str_c(in_path, name)) %>% 
#     preprocess() %>% 
#     write.csv(file = str_c(out_path, name), row.names = FALSE)
# }

# Training & validation set
data <- read_csv(str_c(in_path, "train.csv"))
train_full <- data %>% preprocess()
set.seed(123)
valid <- train_full %>% sample_n(0.3 * nrow(data), replace = FALSE)
train <- setdiff(data, valid)
write.csv(train_full, str_c(out_path, "train_full.csv"), row.names = FALSE)
write.csv(train, str_c(out_path, "train.csv"), row.names = FALSE)
write.csv(valid, str_c(out_path, "valid.csv"), row.names = FALSE)

# Test set
test <- read_csv(str_c(in_path, "test.csv"))
test <- test %>% preprocess()
write.csv(test, file = str_c(out_path, "test.csv"), row.names = FALSE)

# Create dummy variables for LASSO
train_mat <- model.matrix(SalePrice ~ ., data = train)
train_mat <- train_mat[, -1]  # Exclude intercept term
valid_mat <- model.matrix(SalePrice ~ ., data = train)
valid_mat <- valid_mat[, -1]
test_mat <- model.matrix(SalePrice ~ ., data = train)
test_mat <- test_mat[, -1]
# colnames(train_mat) <- str_replace(colnames(train_mat), " ", "_")
# colnames(valid_mat) <- str_replace(colnames(valid_mat), " ", "_")
# colnames(test_mat) <- str_replace(colnames(test_mat), " ", "_")
valid_mat <- valid_mat %>% fit_shape(reference = train_mat)
