library(tidyverse)


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
  data$PoolQC <- ifelse(is.na(data$PoolQC), "No_Pool", "Pool")
  data$Fence[is.na(data$Fence)] <- "No"
  
  drop_cols <- 
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
  data <- data %>% select(-drop_cols)
  
  # Attach "X" in front of column's name if it starts with a digit
  start_with_digit <- colnames(data) %>% str_detect("^[:digit:]")
  colnames(data)[start_with_digit] <- 
    str_c("X", colnames(data)[start_with_digit])
  
  data
}


# Specify path of files
in_path <- "../data/raw/"
out_path <- "../data/processed/"

# Training & validation set
train_full <- read_csv(str_c(in_path, "train.csv")) %>% preprocess()
set.seed(123)
valid <- train_full %>% sample_n(0.3 * nrow(train_full), replace = FALSE)
train <- train_full %>% setdiff(valid)
write.csv(train_full, str_c(out_path, "train_full.csv"), row.names = FALSE)
write.csv(train, str_c(out_path, "train.csv"), row.names = FALSE)
write.csv(valid, str_c(out_path, "valid.csv"), row.names = FALSE)

# Test set
test <- read_csv(str_c(in_path, "test.csv")) %>% preprocess()
write.csv(test, str_c(out_path, "test.csv"), row.names = FALSE)
