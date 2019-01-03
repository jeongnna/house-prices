library(tidyverse)


train <- read_csv("../data/train.csv")

excluded <- 
  c(
    "Id", 
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
    "GarageQual",  # Imbalanced and meaningless
    "GarageCond",  # Imbalanced and meaningless
    "BsmtHalfBath",  # Meaningless
    "PoolArea",  # Meaningless
    "MiscFeature",  # Extremely imbalanced,
    "MiscVal",  # Imbalanced and meaningless
    "MoSold",  # Meaningless
    "YrSole",  # Meaningless
  )

train$MSSubClass <- as.character(train$MSSubClass)
train$Alley[is.na(train$Alley)] <- "NA"
train$BsmtFullBath[train$BsmtFullBath > 1] <- 1
train$PoolQC <- ifelse(is.na(train$PoolQC), 1, 0)
train$Fence[is.na(train$Fence)] <- "NA"
