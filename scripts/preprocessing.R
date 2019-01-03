library(tidyverse)
library(data.table)




train <- read_csv("../data/train.csv")
train <- 
  train %>% 
  select(-Id, -Street) %>% 
  mutate(MSSubClass = as.character(MSSubClass))


# 제외할 변수
exclude_ <- 
  c(
    "Id", 
    "Street",  # Extremely imbalanced
    "Utilities",  # Extremely imbalanced
    "Condition1",  # Condition 1 & 2 ...
    "Condition2",  # Seemingly meaningless
    "LotConfig",  # Imbalanced and meaningless
    "LandSlope",  # Imbalanced and meaningless
  )
