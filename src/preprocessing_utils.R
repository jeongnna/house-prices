fit_shape_tbl <- function(target, reference, cols) {
  cat_levels <- lapply(reference[cols], levels)
  for (i in seq_along(cat_levels)) {
    col <- cols[i]
    target[[col]] <- factor(target[[col]], levels = levels(reference[[col]]))
  }
  target
}


predict2 <- function(model, newdata, n_trees = NULL, scale_revert = NULL,
                     missing = NULL, replace_value = NULL) {
  if (is.null(missing)) {
    missing <- rep(FALSE, nrow(newdata))
  }
  pred <- numeric()
  pred[!missing] <- predict(model, new = newdata, n.trees = n_trees)
  if (!is.null(scale_revert)) {
    pred[!missing] <- pred[!missing] * scale_revert$scale + scale_revert$center
  }
  pred[missing] <- replace_value
  pred
}


impute_na <- function(data, num_cols, cat_cols) {
  
  impute_na_num <- function(data) {
    data %>% lapply(function(x) {x[is.na(x)] <- median(x, na.rm = TRUE); x})
  }
  
  impute_na_cat <- function(data) {
    most_frequent <- function(x) {
      names(sort(table(x), decreasing = TRUE))[1]
    }
    data %>% lapply(function(x) {x[is.na(x)] <- most_frequent(x); x})
  }
  
  data[num_cols] <- impute_na_num(data[num_cols])
  data[cat_cols] <- impute_na_cat(data[cat_cols])
  
  data
}


get_pc_loading <- function(data, num_cols, threshold = .8) {
  prfit <-
    data %>%
    select(num_cols) %>%
    na.omit() %>%
    prcomp()
  
  pve <- prfit$sdev^2 %>% (function(x) {x / sum(x)})
  n_pc <- which(cumsum(pve) > threshold)[1]
  loading <- prfit$rotation[, 1:n_pc]
  
  loading
}


apply_pc_loading <- function(data, num_cols, loading) {
  x_origin <-
    data %>% 
    select(num_cols) %>%
    as.matrix()
  
  x_pc <- x_origin %*% loading
  
  data %>%
    select(-num_cols) %>%
    bind_cols(x_pc %>% as_tibble())
}
