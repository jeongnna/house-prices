rmse <- function(y, yhat) {
  sqrt(mean((y - yhat)^2))
}

fit_shape_tbl <- function(target, reference, cols) {
  cat_levels <- lapply(reference[cols], levels)
  for (i in seq_along(cat_levels)) {
    col <- cols[i]
    target[[col]] <- factor(target[[col]], levels = levels(reference[[col]]))
  }
  target
}

fit_shape_mat <- function(target, reference) {
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

# pred_plot <- function(y, yhat, window = NULL) {
#   if (is.null(window)) {
#     window <- c(0, 1)
#   }
#   window <- floor(window * length(y))
#   window[1] <- max(window[1], 0)
#   bind_cols(y = y, yhat = yhat) %>%
#     mutate(id = 1:n()) %>%
#     slice(window[1]:window[2]) %>%
#     ggplot() +
#     geom_point(aes(x = id, y = y), col = "black") +
#     geom_line(aes(x = id, y = y), col = "black") +
#     geom_point(aes(x = id, y = yhat), col = "red") +
#     geom_line(aes(x = id, y = yhat), col = "red") +
#     labs(x = "Obs", y = "SalePrice") +
#     theme_bw()
# }
