fit_shape <- function(target, reference, cols) {
  cat_levels <- lapply(reference[cols], levels)
  for (i in seq_along(cat_levels)) {
    col <- cols[i]
    target[[col]] <- factor(target[[col]], levels = levels(reference[[col]]))
  }
  target
}


na_impute <- function(data, num_cols, cat_cols) {
  na_impute_numcols <- function(data) {
    data %>% lapply(function(x) {x[is.na(x)] <- median(x, na.rm = TRUE); x})
  }
  na_impute_catcols <- function(data) {
    mode <- function(x) {
      first(names(sort(table(x), decreasing = TRUE)))
    }
    data %>% lapply(function(x) {x[is.na(x)] <- mode(x); x})
  }
  data[num_cols] <- na_impute_numcols(data[num_cols])
  data[cat_cols] <- na_impute_catcols(data[cat_cols])
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
