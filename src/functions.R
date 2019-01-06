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

predict2 <- function(model, newdata, scale_revert = NULL,
                     missing = NULL, replace_value = NULL) {
  if (is.null(missing)) {
    missing <- rep(FALSE, nrow(newdata))
  }
  pred <- numeric()
  pred[!missing] <- predict(model, new = newdata)
  if (!is.null(scale_revert)) {
    pred[!missing] <- pred[!missing] * scale_revert$scale + scale_revert$center
  }
  pred[missing] <- replace_value
  pred
}
