# List of non-expored and non-documented functions

ensure_nonempty_data <- function(data) {
  if (length(data) == 0) {
    data.frame(x = 1)
  } else {
    data
  }
}



times <- `*`


rad2deg <- function(rad, rotate = 0) {
  ((rad + rotate) * 360/(2*pi)) %% 360
}


tibble0 <- function(...) {
  tibble::tibble(..., .name_repair = "minimal")
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || inherits(df, "waiver")
}

ensure_nonempty_data <- function(data) {
  if (empty(data)) {
    tibble0(group = 1, .size = 1)
  }
  else {
    data
  }
}

vectorize <- function(f, drop = TRUE) {
  function(v) {
    stopifnot(is.numeric(v))
    if (is.vector(v)) v <- matrix(v, nrow = 1)
    out <- list(nrow(v))
    for (i in 1:nrow(v)) out[[i]] <- f(v[i,])
    out <- t(simplify2array(out))
    if ((nrow(out) == 1L) && drop) out[1,] else out
  }
}

## helper functions for hession / laplacian
extract_component_function <- function(fun, index) {
  function(v) fun(v)[index]
}

## helper functions for hession / laplacian
compute_laplacian <- function(func, v) {
  hessian_matrix <- numDeriv::hessian(func, v)
  sum(diag(hessian_matrix))
}
