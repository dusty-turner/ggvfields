# List of non-expored and non-documented functions

ensure_nonempty_data <- function(data) {
  if (empty(data)) {
    tibble0(group = 1, .size = 1)
  }
  else {
    data
  }
}

ensure_length_two <- function(n) {
  if (length(n) == 1) n <- rep(n, 2)
  if (length(n) != 2) stop("Length of 'n' must be 2")
  return(n)
}

# ensure_nonempty_data <- function(data) {
#   if (length(data) == 0) {
#     data.frame(x = 1)
#   } else {
#     data
#   }
# }

# Function to normalize angles to the range [-pi, pi]
normalize_angle <- function(angle) {
  angle <- angle %% (2 * pi)
  angle[angle > pi] <- angle[angle > pi] - 2 * pi
  return(angle)
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

# Custom draw key function for length aesthetic
draw_key_length <- function(data, params, size) {
  rel_length <- data$length %||% 1
  grid::linesGrob(
    x = c(0.5 - 0.4 * rel_length, 0.5 + 0.4 * rel_length), y = c(0.5, 0.5),  # Adjust the length of the line
    gp = grid::gpar(
      col = alpha(data$colour %||% "black", data$alpha %||% NA),
      lwd = 1  # Constant line width for the legend key
    )
  )
}

# Utility function to replace %||%
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Helper function to extract expression inside after_stat
extract_after_stat <- function(mapping, aes_name) {
  if (!is.null(mapping[[aes_name]])) {
    aes_expr <- as_label(mapping[[aes_name]])
    # aes_expr <- as.character(mapping[[aes_name]])

    pattern <- "after_stat\\(([^)]+)\\)"
    match <- regexec(pattern, aes_expr)

    if (length(match[[1]]) > 1) {
      extracted <- regmatches(aes_expr, match)[[1]][2]
      return(extracted)
    }
  }
  return(NULL)
}

