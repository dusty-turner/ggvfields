#' @importFrom stats lm median predict quantile qnorm
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
  if (length(n) != 2) cli::cli_abort("Length of 'n' must be 2")
  return(n)
}

# the gam smoothers always fit fx and fy with s(x, y), so a formula has no effect
warn_if_formula_ignored <- function(method, formula_supplied) {
  if (identical(method, "gam") && formula_supplied) {
    cli::cli_warn(c(
      "{.arg formula} is ignored when {.code method = \"gam\"}.",
      "i" = "Use {.code method = \"lm\"} to fit {.arg formula}."
    ))
  }
  invisible()
}


# basis dimension k for the gam smoothers' s(x, y). mgcv's default of 30 needs
# at least 30 distinct locations; with fewer the fit fails. for small data
# sets use half the number of locations, which keeps enough residual degrees of
# freedom for the standard errors without interpolating the noise
gam_basis_size <- function(data) {
  n <- nrow(unique(data[, c("x", "y")]))
  if (n < 5) {
    cli::cli_abort(c(
      "{.code method = \"gam\"} needs at least 5 distinct locations, not {n}.",
      "i" = "Use {.code method = \"lm\"} for smaller data sets."
    ))
  }
  if (n >= 30) 30 else max(4, n %/% 2)
}


tibble0 <- function(...) {
  tibble::tibble(..., .name_repair = "minimal")
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || inherits(df, "waiver")
}

# Utility function to replace %||%
`%||%` <- function(a, b) if (!is.null(a)) a else b

norm <- function(u) sqrt(sum(u^2))

normalize <- function(u) u / norm(u)


utils::globalVariables(c("potential", "fun", "f", "l", "max_t", "avg_spd", "x", "y", "fx", "fy", "normalize", "z"))

# typical spacing of a set of points (columns x and y): the median distance
# from a point to its nearest neighbour. large sets use a sample of points, each
# compared with all the others
grid_spacing <- function(grid) {
  xy <- unique(as.matrix(grid[, c("x", "y"), drop = FALSE]))
  n <- nrow(xy)
  if (n < 2) return(NULL)
  idx <- if (n > 2000) unique(round(seq(1, n, length.out = 2000))) else seq_len(n)
  nn <- vapply(idx, function(i) {
    d <- sqrt((xy[, 1] - xy[i, 1])^2 + (xy[, 2] - xy[i, 2])^2)
    min(d[-i])
  }, numeric(1))
  s <- stats::median(nn)
  if (is.finite(s) && s > 0) s else NULL
}
