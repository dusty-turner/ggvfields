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
  if (length(n) != 2) stop("Length of 'n' must be 2")
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
