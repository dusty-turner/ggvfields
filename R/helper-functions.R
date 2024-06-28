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
