#' @importFrom stats lm median predict quantile
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

# # Helper function to extract expression inside after_stat
# extract_after_stat <- function(mapping, aes_name) {
#   if (!is.null(mapping[[aes_name]])) {
#     aes_expr <- as_label(mapping[[aes_name]])
#
#     pattern <- "after_stat\\(([^)]+)\\)"
#     match <- regexec(pattern, aes_expr)
#
#     if (length(match[[1]]) > 1) {
#       extracted <- regmatches(aes_expr, match)[[1]][2]
#       return(extracted)
#     }
#   }
#   return(NULL)
# }

# create circles in geom_vector_smooth
create_circle_data <- function(x, y, radius, n = 100) {
  angle <- seq(0, 2 * pi, length.out = n)
  data.frame(
    x = x + radius * cos(angle),
    y = y + radius * sin(angle),
    group = 1
  )
}

create_wedge_data <- function(
    x, y, xend_upper, yend_upper, xend_lower, yend_lower,
    xend, yend, id, n_points = 100, radius = NULL,
    distance_lower = NULL, distance_upper = NULL, annular_wedge = FALSE
) {
  # Calculate angles using atan2 for upper, lower, and midpoint
  angle_upper <- atan2(yend_upper - y, xend_upper - x) %% (2 * pi)
  angle_lower <- atan2(yend_lower - y, xend_lower - x) %% (2 * pi)
  midpoint_angle <- atan2(yend - y, xend - x) %% (2 * pi)

  # Calculate the shift to bring the midpoint to 0 radians
  shift <- -midpoint_angle

  # Shift all angles by the same amount
  shifted_upper <- (angle_upper + shift) %% (2 * pi)
  shifted_lower <- (angle_lower + shift) %% (2 * pi)

  # Adjust shifted angles to ensure they are relative to the midpoint:
  # Upper should be positive, lower should be negative
  if (shifted_upper > pi) shifted_upper <- shifted_upper - 2 * pi
  if (shifted_lower > pi) shifted_lower <- shifted_lower - 2 * pi

  # Generate arc points from upper (positive) to lower (negative)
  arc_angles <- seq(shifted_upper, shifted_lower, length.out = n_points)

  if (annular_wedge) {
    # Create an annular wedge when annular_wedge is TRUE
    outer_arc_x <- distance_upper * cos(arc_angles)
    outer_arc_y <- distance_upper * sin(arc_angles)
    inner_arc_x <- distance_lower * cos(arc_angles)
    inner_arc_y <- distance_lower * sin(arc_angles)

    # Rotate arc points back to the original orientation
    outer_x_final <- x + outer_arc_x * cos(-shift) - outer_arc_y * sin(-shift)
    outer_y_final <- y + outer_arc_x * sin(-shift) + outer_arc_y * cos(-shift)
    inner_x_final <- x + inner_arc_x * cos(-shift) - inner_arc_y * sin(-shift)
    inner_y_final <- y + inner_arc_x * sin(-shift) + inner_arc_y * cos(-shift)

    # Create the data frame for the annular wedge
    wedge_data <- data.frame(
      x = c(inner_x_final, rev(outer_x_final)),
      y = c(inner_y_final, rev(outer_y_final)),
      group = rep(id, length.out = 2 * n_points),
      id = rep(id, length.out = 2 * n_points)
    )
  } else {
    # Create a solid wedge if annular_wedge is FALSE
    arc_x <- radius * cos(arc_angles)
    arc_y <- radius * sin(arc_angles)

    # Rotate the arc points back to the original coordinate system
    final_x <- x + arc_x * cos(-shift) - arc_y * sin(-shift)
    final_y <- y + arc_x * sin(-shift) + arc_y * cos(-shift)

    # Create a data frame for the solid wedge
    wedge_data <- data.frame(
      x = c(x, final_x, x),
      y = c(y, final_y, y),
      group = rep(id, length.out = n_points + 2),
      id = rep(id, length.out = n_points + 2)
    )
  }

  return(wedge_data)
}


calculate_bounds <- function(fit, se, probs) {
  if (!se) return(NULL)

  # Calculate critical t-value for the confidence level
  t_critical <- qt(1 - (1 - probs) / 2, df = fit$df)

  # Calculate upper and lower bounds
  lower_bound <- fit$fit - t_critical * fit$se.fit
  upper_bound <- fit$fit + t_critical * fit$se.fit

  # Return a list of bounds
  return(list(lower = lower_bound, upper = upper_bound))
}


# Compute Euclidean distances only between nearest neighbors
euclidean_distances <- function(points) {
  n <- nrow(points)
  min_distance <- Inf

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      # Only consider direct neighbors (1-step difference in x or y)
      if (abs(points$x[i] - points$x[j]) <= min(diff(sort(unique(points$x)))) &&
          abs(points$y[i] - points$y[j]) <= min(diff(sort(unique(points$y))))) {
        dist <- sqrt((points$x[i] - points$x[j])^2 + (points$y[i] - points$y[j])^2)
        min_distance <- min(min_distance, dist)
      }
    }
  }
  return(min_distance)
}
