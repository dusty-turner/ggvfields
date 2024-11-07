# Define the bootstrapping function
perform_bootstrapping <- function(data, grid, probs, se) {
  # Perform bootstrapping to predict angles
  boot_results <- boot::boot(data, function(d, i) {
    fit_sin <- lm(sin(atan2(d$dy, d$dx)) ~ x + y, data = d[i, ])
    fit_cos <- lm(cos(atan2(d$dy, d$dx)) ~ x + y, data = d[i, ])

    pred_sin <- predict(fit_sin, newdata = grid)
    pred_cos <- predict(fit_cos, newdata = grid)

    return(atan2(pred_sin, pred_cos))
  }, R = 500)

  # Extract predictions and use the median as the main angle prediction
  angle_preds <- boot_results$t
  grid$angle_pred <- apply(angle_preds, 2, median)

  # If SE enabled, calculate outer and inner bounds for prediction intervals
  if (se) {
    grid$angle_lower_outer <- apply(angle_preds, 2, function(x) quantile(x, probs = (1 - probs[1]) / 2))
    grid$angle_upper_outer <- apply(angle_preds, 2, function(x) quantile(x, probs = 1 - (1 - probs[1]) / 2))

    if (!is.na(probs[2])) {
      grid$angle_lower_inner <- apply(angle_preds, 2, function(x) quantile(x, probs = (1 - probs[2]) / 2))
      grid$angle_upper_inner <- apply(angle_preds, 2, function(x) quantile(x, probs = 1 - (1 - probs[2]) / 2))
    } else {
      # Fill inner bounds with NA if probs[2] is not provided
      grid$angle_lower_inner <- rep(NA, nrow(grid))
      grid$angle_upper_inner <- rep(NA, nrow(grid))
    }
  }

  # Predict distances
  fit_distance <- lm(sqrt(dx^2 + dy^2) ~ x + y, data = data)
  pred_distance <- predict(fit_distance, newdata = grid, se.fit = se)
  grid$distance_pred <- pred_distance$fit

  # Calculate xend and yend using the predicted angles and distances
  grid$fit_dx <- grid$x + grid$distance_pred * cos(grid$angle_pred)
  grid$fit_dy <- grid$y + grid$distance_pred * sin(grid$angle_pred)

  if (se) {
    # Calculate outer bounds for xend and yend
    grid$xend_upper_outer <- grid$x + grid$distance_pred * cos(grid$angle_upper_outer)
    grid$yend_upper_outer <- grid$y + grid$distance_pred * sin(grid$angle_upper_outer)
    grid$xend_lower_outer <- grid$x + grid$distance_pred * cos(grid$angle_lower_outer)
    grid$yend_lower_outer <- grid$y + grid$distance_pred * sin(grid$angle_lower_outer)

    # Calculate inner bounds or fill with NA for xend and yend
    if (!is.na(probs[2])) {
      grid[, c("xend_upper_inner", "yend_upper_inner",
               "xend_lower_inner", "yend_lower_inner")] <- list(
                 grid$x + grid$distance_pred * cos(grid$angle_upper_inner),
                 grid$y + grid$distance_pred * sin(grid$angle_upper_inner),
                 grid$x + grid$distance_pred * cos(grid$angle_lower_inner),
                 grid$y + grid$distance_pred * sin(grid$angle_lower_inner)
               )
    } else {
      grid[, c("xend_upper_inner", "yend_upper_inner",
               "xend_lower_inner", "yend_lower_inner")] <- NA
    }
  }

  return(grid)
}
