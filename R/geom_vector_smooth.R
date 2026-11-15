#' Create a Smooth Vector Plot Layer
#'
#' `geom_vector_smooth()` creates a ggplot2 layer that visualizes a smooth
#' vector field. It takes raw vector data and applies smoothing (by default a
#' generalized additive model; a multivariate linear model or cokriging are
#' also available via `method`) to estimate the underlying vector field. This
#' functionality is analogous to `geom_smooth()` in ggplot2 but is tailored for
#' vector data rather than scalar responses.
#'
#' @importFrom stats qt integrate vcov model.matrix coef resid cov
#' @importFrom mgcv gam s
#'
#' @param mapping A set of aesthetic mappings created by \code{ggplot2::aes()}.
#'   **Required:** Must include **`x`** and **`y`**; vector displacements are defined by
#'   **`fx`** and **`fy`**.
#' @param data A data frame containing the raw vector data.
#' @param stat The statistical transformation. `geom_vector_smooth()` always
#'   uses `StatVectorSmooth`, which fits the model; another value is ignored
#'   with a warning.
#' @param position Position adjustment, either as a string or the result of a
#'   position adjustment function.
#' @param n An integer vector specifying the number of grid points along each
#'   axis for smoothing.
#' @param xlim,ylim Numeric vectors of length 2 giving the range of the grid
#'   on which the smoothed field is evaluated when `eval_points` is `NULL`.
#'   They default to the range of the data.
#' @param method Character. Specifies the smoothing method. Supported options
#'   are `"gam"` (default), `"lm"`, and `"kriging"`. The `"gam"` method fits
#'   `fx` and `fy` together, each with the smooth `s(x, y)`, in mgcv's
#'   multivariate normal family ([mgcv::mvn()]), which also estimates the
#'   correlation of their residuals; the `"lm"` method fits the multivariate
#'   linear model given by `formula`, and `"kriging"` uses a cokriging approach
#'   via the `gstat` package. Each smooth uses up to 30 basis functions, and
#'   at most half as many as there are distinct locations; at least 5
#'   locations are needed. With fewer than 10 locations, or data without
#'   noise, `fx` and `fy` are fitted separately and their residuals are taken
#'   to be uncorrelated.
#' @param se Logical. If `TRUE`, prediction intervals for the tip of each
#'   vector are computed and plotted. For every `method` they cover a new
#'   observation at that location, not only the fitted mean: their covariance
#'   is the covariance of the fitted vector plus the residual covariance, so
#'   ellipses tilt when `fx` and `fy` are correlated.
#' @param se.circle Logical. Defaults to `FALSE`. If `TRUE`, circles are drawn
#'   around the origin of each vector. By default, the circle’s radius is
#'   computed as the magnitude of the predicted vector.
#' @param conf_level Numeric. Specifies the confidence level for the prediction
#'   intervals. Default is `0.95`. If a vector is given, its first element is used.
#' @param eval_points A data frame of evaluation points. If provided, these
#'   specify the grid where the smoothing model is evaluated. If `NULL`, a grid
#'   is generated based on `n`.
#' @param pi_type Character. Determines the display style for prediction
#'   intervals. By default (`NULL`), ellipses are drawn at `eval_points` and
#'   wedges on a grid.
#'   - `"ellipse"`: Ellipses are used to represent the covariance of the predictions.
#'   - `"wedge"`: Angular wedges show the prediction interval for the direction
#'   of each vector: the central `conf_level` range of the angle of its tip,
#'   from the exact distribution of that angle when the tip is normally
#'   distributed with the predicted mean and covariance.
#'   Ellipses need `eval_points`: if `pi_type = "ellipse"` is set and
#'   `eval_points` is `NULL`, wedges are drawn instead, with a warning.
#' @param arrow A \code{grid::arrow()} specification for arrowheads on the
#'   smoothed vectors.
#' @param formula A formula specifying the multivariate linear model used for
#'   smoothing when `method = "lm"`. The default is `cbind(fx, fy) ~ x * y`. It
#'   is ignored by the other methods, with a warning if it is supplied when
#'   `method = "gam"`.
#' @param ... Other arguments passed to \code{ggplot2::layer()} and the
#'   underlying geometry/stat.
#'
#'
#' @section Aesthetics: `geom_vector_smooth()` supports the following aesthetics
#'   (required aesthetics are in **bold**):
#'
#'   - **`x`**: The x-coordinate of the vector's starting point.
#'   - **`y`**: The y-coordinate of the vector's starting point.
#'   - **`fx`**: The horizontal component of the vector displacement.
#'   - **`fy`**: The vertical component of the vector displacement.
#'   - `color`: The color of the vector lines.
#'   - `linewidth`: The thickness of the vector lines.
#'   - `linetype`: The type of the vector lines (e.g., solid, dashed).
#'   - `alpha`: The transparency level of the vectors.
#'   - `arrow`: An aesthetic that can be used to modify arrowhead properties.
#'
#' @section Details:
#' **Multivariate Linear Model:**
#'   The `"lm"` method fits a multivariate linear model to predict vector
#'   displacements (`fx` and `fy`) based on the coordinates `x` and `y`,
#'   including interaction terms (`x * y`). This model smooths the raw vector
#'   data to provide an estimate of the underlying vector field.
#'
#' **Prediction Intervals:**
#'   When `se = TRUE`, prediction intervals are computed for the smoothed
#'   vectors. Two types of intervals are supported:
#'   - **Ellipse:** Ellipses represent the joint uncertainty (covariance) in the predicted `fx` and `fy`.
#'   - **Wedge:** Wedges (angular sectors) show the range of likely directions
#'   of each vector; their radius is the predicted vector's length.
#'   The type of interval displayed is controlled by `pi_type`, and the
#'   confidence level is set via `conf_level`.
#'
#' @return A ggplot2 layer that can be added to a plot to create a smooth vector
#'   field visualization.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' # Function to generate vectors
#' generate_vectors <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(
#'     sin(x) + sin(y) + rnorm(1, 5, 1),
#'     sin(x) - sin(y) - rnorm(1, 5, 1)
#'   )
#' }
#'
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Create sample points and compute vectors
#' sample_points <- data.frame(
#'   x = runif(30, 0, 10),
#'   y = runif(30, 0, 10)
#' )
#'
#' result <- t(apply(sample_points, 1, generate_vectors))
#'
#' sample_points$xend <- result[, 1]
#' sample_points$yend <- result[, 2]
#' sample_points$fx <- sample_points$xend - sample_points$x
#' sample_points$fy <- sample_points$yend - sample_points$y
#' sample_points$distance <- sqrt(sample_points$fx^2 + sample_points$fy^2)
#' sample_points$angle <- atan2(sample_points$fy, sample_points$fx)
#'
#' # Define evaluation points
#' eval_points <- data.frame(
#'   x = c(0, 7.5),
#'   y = c(10, 5)
#' )
#'
#' # Example 1:
#' ggplot(sample_points, aes(x = x, y = y)) +
#'   geom_vector(aes(fx = fx, fy = fy, color = NULL), center = FALSE, alpha = 0.2) +
#'   geom_vector_smooth(aes(fx = fx, fy = fy), n = 5) +
#'   ggtitle("Smoothed Vector Field")
#'
#' # Example 2: Ellipse with eval_points
#' ggplot(sample_points, aes(x = x, y = y)) +
#'   geom_vector(aes(fx = fx, fy = fy, color = NULL), center = FALSE, alpha = 0.2) +
#'   geom_vector_smooth(aes(fx = fx, fy = fy), eval_points = eval_points, conf_level = 0.9) +
#'   ggtitle("Smoothed Vector Field with Ellipse Intervals")
#'
#' # Example 3: Wedge with eval_points
#' ggplot(sample_points, aes(x = x, y = y)) +
#'   geom_vector(aes(fx = fx, fy = fy, color = NULL), center = FALSE, alpha = 0.2) +
#'   geom_vector_smooth(aes(fx = fx, fy = fy), eval_points = eval_points, pi_type = "wedge") +
#'   ggtitle("Smoothed Vector Field with Wedge Intervals")
#'
#' @aliases geom_vector_smooth stat_vector_smooth StatVectorSmooth
#' @name geom_vector_smooth
#' @export
NULL

#' @rdname geom_vector_smooth
#' @export
geom_vector_smooth <- function(mapping = NULL, data = NULL,
   stat = "vector_smooth",
   position = "identity",
   ...,
   na.rm = FALSE,
   show.legend = NA,
   inherit.aes = TRUE,
   n = c(11, 11),
   xlim = NULL,
   ylim = NULL,
   method = "gam",
   se = TRUE,
   se.circle = FALSE,
   pi_type = NULL,
   conf_level = .95,
   formula = cbind(fx, fy) ~ x * y,
   eval_points = NULL,
   arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
   ) {

  call_args <- as.list(environment())

  # the layer always uses StatVectorSmooth, which fits the model
  warn_if_stat_ignored(stat, StatVectorSmooth, "vector_smooth", "geom_vector_smooth")

  construct_layer(
    vector_smooth_layer, call_args, list(...),
    stat = StatVectorSmooth,
    geom = GeomVectorSmooth,
    formula_supplied = !missing(formula)
  )
}

#' @rdname geom_vector_smooth
#' @export
stat_vector_smooth <- function(mapping = NULL, data = NULL,
   geom = "vector_smooth",
   position = "identity",
   ...,
   na.rm = FALSE,
   show.legend = NA,
   inherit.aes = TRUE,
   n = c(11, 11),
   xlim = NULL,
   ylim = NULL,
   method = "gam",
   se = TRUE,
   se.circle = FALSE,
   conf_level = .95,
   pi_type = NULL,
   formula = cbind(fx, fy) ~ x * y,
   eval_points = NULL,
   arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
   ) {

  call_args <- as.list(environment())

  construct_layer(
    vector_smooth_layer, call_args, list(...),
    stat = StatVectorSmooth,
    formula_supplied = !missing(formula)
  )
}

#' @rdname geom_vector_smooth
#' @format NULL
#' @usage NULL
#' @keywords internal
StatVectorSmooth <- ggproto(
  "StatVectorSmooth",
  Stat,
  required_aes = c("x", "y"),
  dropped_aes = c("distance", "norm", "angle"),
  default_aes = aes(
    linewidth = 0.5, linetype = 1, alpha = 1,
    angle = NA, distance = NA, norm = NA, fx = NA, fy = NA, length = NA
  ),

  compute_group = function(data, scales, n, method, se = TRUE, conf_level,
                           pi_type, eval_points = NULL, formula,
                           xlim = NULL, ylim = NULL, ...) {
    # ----------------------------
    # 1. Initial Data Checks and Manipulation
    # ----------------------------
    if (pi_type == "ellipse" && is.null(eval_points)) {
      cli::cli_warn(c(
        "!" = "{.field eval_points} is {.code NULL}; changing {.field pi_type} from {.val ellipse} to {.val wedge}."
      ))

      pi_type <- "wedge"
    }
    # Use helper function to validate input
    data <- norm_as_distance(data)
    validation_result <- validate_aesthetics(data)

    # If 'angle' and 'distance' are provided, compute 'fx' and 'fy'
    if (!all(is.na(data$angle)) && !all(is.na(data$distance))) {
      data$fx <- data$distance * cos(data$angle)
      data$fy <- data$distance * sin(data$angle)
    } else if (all(!is.na(data$fx)) && all(!is.na(data$fy))) {
      # If 'fx' and 'fy' are provided, compute 'angle' and 'distance'
      data$distance <- sqrt(data$fx^2 + data$fy^2)
      data$angle <- atan2(data$fy, data$fx)
    }

    # Ensure 'n' is a numeric vector of length 2
    n <- ensure_length_two(n)

    # Create grid for evaluation
    if (!is.null(eval_points)) {
      # Validate eval_points contains necessary columns
      if (!all(c("x", "y") %in% names(eval_points))) {
        cli::cli_abort("The 'eval_points' argument must contain 'x' and 'y' columns.")
      }

      grid <- eval_points

      # Compute base_radius based on data range
      data_range <- min(diff(range(data$x)), diff(range(data$y)))
      base_radius <- data_range / 2.5
    } else {
      # Generate grid when eval_points is not provided
      xlim <- xlim %||% range(data$x, na.rm = TRUE)
      ylim <- ylim %||% range(data$y, na.rm = TRUE)
      x_seq <- seq(xlim[1], xlim[2], length.out = n[1])
      y_seq <- seq(ylim[1], ylim[2], length.out = n[2])
      grid <- expand.grid(x = x_seq, y = y_seq)

      # Calculate grid spacing and base radius
      x_spacing <- diff(sort(unique(grid$x)))[1]
      y_spacing <- diff(sort(unique(grid$y)))[1]
      base_radius <- min(x_spacing, y_spacing) / 2.5
    }
    grid$id <- seq_len(nrow(grid))

    # Calculate xend and yend using fx and fy
    data$xend <- data$x + data$fx
    data$yend <- data$y + data$fy

    # Calculate angle and distance
    data$distance <- sqrt(data$fx^2 + data$fy^2)
    data$angle <- atan2(data$fy, data$fx)

    # ----------------------------
    # 2. Model Fitting and Prediction
    # ----------------------------
    if(method == "lm"){
    # Fit the multivariate linear model
    model_mv <- lm(formula, data = data)

    # Predict fx and fy for the grid
    predictions_mv <- predict(model_mv, newdata = grid)

    # Extract predicted means (ensure correct column names)
    grid$fx <- predictions_mv[, "fx"]  # Predicted fx
    grid$fy <- predictions_mv[, "fy"]  # Predicted fy

    # ----------------------------
    # 3. Covariance Matrix Computation
    # ----------------------------

    # Extract the covariance matrix of the model coefficients
    cov_coef <- vcov(model_mv)
    # Create the design matrix for the grid
    design_matrix <- model.matrix(formula, data = grid)

    # Number of response variables (fx and fy)
    n_resp <- length(all.vars(formula[[2]]))

    # Number of coefficients per response
    coeffs_per_response <- length(coef(model_mv)[,1])

    # Check the number of coefficients
    total_coeffs <- ncol(cov_coef)

    # Extract residuals from the model
    residuals_mv <- resid(model_mv)

    # Compute covariance matrix of residuals
    Sigma <- cov(residuals_mv)

    # Extract correlation coefficient
    sigma_x <- sqrt(Sigma["fx", "fx"])
    sigma_y <- sqrt(Sigma["fy", "fy"])
    rho <- Sigma["fx", "fy"] / (sigma_x * sigma_y)

    expected_coeffs <- coeffs_per_response * n_resp
    if (total_coeffs != expected_coeffs) {
      cli::cli_abort("Unexpected number of coefficients in the model. Please verify the formula and data.")
    }

    # Extract covariance matrices
    cov_beta_fx <- cov_coef[1:coeffs_per_response, 1:coeffs_per_response]     # Covariance for fx coefficients
    cov_beta_fy <- cov_coef[(coeffs_per_response+1):(expected_coeffs), (coeffs_per_response+1):(expected_coeffs)]     # Covariance for fy coefficients
    cov_beta_fx_fy <- cov_coef[1:coeffs_per_response, (coeffs_per_response+1):(expected_coeffs)]  # Covariance between fx and fy coefficients

    # Compute variance for fx and fy predictions
    var_fx <- rowSums(design_matrix * (design_matrix %*% cov_beta_fx))
    var_fy <- rowSums(design_matrix * (design_matrix %*% cov_beta_fy))
    cov_fx_fy <- rowSums(design_matrix * (design_matrix %*% cov_beta_fx_fy))

    # Add residual variances and covariance
    var_fx <- var_fx + Sigma["fx", "fx"]
    var_fy <- var_fy + Sigma["fy", "fy"]
    cov_fx_fy <- cov_fx_fy + Sigma["fx", "fy"]

    # Assemble the covariance matrix for (fx, fy) predictions
    cov_pred <- data.frame(var_fx, var_fy, cov_fx_fy)
    }

    if(method == "kriging"){

      rlang::check_installed(c("sp", "gstat"), reason = "to use `method = \"kriging\"`.")

      # kriging runs in standardized coordinates (shifted to the lower corner
      # of the data and divided by its diagonal), so the result does not
      # depend on the units of x and y; predictions are reported at the
      # original evaluation points
      origin <- c(min(data$x, na.rm = TRUE), min(data$y, na.rm = TRUE))
      unit <- sqrt(diff(range(data$x, na.rm = TRUE))^2 + diff(range(data$y, na.rm = TRUE))^2)
      if (!is.finite(unit) || unit <= 0) unit <- 1
      standardize <- function(d) {
        d <- as.data.frame(d)
        d$x <- (d$x - origin[1]) / unit
        d$y <- (d$y - origin[2]) / unit
        d
      }

      # Convert data (a tibble) to a regular data frame and then to a SpatialPointsDataFrame
      df <- standardize(data)
      sp::coordinates(df) <- ~ x + y

      # Build the cokriging model for fx and fy
      g <- gstat::gstat(id = "fx", formula = fx ~ 1, data = df)
      g <- gstat::gstat(g, id = "fy", formula = fy ~ 1, data = df)

      # Compute the experimental variograms (adjust cutoff as needed)
      v <- gstat::variogram(g)

      # Fit a linear model of coregionalization (LMC) using a Gaussian model.
      # fit.lmc() fits the sills for a given range, so the range is estimated
      # from the data first: a Gaussian variogram is fitted to each component
      # and the LMC uses the mean of the two ranges
      range0 <- kriging_range(df, v)
      model <- gstat::vgm(psill = 1, model = "Gau", range = range0, nugget = 0.1)
      # correct.diagonal keeps the fitted sill matrices positive definite, so
      # the model is a valid covariance and prediction does not fail
      lmc <- gstat::fit.lmc(v, g, model = model, correct.diagonal = 1.01)

      # Define prediction points: if eval_points is NULL, use grid; otherwise, use eval_points
      new_point <- if (is.null(eval_points)) grid else eval_points
      new_xy <- data.frame(x = new_point$x, y = new_point$y)

      # Convert the standardized points to a SpatialPointsDataFrame
      new_std <- standardize(new_xy)
      sp::coordinates(new_std) <- ~ x + y

      # Perform prediction using the fitted LMC model
      invisible(capture.output(pred <- predict(lmc, newdata = new_std)))

      # Extract prediction results into a simple data frame
      grid <- data.frame(x = new_xy$x, y = new_xy$y,
                         fx = pred$fx.pred, fy = pred$fy.pred)
      cov_pred <- data.frame(var_fx = pred$fx.var,
                             var_fy = pred$fy.var,
                             cov_fx_fy = pred$cov.fx.fy)

      # Compute overall (lag-0) covariance matrix from the LMC model
      sill_fx    <- sum(lmc$model$fx$psill)
      sill_fy    <- sum(lmc$model$fy$psill)
      sill_fx_fy <- sum(lmc$model$`fx.fy`$psill)

      Sigma <- matrix(c(sill_fx, sill_fx_fy,
                        sill_fx_fy, sill_fy), nrow = 2)

      # Calculate the overall correlation (rho) from the covariance matrix
      rho <- sill_fx_fy / sqrt(sill_fx * sill_fy)
    }

    if (method == "gam") {

      fit <- fit_joint_gam(data)

      # Define prediction points: if eval_points is NULL, use grid; otherwise, use eval_points
      new_point <- if (is.null(eval_points)) grid else eval_points
      pred <- predict_joint_gam(fit, new_point)

      grid <- data.frame(x = new_point$x, y = new_point$y, fx = pred$fx, fy = pred$fy)
      cov_pred <- pred[, c("var_fx", "var_fy", "cov_fx_fy")]
      rho <- NULL

    }

    # ----------------------------
    # 4. Compute Prediction Intervals
    # ----------------------------


    if (pi_type == "ellipse" || pi_type == "wedge") {
      # Compute prediction intervals based on pi_type
      if (pi_type == "ellipse") {
        ellipse_params_list <- mapply(
          compute_ellipse_params,
          var_fx = cov_pred$var_fx,
          var_fy = cov_pred$var_fy,
          cov_fx_fy = cov_pred$cov_fx_fy,
          MoreArgs = list(conf_level = conf_level[1]),
          SIMPLIFY = FALSE
        )
        # Extract ellipse parameters and add to grid
        grid$ellipse_width <- sapply(ellipse_params_list, `[[`, "width")
        grid$ellipse_height <- sapply(ellipse_params_list, `[[`, "height")
        grid$ellipse_angle <- sapply(ellipse_params_list, `[[`, "angle")
      }

      if (pi_type == "wedge") {

        wedge_angles <- do.call(rbind, mapply(
          function(x, y, mux, muy, var_fx, var_fy, cov_fx_fy, rho, conf_level) {

            Sigma_local <- matrix(c(var_fx, cov_fx_fy, cov_fx_fy, var_fy), nrow = 2)
            predict_theta_interval(x = x, y = y, mux = mux, muy = muy,
                                   Sigma = Sigma_local, rho = rho, conf_level = conf_level)
          },
          x = grid$x,
          y = grid$y,
          mux = grid$fx,
          muy = grid$fy,
          var_fx = cov_pred$var_fx,
          var_fy = cov_pred$var_fy,
          cov_fx_fy = cov_pred$cov_fx_fy,
          MoreArgs = list(rho = rho, conf_level = conf_level[1]),
          SIMPLIFY = FALSE
        ))

        grid <- cbind(grid, wedge_angles)
        grid$r_upper <- sqrt(grid$fx^2 + grid$fy^2)
        grid$r_lower <- 0

        # Adjust scale if eval_points is NULL
        if (is.null(eval_points)) {
          current_magnitudes <- sqrt(grid$fx^2 + grid$fy^2)
          current_magnitudes[current_magnitudes == 0] <- 1  # Avoid division by zero
          scaling_factors <- base_radius / current_magnitudes
          grid$fx <- grid$fx * scaling_factors
          grid$fy <- grid$fy * scaling_factors
          grid$xend <- grid$x + grid$fx
          grid$yend <- grid$y + grid$fy
          grid$r_upper <- base_radius
        }

        # Compute prediction endpoints
        prediction_results <- mapply(
          compute_prediction_endpoints,
          x = grid$x,
          y = grid$y,
          fx = grid$fx,
          fy = grid$fy,
          angle_lower = grid$min_angle,
          angle_upper = grid$max_angle,
          SIMPLIFY = FALSE
        )
        prediction_df <- do.call(rbind, prediction_results)
        grid <- cbind(grid, prediction_df)
      }

      # Finalize result
      result <- grid
      result$xend <- result$x + result$fx
      result$yend <- result$y + result$fy
    } else {
      cli::cli_abort("Invalid value for pi_type. Must be 'wedge' or 'ellipse'.")
    }
    return(result)
  }
)


#' @rdname geom_vector_smooth
#' @format NULL
#' @usage NULL
#' @keywords internal
GeomVectorSmooth <- ggproto(
  "GeomVectorSmooth",
  GeomSegment,
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = aes(
    colour = "#3366FF",
    linewidth = 0.5,
    linetype = 1, alpha = 1,
    fill = NULL
  ),

  setup_data = function(data, params) {
    data$id <- seq_len(nrow(data))
    return(data)
  },

  draw_panel = function(data, panel_params, coord, arrow = NULL, se = TRUE,
                        se.circle = FALSE, eval_points, pi_type) {


    grobs <- list()

    if (pi_type == "ellipse" && is.null(eval_points)) {
      pi_type <- "wedge"
    }

    if (se) {
      if (pi_type == "wedge") {

        # Initialize a list to store all wedge polygons
        wedge_polygons <- vector("list", nrow(data))

        for (i in 1:nrow(data)) {

          wedge_polygons[[i]] <- create_wedge_data(
            x = data$x[i], y = data$y[i],
            xend_upper = data$xend_upper[i], yend_upper = data$yend_upper[i],
            xend_lower = data$xend_lower[i], yend_lower = data$yend_lower[i],
            xend = data$xend[i], yend = data$yend[i],
            id = data$id[i],
            n_points = 50,
            outer_radius = data$r_lower[i],
            inner_radius = data$r_upper[i]
          )
        }

        # Combine all wedge data into a single data frame
        wedge_data <- do.call(rbind, wedge_polygons)

        # Assign aesthetics for the wedge
        wedge_data$linewidth <- 0.5
        wedge_data$alpha <- .4
        wedge_data$fill <- "grey40"
        wedge_data$colour <- NA

        # Draw the wedges using GeomPolygon
        wedge_grob <- GeomPolygon$draw_panel(
          wedge_data,
          panel_params = panel_params,
          coord = coord
        )



        grobs <- c(grid::gList(wedge_grob), grobs)

      } else if (pi_type == "ellipse") {
        # Draw ellipses
        ellipse_data_list <- lapply(1:nrow(data), function(i) {
          ellipse <- create_ellipse_data(
            x_center = data$xend[i],
            y_center = data$yend[i],
            width = data$ellipse_width[i],
            height = data$ellipse_height[i],
            angle = data$ellipse_angle[i],
            n_points = 100
          )
          ellipse$group <- data$id[i]
          ellipse
        })

        # Combine ellipse data
        ellipse_data <- do.call(rbind, ellipse_data_list)

        # Assign aesthetics
        ellipse_data$linewidth <- 0.5
        ellipse_data$alpha <- 0.4
        ellipse_data$fill <- "grey60"
        ellipse_data$colour <- NA

        # Draw the ellipses using GeomPolygon
        ellipse_grob <- GeomPolygon$draw_panel(
          ellipse_data,
          panel_params = panel_params,
          coord = coord
        )

        grobs <- c(grid::gList(ellipse_grob), grobs)
      } else {
        cli::cli_abort("Invalid value for pi_type. Must be 'wedge' or 'ellipse'.")
      }
    }
    if (se.circle) {
      # Initialize a list to store all circle polygons
      circle_polygons <- vector("list", nrow(data))

      for (i in 1:nrow(data)) {
        circle_polygons[[i]] <- create_circle_data(
          x = data$x[i], y = data$y[i],
          radius = sqrt(data$fx[i]^2 + data$fy[i]^2),
          n = 100,
          group = data$id[i]
        )
      }

      # Combine all circle data into a single data frame
      circle_data <- do.call(rbind, circle_polygons)

      # Assign aesthetics for the circles
      circle_data$linewidth <- 0.5
      circle_data$alpha <- 0.2
      circle_data$fill <- NA
      circle_data$colour <- "grey60"

      # Draw the circles using GeomPolygon
      circle_grob <- GeomPolygon$draw_panel(
        circle_data,
        panel_params = panel_params,
        coord = coord
      )

      grobs <- c(grobs, grid::gList(circle_grob))
    }

    # Draw the main vectors using GeomSegment
    segments_grob <- GeomSegment$draw_panel(
      data, panel_params, coord, arrow = arrow
    )
    grobs <- c(grobs, grid::gList(segments_grob))

    # Combine all grobs into a single grobTree
    combined_grob <- grid::grobTree(children = do.call(grid::gList, grobs))

    return(combined_grob)
  },

  draw_key = draw_key_smooth
)

#' @keywords internal
validate_aesthetics <- function(data) {
  # Helper function to check if all specified columns exist and are non-NA
  columns_valid <- function(df, cols) {
    all(cols %in% names(df)) && all(!is.na(df[[cols[1]]])) && all(!is.na(df[[cols[2]]]))
  }

  # Check if both 'angle' and 'distance' are provided and fully populated
  has_angle_distance <- columns_valid(data, c("angle", "distance"))

  # Check if both 'dx' and 'dy' are provided and fully populated
  has_fx_fy <- columns_valid(data, c("fx", "fy"))

  # If neither pair is fully provided, throw an error
  if (!(has_angle_distance || has_fx_fy)) {
    cli::cli_abort("You must provide either both 'fx' and 'fy' or both 'angle' and 'norm' aesthetics.")
  }

  # Return a list indicating which pair is present
  return(list(
    has_angle_distance = has_angle_distance,
    has_fx_fy = has_fx_fy
  ))
}

#' @keywords internal
# Compute ellipse parameters based on covariance - geom_vector_smooth
compute_ellipse_params <- function(var_fx, var_fy, cov_fx_fy, conf_level = 0.95) {
  # Construct the covariance matrix
  cov_matrix <- matrix(c(var_fx, cov_fx_fy, cov_fx_fy, var_fy), nrow = 2)

  # Eigen decomposition
  eig <- eigen(cov_matrix)

  # Eigenvalues and eigenvectors
  eigenvalues <- eig$values
  eigenvectors <- eig$vectors

  # Compute the angle of rotation in degrees
  angle <- atan2(eigenvectors[2,1], eigenvectors[1,1]) * (180 / pi)

  # Compute the scaling factor based on the desired confidence level
  # For a 95% confidence ellipse in 2D, the scaling factor is sqrt(5.991)
  # This comes from the chi-square distribution with 2 degrees of freedom
  chi_sq_val <- stats::qchisq(conf_level, df = 2)
  scale_factor <- sqrt(chi_sq_val)

  # Width and height of the ellipse (2 * axis lengths)
  width <- 2 * scale_factor * sqrt(eigenvalues[1])
  height <- 2 * scale_factor * sqrt(eigenvalues[2])

  return(list(width = width, height = height, angle = angle))
}

#' @keywords internal
# range of the Gaussian variogram used by the kriging smoother, estimated from
# the data: the mean of the ranges of Gaussian variograms fitted to fx and fy
# separately, each started from a third of the data's diagonal. that starting
# value is used for a component whose fit fails
kriging_range <- function(df, v) {
  coords <- sp::coordinates(df)
  diagonal <- sqrt(sum(apply(coords, 2, function(z) diff(range(z)))^2))
  start <- diagonal / 3
  fit_one <- function(id) {
    vv <- v[v$id == id, ]
    s <- stats::var(df[[id]])
    fitted <- tryCatch(
      suppressWarnings(gstat::fit.variogram(
        vv, gstat::vgm(psill = 0.9 * s, model = "Gau", range = start, nugget = 0.1 * s)
      )),
      error = function(e) NULL
    )
    r <- if (is.null(fitted)) NA_real_ else fitted$range[fitted$model == "Gau"]
    if (length(r) == 1 && is.finite(r) && r > 0 && r < 10 * diagonal) r else start
  }
  mean(c(fit_one("fx"), fit_one("fy")))
}

#' @keywords internal
# density of the angle theta of a bivariate normal vector N(mu, Sigma): the
# polar-coordinate density f(r cos theta, r sin theta) |r| integrated over
# r >= 0, in closed form. with u = (cos theta, sin theta), a = u' S^-1 u,
# b = u' S^-1 mu and c = mu' S^-1 mu
angle_density <- function(theta, mu, Sigma) {
  Si <- solve(Sigma)
  c0 <- drop(crossprod(mu, Si %*% mu))
  k <- 2 * pi * sqrt(det(Sigma))
  vapply(theta, function(th) {
    u <- c(cos(th), sin(th))
    a <- drop(crossprod(u, Si %*% u))
    b <- drop(crossprod(u, Si %*% mu))
    (exp(-c0 / 2) / a +
       sqrt(2 * pi) * b * a^(-1.5) * exp(-(c0 - b^2 / a) / 2) * stats::pnorm(b / sqrt(a))) / k
  }, numeric(1))
}

#' @keywords internal
# offsets from the predicted angle theta0 of the alpha/2 and 1 - alpha/2
# quantiles of the angle distribution, taken over the circle centered on
# theta0. the density can be a very narrow peak at theta0, which a single
# integrate() call over a wide range can miss, so the integrals are split at
# points around theta0 spaced by multiples of the angle's approximate
# (delta-method) standard error; the result is still the exact distribution
angle_quantiles <- function(mu, Sigma, theta0, alpha) {
  f <- function(t) angle_density(t, mu, Sigma)

  g <- c(-mu[2], mu[1]) / sum(mu^2)
  se <- sqrt(drop(crossprod(g, Sigma %*% g)))
  if (!is.finite(se) || se <= 0) se <- 1
  offsets <- se * c(0.5, 2, 8, 32, 128)
  breaks <- sort(unique(theta0 + c(-pi, -offsets[offsets < pi], 0, offsets[offsets < pi], pi)))

  # integral of f from a to b, split at the breaks between them
  integ <- function(a, b) {
    pts <- c(a, breaks[breaks > a & breaks < b], b)
    sum(vapply(seq_len(length(pts) - 1), function(i) {
      stats::integrate(f, pts[i], pts[i + 1], subdivisions = 500L, rel.tol = 1e-8)$value
    }, numeric(1)))
  }

  F_lower <- function(t) integ(theta0 - pi, t)
  below <- F_lower(theta0)
  F_upper <- function(t) below + integ(theta0, t)

  lower <- if (below <= alpha / 2) -pi else
    stats::uniroot(function(t) F_lower(t) - alpha / 2, c(theta0 - pi, theta0), tol = 1e-10)$root - theta0
  upper <- if (F_upper(theta0 + pi) <= 1 - alpha / 2) pi else
    stats::uniroot(function(t) F_upper(t) - (1 - alpha / 2), c(theta0, theta0 + pi), tol = 1e-10)$root - theta0
  c(lower, upper)
}

#' @keywords internal
# Define a helper function to compute wedge endpoints (unchanged) - geom_vector_smooth
compute_prediction_endpoints <- function(x, y, fx, fy, angle_lower, angle_upper) {
  # Validate inputs
  if (!is.numeric(x) || length(x) != 1) {
    cli::cli_abort("Input 'x' must be a single numeric value.")
  }
  if (!is.numeric(y) || length(y) != 1) {
    cli::cli_abort("Input 'y' must be a single numeric value.")
  }
  if (!is.numeric(fx) || length(fx) != 1) {
    cli::cli_abort("Input 'fx' must be a single numeric value.")
  }
  if (!is.numeric(fy) || length(fy) != 1) {
    cli::cli_abort("Input 'fy' must be a single numeric value.")
  }
  if (!is.numeric(angle_lower) || length(angle_lower) != 1) {
    cli::cli_abort("Input 'angle_lower' must be a single numeric value in radians.")
  }
  if (!is.numeric(angle_upper) || length(angle_upper) != 1) {
    cli::cli_abort("Input 'angle_upper' must be a single numeric value in radians.")
  }

  # Calculate the magnitude of the predicted vector
  magnitude <- sqrt(fx^2 + fy^2)

  # Compute the endpoints based on absolute angles
  xend_lower <- x + magnitude * cos(angle_lower)
  yend_lower <- y + magnitude * sin(angle_lower)

  xend_upper <- x + magnitude * cos(angle_upper)
  yend_upper <- y + magnitude * sin(angle_upper)

  # Create and return the output dataframe
  result_df <- data.frame(
    xend_lower = xend_lower,
    yend_lower = yend_lower,
    xend_upper = xend_upper,
    yend_upper = yend_upper
  )

  return(result_df)
}

#' @keywords internal
# create circles in geom_vector_smooth
create_circle_data <- function(x, y, radius, n = 100, group) {
  angle <- seq(0, 2 * pi, length.out = n)
  data.frame(
    x = x + radius * cos(angle),
    y = y + radius * sin(angle),
    group = group
  )
}

#' @keywords internal
# create circles in geom_vector_smooth
create_wedge_data <- function(
    x, y, xend_upper, yend_upper, xend_lower, yend_lower,
    xend, yend, id, n_points = 100,
    outer_radius = NULL,
    inner_radius = 0
) {
  # Calculate angles using atan2 for upper, lower, and midpoint
  angle_upper <- atan2(yend_upper - y, xend_upper - x) + 2*pi#%% (2 * pi)
  angle_lower <- atan2(yend_lower - y, xend_lower - x) + 2*pi#%% (2 * pi)
  midpoint_angle <- atan2(yend - y, xend - x) + 2*pi#%% (2 * pi)

  if(angle_upper < angle_lower){
    angle_upper <- angle_upper + 2*pi
  }

  # Calculate the shift to bring the midpoint to 0 radians
  shift <- -midpoint_angle

  # Shift all angles by the same amount
  shifted_upper <- (angle_upper + shift) #%% (2 * pi)
  shifted_lower <- (angle_lower + shift) #%% (2 * pi)

  # Generate arc points from upper (positive) to lower (negative)
  arc_angles <- seq(shifted_upper, shifted_lower, length.out = n_points)

  # Create outer arc
  arc_x_outer <- outer_radius * cos(arc_angles)
  arc_y_outer <- outer_radius * sin(arc_angles)

  # Rotate the arc points back to the original coordinate system
  final_x_outer <- x + arc_x_outer * cos(-shift) - arc_y_outer * sin(-shift)
  final_y_outer <- y + arc_x_outer * sin(-shift) + arc_y_outer * cos(-shift)

  if (inner_radius > 0) {
    # if (inner_radius > 0 && !is.null(inner_radius)) {
    # Create inner arc
    arc_x_inner <- inner_radius * cos(arc_angles)
    arc_y_inner <- inner_radius * sin(arc_angles)

    # Rotate the inner arc points back to the original coordinate system
    final_x_inner <- x + arc_x_inner * cos(-shift) - arc_y_inner * sin(-shift)
    final_y_inner <- y + arc_x_inner * sin(-shift) + arc_y_inner * cos(-shift)

    # Combine outer and inner arcs to form an annular wedge
    wedge_data <- data.frame(
      x = c(final_x_outer, rev(final_x_inner)),
      y = c(final_y_outer, rev(final_y_inner)),
      group = rep(id, each = length(final_x_outer) + length(final_x_inner)),
      id = rep(id, each = length(final_x_outer) + length(final_x_inner))
    )

  } else {
    # Create regular wedge by connecting outer arc to center
    wedge_data <- data.frame(
      x = c(x, final_x_outer, x),
      y = c(y, final_y_outer, y),
      group = rep(id, length.out = n_points + 2),
      id = rep(id, length.out = n_points + 2)
    )
  }

  return(wedge_data)
}

#' @keywords internal
# Helper function to create ellipse polygon data
create_ellipse_data <- function(x_center, y_center, width, height, angle, n_points = 50) {
  theta <- seq(0, 2 * pi, length.out = n_points)
  ellipse <- data.frame(
    theta = theta,
    x = width / 2 * cos(theta),
    y = height / 2 * sin(theta)
  )

  # Rotation matrix
  rotation_matrix <- matrix(c(cos(angle * pi / 180), -sin(angle * pi / 180),
                              sin(angle * pi / 180),  cos(angle * pi / 180)),
                            nrow = 2)

  rotated <- as.matrix(ellipse[, c("x", "y")]) %*% rotation_matrix
  ellipse$x <- rotated[,1] + x_center
  ellipse$y <- rotated[,2] + y_center

  return(ellipse)
}

#' @keywords internal
predict_theta_interval <- function(x, y, mux, muy, Sigma, rho = NULL, conf_level) {

  # Validate inputs
  if (!is.numeric(x) || length(x) != 1) {
    cli::cli_abort("Input 'x' must be a single numeric value.")
  }
  if (!is.numeric(y) || length(y) != 1) {
    cli::cli_abort("Input 'y' must be a single numeric value.")
  }
  if (!is.numeric(mux) || length(mux) != 1) {
    cli::cli_abort("Input 'mux' must be a single numeric value.")
  }
  if (!is.numeric(muy) || length(muy) != 1) {
    cli::cli_abort("Input 'muy' must be a single numeric value.")
  }
  if (!is.matrix(Sigma) || any(dim(Sigma) != c(2, 2))) {
    cli::cli_abort("Input 'Sigma' must be a 2x2 covariance matrix.")
  }
  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    cli::cli_abort("Input 'conf_level' must be a numeric value between 0 and 1.")
  }

  # If rho is not provided, calculate it (kept for compatibility)
  if (is.null(rho)) {
    sigma_x <- sqrt(Sigma[1, 1])
    sigma_y <- sqrt(Sigma[2, 2])
    rho <- Sigma[1, 2] / (sigma_x * sigma_y)
  }

  # Compute the predicted vector's angle (theta_pred) in [0, 2*pi)
  theta_pred <- atan2(muy, mux)
  if (theta_pred < 0) theta_pred <- theta_pred + 2*pi

  # the predicted vector has no direction
  if (mux^2 + muy^2 == 0) {
    return(data.frame(min_angle = NA, max_angle = NA))
  }

  # the interval comes from the exact distribution of the angle of the vector
  # tip (fx, fy) ~ N(mu, Sigma): its density is the joint density in polar
  # coordinates, f(r cos theta, r sin theta) |r|, integrated over r >= 0,
  # which has the closed form in angle_density(). the bounds are the
  # alpha/2 and 1 - alpha/2 quantiles of the angle around the predicted one
  mu <- c(mux, muy)
  alpha <- 1 - conf_level
  bounds <- if (det(Sigma) > 0) {
    tryCatch(angle_quantiles(mu, Sigma, theta_pred, alpha), error = function(e) NULL)
  }
  if (is.null(bounds)) {
    # no spread in the prediction: the wedge has no width
    bounds <- c(0, 0)
  }

  raw_lower <- theta_pred + bounds[1]
  raw_upper <- theta_pred + bounds[2]

  # If the raw interval width is >= 2*pi, cap the interval at the full circle.
  if ((raw_upper - raw_lower) >= 2*pi) {
    # cat("Delta method interval exceeds full circle; capping at [0, 2*pi].\n")
    return(data.frame(min_angle = .001, max_angle = 2*pi))
  }

  # Unwrap the interval so it is continuous:
  if (raw_lower < 0) {
    lower_unwrapped <- raw_lower + 2*pi
    upper_unwrapped <- raw_upper + 2*pi
  } else if (raw_upper >= 2*pi) {
    lower_unwrapped <- raw_lower - 2*pi
    upper_unwrapped <- raw_upper - 2*pi
  } else {
    lower_unwrapped <- raw_lower
    upper_unwrapped <- raw_upper
  }

  data.frame(min_angle = lower_unwrapped, max_angle = upper_unwrapped)
}


