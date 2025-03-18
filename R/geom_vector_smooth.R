#' Create a Smooth Vector Plot Layer
#'
#' `geom_vector_smooth()` creates a ggplot2 layer that visualizes a smooth
#' vector field. It takes raw vector data and applies smoothing (via a
#' multivariate linear model) to estimate the underlying vector field. This
#' functionality is analogous to `geom_smooth()` in ggplot2 but is tailored for
#' vector data rather than scalar responses.
#'
#' @importFrom stats qt
#' @importFrom stats integrate
#' @importFrom sp coordinates coordinates<-
#' @importFrom gstat gstat variogram fit.lmc vgm
#'
#' @param mapping A set of aesthetic mappings created by \code{ggplot2::aes()}.
#'   **Required:** Must include **`x`** and **`y`**; vector displacements are defined by
#'   **`fx`** and **`fy`**.
#' @param data A data frame containing the raw vector data.
#' @param stat The statistical transformation to use on the data (default:
#'   `"vector_smooth"`).
#' @param position Position adjustment, either as a string or the result of a
#'   position adjustment function.
#' @param n An integer vector specifying the number of grid points along each
#'   axis for smoothing.
#' @param method Character. Specifies the smoothing method. Currently, the only
#'   supported method is `"lm"`, which fits a multivariate linear model to
#'   predict the vector displacements (`fx`, `fy`) from `x` and `y`.
#' @param se Logical. If `TRUE`, prediction (confidence) intervals are computed
#'   and plotted.
#' @param se.circle Logical. If `TRUE`, circles are drawn around the origin of
#'   each vector to represent the radius of the prediction interval.
#' @param conf_level Numeric. Specifies the confidence level for the prediction
#'   intervals. Default is `0.95`.
#' @param eval_points A data frame of evaluation points. If provided, these
#'   specify the grid where the smoothing model is evaluated. If `NULL`, a grid
#'   is generated based on `n`.
#' @param pi_type Character. Determines the display style for prediction
#'   intervals:
#'   - `"wedge"` (default): Angular wedges are drawn.
#'   - `"ellipse"`: Ellipses are used to represent the covariance of the predictions.
#'   If `pi_type` is set to `"ellipse"` and `eval_points` is `NULL`, it will
#'   revert to `"wedge"`.
#' @param arrow A \code{grid::arrow()} specification for arrowheads on the smoothed
#'   vectors.
#' @param formula A formula specifying the multivariate linear model used for
#'   smoothing. The default is `cbind(fx, fy) ~ x * y`.
#' @param ... Other arguments passed to \code{ggplot2::layer()} and the underlying
#'   geometry/stat.
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
#' The `"lm"` method fits a multivariate linear model to predict vector
#' displacements (`fx` and `fy`) based on the coordinates `x` and `y`, including
#' interaction terms (`x * y`). This model smooths the raw vector data to
#' provide an estimate of the underlying vector field.
#'
#' **Prediction Intervals:**
#' When `se = TRUE`, prediction intervals are computed for the smoothed vectors.
#' Two types of intervals are supported:
#'   - **Ellipse:** Ellipses represent the joint uncertainty (covariance) in the predicted `fx` and `fy`.
#'   - **Wedge:** Wedges (angular sectors) indicate the range of possible vector directions and magnitudes.
#' The type of interval displayed is controlled by `pi_type`, and the confidence
#' level is set via `conf_level`.
#'
#' @return A ggplot2 layer that can be added to a plot to create a smooth vector
#'   field visualization.
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
#'   geom_vector_smooth(aes(fx = fx, fy = fy), eval_points = eval_points, conf_level = c(0.9)) +
#'   ggtitle("Smoothed Vector Field with Ellipse Intervals")
#'
#' # Example 3: Wedge with eval_points
#' ggplot(sample_points, aes(x = x, y = y)) +
#'   geom_vector(aes(fx = fx, fy = fy, color = NULL), center = FALSE, alpha = 0.2) +
#'   geom_vector_smooth(aes(fx = fx, fy = fy), eval_points = eval_points, pi_type = "ellipse") +
#'   ggtitle("Smoothed Vector Field with Wedge Intervals")
#'
#' @aliases geom_vector_smooth stat_vector_smooth geom_vector_smooth2 stat_vector_smooth2 StatVectorSmooth
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
   method = "lm",
   se = TRUE,
   se.circle = TRUE,
   pi_type = "ellipse",
   conf_level = c(.95, NA),
   formula = cbind(fx, fy) ~ x * y,
   eval_points = NULL,
   arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
   ) {

  layer(
    stat = StatVectorSmooth,
    data = data,
    mapping = mapping,
    geom = GeomVectorSmooth,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      method = method,
      se = se,
      se.circle = se.circle,
      pi_type = pi_type,
      conf_level = conf_level,
      arrow = arrow,
      eval_points = eval_points,
      formula = formula,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_vector_smooth
#' @format NULL
#' @usage NULL
#' @keywords internal
stat_vector_smooth <- function(mapping = NULL, data = NULL,
   geom = "vector_smooth",
   position = "identity",
   ...,
   na.rm = FALSE,
   show.legend = NA,
   inherit.aes = TRUE,
   n = c(11, 11),
   method = "lm",
   se = TRUE,
   se.circle = TRUE,
   conf_level = c(.95, NA),
   pi_type = "ellipse",
   formula = cbind(fx, fy) ~ x * y,
   eval_points = NULL,
   arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
   ) {

  layer(
    stat = StatVectorSmooth,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      method = method,
      se = se,
      se.circle = se.circle,
      pi_type = pi_type,
      conf_level = conf_level,
      arrow = arrow,
      eval_points = eval_points,
      formula = formula,
      na.rm = na.rm,
      ...
    )
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
  dropped_aes = c("distance", "angle"),
  # default_aes = aes(
  #   color = "#3366FF", linewidth = 0.5, linetype = 1, alpha = 1,
  #   angle = NA, distance = NA, fx = NA, fy = NA, length = NA,
  # ),
  default_aes = aes(
    linewidth = 0.5, linetype = 1, alpha = 1,
    angle = NA, distance = NA, fx = NA, fy = NA, length = NA
  ),

  compute_group = function(data, scales, n, method, se = TRUE, conf_level,
                           pi_type, eval_points = NULL, formula, ...) {
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
        stop("The 'eval_points' argument must contain 'x' and 'y' columns.")
      }

      grid <- eval_points

      # Compute base_radius based on data range
      data_range <- min(diff(range(data$x)), diff(range(data$y)))
      base_radius <- data_range / 2.5
    } else {
      # Generate grid when eval_points is not provided
      x_seq <- seq(min(data$x), max(data$x), length.out = n[1])
      y_seq <- seq(min(data$y), max(data$y), length.out = n[2])
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
      stop("Unexpected number of coefficients in the model. Please verify the formula and data.")
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

      # Convert data (a tibble) to a regular data frame and then to a SpatialPointsDataFrame
      df <- as.data.frame(data)
      sp::coordinates(df) <- ~ x + y

      # Build the cokriging model for fx and fy
      g <- gstat(id = "fx", formula = fx ~ 1, data = df)
      g <- gstat(g, id = "fy", formula = fy ~ 1, data = df)

      # Compute the experimental variograms (adjust cutoff as needed)
      # v <- variogram(g, cutoff = 5)
      v <- variogram(g)

      # Fit a linear model of coregionalization (LMC) using a spherical model example
      model <- vgm(psill = 1, model = "Sph", range = 3, nugget = 0.1)
      lmc <- fit.lmc(v, g, model = model)

      # Define prediction points: if eval_points is NULL, use grid; otherwise, use eval_points
      new_point <- if (is.null(eval_points)) grid else eval_points

      # Convert the new_point data frame to a SpatialPointsDataFrame
      coordinates(new_point) <- ~ x + y

      # Perform prediction using the fitted LMC model
      pred <- predict(lmc, newdata = new_point)

      # Extract prediction results into a simple data frame (optional)
      grid <- data.frame(x = new_point$x, y = new_point$y,
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
          predict_theta_interval,
          x = grid$x,
          y = grid$y,
          mux = grid$fx,
          muy = grid$fy,
          MoreArgs = list(Sigma = Sigma, rho = rho, conf_level = conf_level[1]),
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
      stop("Invalid value for pi_type. Must be 'wedge' or 'ellipse'.")
    }
    return(result)
  }
)




#' @rdname geom_vector_smooth
#' @export
GeomVectorSmooth <- ggproto(
  "GeomVectorSmooth",
  GeomSegment,
  required_aes = c("x", "y", "xend", "yend"),
  # default_aes = aes(
  #   linewidth = 0.5, linetype = 1, alpha = 1,
  #   fill = NULL, color = "#3366FF"
  # ),
  default_aes = aes(
    linewidth = 0.5, linetype = 1, alpha = 1,
    fill = NULL
  ),

  setup_data = function(data, params) {
    data$id <- seq_len(nrow(data))
    return(data)
  },

  draw_panel = function(
    data, panel_params, coord,
    arrow = NULL, se = TRUE, se.circle = FALSE,
    eval_points, pi_type
  ) {

    if (is.null(data$colour)) {
      data$colour <- "#3366FF"
    }

    grobs <- list()

    if (pi_type == "ellipse" && is.null(eval_points)) {
      # message("eval_points is NULL; changing pi_type from 'ellipse' to 'wedge'.")
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
        wedge_data$fill <- "grey60"
        wedge_data$colour <- NA

        # Draw the wedges using GeomPolygon
        wedge_grob <- GeomPolygon$draw_panel(
          wedge_data,
          panel_params = panel_params,
          coord = coord
        )

        raster_data <- data.frame(x = data$x, y = data$y)
        raster_data$z <- (data$max_angle - data$min_angle) * (180 / pi)

        n_rows <- length(unique(raster_data$y))
        n_cols <- length(unique(raster_data$x))

        raster_data <- raster_data[order(raster_data$y, raster_data$x), ]

        z_matrix <- matrix(raster_data$z, nrow = n_rows, ncol = n_cols, byrow = TRUE)

        nColors <- 100
        colorRamp <- colorRampPalette(c("black", "white"))

        # Normalize z_matrix to indices between 1 and nColors
        z_norm <- round((z_matrix - min(z_matrix)) / (max(z_matrix) - min(z_matrix)) * (nColors - 1)) + 1
        color_matrix <- matrix(colorRamp(nColors)[z_norm], nrow = nrow(z_matrix))

        raster_grob <- grid::nullGrob()

        # raster_grob <-  grid::rasterGrob(image = color_matrix,
        #                           width = unit(1, "npc"),
        #                           height = unit(1, "npc"),
        #                           interpolate = TRUE)


        grobs <- c(grobs, grid::gList(raster_grob, wedge_grob))

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

        grobs <- c(grobs, grid::gList(ellipse_grob))
      } else {
        stop("Invalid value for pi_type. Must be 'wedge' or 'ellipse'.")
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
