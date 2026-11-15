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


# the gam smoothers fit fx and fy together, each with the smooth s(x, y), in
# mgcv's multivariate normal family, which also estimates the covariance of
# the residuals of fx and fy. the joint fit uses at most half as many basis
# functions as locations. where it cannot be fitted (fewer than 10 locations,
# or data without noise, whose residual covariance is singular), fx and fy are
# fitted separately and their residuals are taken to be uncorrelated
fit_joint_gam <- function(data) {
  k <- gam_basis_size(data)
  n_loc <- nrow(unique(data[, c("x", "y")]))
  joint <- tryCatch(
    suppressWarnings(mgcv::gam(
      list(fx ~ s(x, y, bs = "tp", k = min(k, max(4, n_loc %/% 2))),
           fy ~ s(x, y, bs = "tp", k = min(k, max(4, n_loc %/% 2)))),
      family = mgcv::mvn(d = 2), data = data
    )),
    error = function(e) NULL
  )
  if (!is.null(joint)) {
    S <- tryCatch(solve(crossprod(joint$family$data$R)), error = function(e) NULL)
    if (!is.null(S) && all(is.finite(S)) && all(eigen(S, symmetric = TRUE)$values > 0)) {
      return(list(joint = joint))
    }
  }
  list(
    fx = mgcv::gam(fx ~ s(x, y, bs = "tp", k = k), data = data),
    fy = mgcv::gam(fy ~ s(x, y, bs = "tp", k = k), data = data)
  )
}

# predictions of fit_joint_gam() at the points newdata (columns x and y): the
# fitted fx and fy and the covariance of a new observation there, which is the
# covariance of the fitted vector plus the residual covariance. mgcv estimates
# the residual covariance by maximum likelihood; it is scaled by n / (n - edf)
# for each component, as the residual variance of a single gam is
predict_joint_gam <- function(fit, newdata, se = TRUE) {
  newdata <- newdata[, c("x", "y")]

  if (is.null(fit$joint)) {
    px <- predict(fit$fx, newdata = newdata, se.fit = se)
    py <- predict(fit$fy, newdata = newdata, se.fit = se)
    if (!se) return(data.frame(fx = as.numeric(px), fy = as.numeric(py)))
    return(data.frame(
      fx = as.numeric(px$fit), fy = as.numeric(py$fit),
      var_fx = as.numeric(px$se.fit^2 + fit$fx$sig2),
      var_fy = as.numeric(py$se.fit^2 + fit$fy$sig2),
      cov_fx_fy = 0
    ))
  }

  j <- fit$joint
  Xp <- predict(j, newdata = newdata, type = "lpmatrix")
  lpi <- attr(Xp, "lpi")
  b <- stats::coef(j)
  X1 <- Xp[, lpi[[1]], drop = FALSE]
  X2 <- Xp[, lpi[[2]], drop = FALSE]
  out <- data.frame(fx = drop(X1 %*% b[lpi[[1]]]), fy = drop(X2 %*% b[lpi[[2]]]))
  if (!se) return(out)

  V <- j$Vp
  n <- nrow(j$model)
  edf <- vapply(lpi, function(i) sum(j$edf[i]), numeric(1))
  scale <- n / (n - edf)
  S <- solve(crossprod(j$family$data$R)) * sqrt(outer(scale, scale))

  out$var_fx <- rowSums((X1 %*% V[lpi[[1]], lpi[[1]]]) * X1) + S[1, 1]
  out$var_fy <- rowSums((X2 %*% V[lpi[[2]], lpi[[2]]]) * X2) + S[2, 2]
  out$cov_fx_fy <- rowSums((X1 %*% V[lpi[[1]], lpi[[2]]]) * X2) + S[1, 2]
  out
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

# a vector given in polar form has an angle and a magnitude; the magnitude
# may be mapped as `norm` (the name of the computed variable) or `distance`
norm_as_distance <- function(data) {
  if ("norm" %in% names(data) && !all(is.na(data$norm)) &&
      (!"distance" %in% names(data) || all(is.na(data$distance)))) {
    data$distance <- data$norm
  }
  data
}

# the smoothing layers always use their own stat, which fits the model; warn
# when a different stat is supplied, since it is not used
warn_if_stat_ignored <- function(stat, own, own_name, fn) {
  if (is.null(stat) || identical(stat, own) || identical(stat, own_name)) return(invisible())
  cli::cli_warn(c(
    "{.fn {fn}} always uses {.cls {class(own)[1]}}, which fits the model.",
    "i" = "The supplied {.arg stat} is ignored."
  ))
  invisible()
}
