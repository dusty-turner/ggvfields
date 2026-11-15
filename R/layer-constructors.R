# Each geom_*() / stat_*() pair is a thin wrapper around one of the internal
# constructors in this file. A wrapper only fixes what makes it that wrapper
# (its geom or stat, and anything else passed through construct_layer()'s `...`),
# so the logic of a layer lives in one place and a pair cannot drift apart.

# Calls `constructor` with every argument of the calling wrapper: `args` is the
# wrapper's `as.list(environment())` and `dots` its `list(...)`. Values given in
# `...` here are added to, or take the place of, the wrapper's arguments.
construct_layer <- function(constructor, args, dots, ...) {
  args <- c(args, dots)
  fixed <- list(...)
  args[names(fixed)] <- fixed
  do.call(constructor, args)
}

# Merge a layer's default mapping with the user's; the user's mappings win.
merge_default_mapping <- function(default_mapping, mapping) {
  if (is.null(mapping)) {
    default_mapping
  } else {
    modifyList(default_mapping, mapping)
  }
}

# Was a fixed colour, e.g. color = "red", passed through `...`?
has_fixed_colour <- function(...) {
  any(c("color", "colour") %in% names(list(...)))
}


# geom_vector(), stat_vector(), geom_vector2(), stat_vector2()
vector_layer <- function(
    mapping, data, stat, geom, position, ...,
    na.rm, show.legend, inherit.aes,
    center, normalize = TRUE, tail_point, eval_point, L, arrow,
    default_mapping = NULL
) {

  if (!is.null(default_mapping)) {
    mapping <- merge_default_mapping(default_mapping, mapping)
  }

  layer(
    stat = stat,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      center = center,
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      L = L,
      type = "vector",
      ...
    )
  )
}


# geom_vector_field(), stat_vector_field()
vector_field_layer <- function(
    mapping, data, stat, geom, position, ...,
    na.rm, show.legend, inherit.aes,
    fun, xlim, ylim, n, args, center, normalize, tail_point, eval_point, grid,
    lineend, linejoin, linemitre, arrow,
    extra_params = list()
) {

  if (!has_fixed_colour(...)) {
    default_mapping <- aes(color = after_stat(norm))
  } else {
    default_mapping <- aes()
  }

  mapping <- modifyList(default_mapping, mapping %||% aes())

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  layer(
    stat = stat,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(
      list(
        fun = fun,
        xlim = xlim,
        ylim = ylim,
        n = n,
        args = args,
        method = "euler",
        na.rm = na.rm,
        center = center,
        normalize = normalize,
        type = "vector",
        tail_point = tail_point,
        eval_point = eval_point,
        grid = grid,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre,
        arrow = arrow,
        ...
      ),
      extra_params
    )
  )
}


# geom_vector_field2(), stat_vector_field2()
vector_field2_layer <- function(
    mapping, data, stat, geom, position, ...,
    na.rm, show.legend, inherit.aes,
    fun, xlim, ylim, n, args, center, tail_point, eval_point, grid,
    lineend, linejoin, linemitre, arrow
) {

  mapping <- merge_default_mapping(aes(length = after_stat(norm)), mapping)

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  layer(
    stat = stat,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      args = args,
      method = "euler",
      na.rm = na.rm,
      center = center,
      normalize = TRUE,
      type = "vector",
      tail_point = tail_point,
      eval_point = eval_point,
      T = 1,
      grid = grid,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      ...
    )
  )
}


# geom_stream_field(), stat_stream_field()
stream_field_layer <- function(
    mapping, data, stat, geom, position, ...,
    na.rm, show.legend, inherit.aes,
    fun, xlim, ylim, n, args, max_it, tol, T, L, center, type, normalize,
    tail_point, eval_point, grid, method, lineend, linejoin, linemitre, arrow
) {

  if (type == "vector") {
    default_mapping <- aes(color = after_stat(norm))
  } else if (type == "stream") {
    default_mapping <- aes(color = after_stat(avg_spd))
  } else {
    cli::cli_abort("Unknown type specified. Valid values are 'vector' or 'stream'.")
  }

  mapping <- merge_default_mapping(default_mapping, mapping)

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)
  method <- match.arg(method, c("rk4", "euler", "lsoda"))

  layer(
    stat = stat,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      args = args,
      method = method,
      na.rm = na.rm,
      max_it = max_it,
      tol = tol,
      T = T,
      L = L,
      center = center,
      type = type,
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      grid = grid,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      ...
    )
  )
}


# geom_stream_field2(), stat_stream_field2()
stream_field2_layer <- function(
    mapping, data, stat, geom, position, ...,
    na.rm, show.legend, inherit.aes,
    fun, xlim, ylim, n, args, max_it, tol, L, center, type,
    tail_point, eval_point, grid, lineend, linejoin, linemitre, method
) {

  mapping <- merge_default_mapping(aes(color = after_stat(NULL)), mapping)

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)
  method <- match.arg(method, c("rk4", "euler", "lsoda"))

  layer(
    stat = stat,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      args = args,
      method = method,
      na.rm = na.rm,
      max_it = max_it,
      tol = tol,
      L = L,
      center = center,
      type = type,
      # streams are grown to L and then all cropped back to the time of the
      # fastest one, so their lengths show relative speed; vectors keep the
      # same length
      normalize = type != "stream",
      tail_point = tail_point,
      eval_point = eval_point,
      grid = grid,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = NULL,
      ...
    )
  )
}


# geom_gradient_field(), stat_gradient_field(), geom_gradient_field2(),
# stat_gradient_field2(). `map_colour` is whether colour is mapped by default.
gradient_field_layer <- function(
    mapping, data, stat, geom, position, ...,
    na.rm, show.legend, inherit.aes,
    fun, xlim, ylim, n, max_it, T, L, center, type, normalize,
    tail_point, eval_point, grid, lineend, linejoin, linemitre, arrow,
    map_colour
) {

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  if (map_colour) {

    if (type == "stream") {
      default_mapping <- aes(color = after_stat(avg_spd))
    } else if (type == "vector") {
      default_mapping <- aes(color = after_stat(norm))
    } else {
      cli::cli_abort("`type` must be either 'stream' or 'vector', not {.val {type}}")
    }

    mapping <- merge_default_mapping(default_mapping, mapping)

  } else if (type == "vector") {

    # geom_gradient_field2(type = "vector") maps the norm to length, like
    # geom_vector_field2(), so vectors are scaled by the size of the gradient.
    # streams ignore length, so they get no mapping (and no empty legend)
    mapping <- merge_default_mapping(aes(length = after_stat(norm)), mapping)

  }

  if (missing(fun) || !is.function(fun)) {
    cli::cli_abort("Please provide a valid scalar function `fun` that takes a numeric vector and returns a single numeric value.")
  }

  gradient_fun <- function(u) {
    if (!is.numeric(u) || length(u) != 2) {
      cli::cli_abort("Input to the gradient function must be a numeric vector of length 2.")
    }
    numDeriv::grad(func = fun, x = u)
  }

  layer(
    stat = stat,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = gradient_fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      na.rm = na.rm,
      max_it = max_it,
      T = T,
      L = L,
      center = center,
      type = type,
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      grid = grid,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      ...
    )
  )
}


# geom_gradient_smooth(), stat_gradient_smooth()
gradient_smooth_layer <- function(
    mapping, data, stat, geom, position, ...,
    na.rm, show.legend, inherit.aes,
    formula, xlim, ylim, n, max_it, T, L, center, type, normalize,
    tail_point, eval_point, grid, lineend, linejoin, linemitre, arrow
) {

  n <- ensure_length_two(n)

  mapping <- merge_default_mapping(aes(x = x, y = y, z = z), mapping)

  default_color_aes <- if (type == "stream") {
    aes(color = after_stat(avg_spd))
  } else if (type == "vector") {
    aes(color = after_stat(norm))
  } else {
    cli::cli_abort("`type` must be either 'stream' or 'vector', not {.val {type}}")
  }

  if (!has_fixed_colour(...)) {
    mapping <- modifyList(mapping, default_color_aes)
  }

  layer(
    stat = stat,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      formula = formula,
      xlim = xlim,
      ylim = ylim,
      n = n,
      na.rm = na.rm,
      max_it = max_it,
      T = T,
      L = L,
      center = center,
      type = type,
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      grid = grid,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      ...
    )
  )
}


# geom_stream_smooth(), stat_stream_smooth(). `formula_supplied` is whether the
# user gave a formula, which a wrapper knows from missing(formula).
stream_smooth_layer <- function(
    mapping, data, stat, geom, position, ...,
    na.rm, show.legend, inherit.aes,
    n, xlim, ylim, normalize, center, type, formula, method, eval_points,
    lineend, linejoin, linemitre, arrow,
    formula_supplied
) {

  warn_if_formula_ignored(method, formula_supplied)

  # Inspect the LHS of the formula. If it is cbind(xend, yend), then change it
  lhs <- formula[[2]]
  if (is.call(lhs) && identical(lhs[[1]], as.name("cbind"))) {
    lhs_vars <- sapply(as.list(lhs[-1]), as.character)
    if (all(lhs_vars %in% c("xend", "yend")) ||
        all(lhs_vars %in% c("distance", "angle"))) {
      # Change to use fx and fy instead
      formula[[2]] <- substitute(cbind(fx, fy))
    }
  }

  if (is.null(mapping)) mapping <- aes(x = x, y = y, fx = fx, fy = fy)

  n <- ensure_length_two(n)
  dots <- list(...)
  if (!has_fixed_colour(...)) {
    dots$color <- "blue"
  }

  layer(
    stat = stat,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(
      list(
        na.rm = na.rm,
        n = n,
        normalize = normalize,
        center = center,
        formula = formula,
        method = method,
        xlim = xlim,
        ylim = ylim,
        grid = eval_points,
        type = type,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre,
        arrow = arrow
      ),
      dots
    )
  )
}


# geom_vector_smooth(), stat_vector_smooth()
vector_smooth_layer <- function(
    mapping, data, stat, geom, position, ...,
    na.rm, show.legend, inherit.aes,
    n, method, se, se.circle, pi_type, conf_level, formula, eval_points, arrow,
    formula_supplied
) {

  warn_if_formula_ignored(method, formula_supplied)

  layer(
    stat = stat,
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


# geom_potential(), stat_potential()
potential_layer <- function(
    mapping, data, stat, geom, position, ...,
    na.rm, inherit.aes, show.legend,
    fun, xlim, ylim, n, tol, verify_conservative
) {

  if (is.null(data)) {
    data <- data.frame(x = NA_real_, y = NA_real_)
  }

  # Ensure the potential function is provided
  if (missing(fun)) {
    cli::cli_abort("Parameter `fun` must be provided to compute the potential function.")
  }

  mapping <- merge_default_mapping(aes(fill = after_stat(potential)), mapping)

  layer(
    stat = stat,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      tol = tol,
      verify_conservative = verify_conservative,
      ...
    )
  )
}


# geom_stream(), stat_stream()
stream_layer <- function(
    mapping, data, stat, geom, position, ...,
    na.rm, show.legend, inherit.aes,
    arrow.fill, lineend, linejoin, linemitre, arrow
) {

  default_mapping <- aes(t = after_stat(t), color = after_stat(t))

  # the default colour mapping is dropped if the user maps or fixes colour
  if (!is.null(mapping)) {
    if (!( "colour" %in% names(mapping) || has_fixed_colour(...) )) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    if (has_fixed_colour(...)) {
      mapping <- aes(t = after_stat(t))
    } else {
      mapping <- default_mapping
    }
  }

  # Map 'id' to 'group' if provided.
  if (!is.null(mapping) && "id" %in% names(mapping)) {
    mapping$group <- mapping$group %||% mapping$id
  }

  layer(
    stat = stat,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      arrow.fill = arrow.fill,
      type = "stream",
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      ...
    )
  )
}
