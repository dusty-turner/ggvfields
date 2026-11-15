# Each geom_*() / stat_*() pair must hand the same parameters to its layer when
# called with the same arguments. Every argument is set to a non-default value
# so that one that is dropped or hardcoded in either wrapper shows up.

f_vec <- function(u) c(-u[2], u[1])
f_scalar <- function(v) v[1]^2 + v[2]^2
pts <- data.frame(x = 0, y = 0)
ar <- grid::arrow(angle = 10)
line_args <- list(lineend = "round", linejoin = "mitre", linemitre = 5)

pairs <- list(
  vector = list(
    args = list(center = FALSE, normalize = FALSE, tail_point = TRUE, eval_point = TRUE, L = 2, arrow = ar)
  ),
  vector2 = list(
    args = list(center = TRUE, tail_point = FALSE, eval_point = TRUE, L = 2, arrow = ar)
  ),
  vector_field = list(
    args = c(
      list(fun = f_vec, xlim = c(0, 1), ylim = c(0, 2), n = c(3, 4), args = list(a = 1),
           center = FALSE, normalize = FALSE, tail_point = TRUE, eval_point = TRUE,
           grid = pts, arrow = ar),
      line_args
    ),
    # geom_vector_field() also passes max_it = 2, which vectors do not use
    known = "max_it"
  ),
  vector_field2 = list(
    args = c(
      list(fun = f_vec, xlim = c(0, 1), ylim = c(0, 2), n = 3, args = list(a = 1),
           center = TRUE, tail_point = FALSE, eval_point = TRUE, grid = pts, arrow = ar),
      line_args
    )
  ),
  stream_field = list(
    args = c(
      list(fun = f_vec, xlim = c(0, 1), ylim = c(0, 2), n = c(3, 4), args = list(a = 1),
           max_it = 50, tol = 1e-3, T = 2, L = 3, center = FALSE, type = "vector",
           normalize = FALSE, tail_point = TRUE, eval_point = TRUE, grid = pts,
           domain = function(u) TRUE, method = "euler", arrow = ar),
      line_args
    )
  ),
  stream_field2 = list(
    args = c(
      list(fun = f_vec, xlim = c(0, 1), ylim = c(0, 2), n = 3, args = list(a = 1),
           max_it = 50, tol = 1e-3, L = 3, center = TRUE, type = "vector",
           tail_point = FALSE, eval_point = TRUE, grid = pts, method = "lsoda"),
      line_args
    )
  ),
  gradient_field = list(
    args = c(
      list(fun = f_scalar, xlim = c(0, 1), ylim = c(0, 2), n = c(3, 4), max_it = 50,
           method = "euler", T = 2, L = 3, center = FALSE, type = "stream", normalize = FALSE,
           tail_point = TRUE, eval_point = TRUE, grid = pts, arrow = ar),
      line_args
    ),
    # fun is wrapped in a new closure on every call
    known = "fun"
  ),
  gradient_field2 = list(
    args = c(
      list(fun = f_scalar, xlim = c(0, 1), ylim = c(0, 2), n = 3, max_it = 50,
           method = "lsoda", T = 2, L = 3, center = TRUE, type = "vector", normalize = FALSE,
           tail_point = FALSE, eval_point = TRUE, grid = pts, arrow = ar),
      line_args
    ),
    known = "fun"
  ),
  gradient_smooth = list(
    args = c(
      list(formula = z ~ x, xlim = c(0, 1), ylim = c(0, 2), n = c(3, 4), max_it = 50,
           T = 2, L = 3, center = FALSE, type = "stream", normalize = FALSE,
           tail_point = TRUE, eval_point = TRUE, grid = pts, arrow = ar),
      line_args
    )
  ),
  stream_smooth = list(
    args = c(
      list(n = c(3, 4), xlim = c(0, 1), ylim = c(0, 2), normalize = FALSE, center = TRUE,
           type = "stream", formula = cbind(fx, fy) ~ x, method = "lm",
           eval_points = pts, arrow = ar),
      line_args
    )
  ),
  vector_smooth = list(
    args = list(n = c(3, 4), xlim = c(0, 1), ylim = c(0, 2), method = "lm", se = FALSE, se.circle = TRUE,
                pi_type = "wedge", conf_level = .8, formula = cbind(fx, fy) ~ x,
                eval_points = pts, arrow = ar),
    fixed_aes = list(linetype = 2)
  ),
  potential = list(
    args = list(fun = f_vec, xlim = c(0, 1), ylim = c(0, 2), n = 7, tol = 1e-3,
                verify_conservative = TRUE),
    fixed_aes = list(alpha = .5)
  ),
  stream = list(
    args = c(list(arrow.fill = "red", arrow = ar), line_args)
  )
)

for (family in names(pairs)) {

  test_that(paste0("geom_", family, "() and stat_", family, "() build the same layer"), {
    fixed_aes <- pairs[[family]]$fixed_aes
    if (is.null(fixed_aes)) fixed_aes <- list(linetype = 2, colour = "red")
    args <- c(pairs[[family]]$args, list(na.rm = TRUE), fixed_aes)
    known <- pairs[[family]]$known

    geom_layer <- do.call(paste0("geom_", family), args)
    stat_layer <- do.call(paste0("stat_", family), args)

    for (part in c("stat_params", "geom_params", "aes_params")) {
      geom_part <- geom_layer[[part]]
      stat_part <- stat_layer[[part]]
      expect_setequal(setdiff(names(geom_part), known), setdiff(names(stat_part), known))

      shared <- setdiff(names(geom_part), known)
      expect_equal(geom_part[shared], stat_part[shared], ignore_function_env = TRUE)
    }

    expect_identical(class(geom_layer$stat), class(stat_layer$stat))
    expect_identical(class(geom_layer$geom), class(stat_layer$geom))

    # the default mappings must match too, with and without a fixed colour
    labels <- function(layer) {
      if (length(layer$mapping) == 0) return(character(0))
      m <- vapply(layer$mapping, rlang::as_label, character(1))
      m[order(names(m))]
    }
    for (a in list(args, pairs[[family]]$args)) {
      expect_identical(labels(do.call(paste0("geom_", family), a)),
                       labels(do.call(paste0("stat_", family), a)))
    }
  })

}

test_that("construct_layer forwards arguments and lets fixed values win", {
  constructor <- function(a, b, ..., geom) list(a = a, b = b, geom = geom, dots = list(...))

  out <- construct_layer(constructor, list(a = 1, b = 2), list(colour = "red"), geom = "g", b = 3)

  expect_equal(out, list(a = 1, b = 3, geom = "g", dots = list(colour = "red")))
})

test_that("merge_default_mapping lets the user's mapping win", {
  default_mapping <- aes(colour = z, x = x)

  expect_identical(merge_default_mapping(default_mapping, NULL), default_mapping)

  merged <- merge_default_mapping(default_mapping, aes(colour = w, y = y))
  expect_setequal(names(merged), c("colour", "x", "y"))
  expect_equal(rlang::as_label(merged$colour), "w")
})
