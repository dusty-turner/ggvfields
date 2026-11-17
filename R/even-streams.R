# evenly spaced streamlines (Jobard and Lefer, 1997). streams are grown in both
# directions from a seed until they come within d_test of another stream (or
# of an earlier part of themselves), leave the box xlim x ylim, reach a sink,
# or reach a point where the field is undefined. new seeds are tried d_sep to
# either side of each finished stream, and then on a coarse grid, so the
# streams fill the box about d_sep apart.
#
# fun takes a point c(x = , y = ) and returns c(dx, dy). returns a list of
# data frames with columns t, x, y, d, l, avg_spd and norm, one per stream,
# each running in the direction of the flow.
even_streams <- function(fun, xlim, ylim, d_sep, d_test = d_sep / 2,
                         step = d_sep / 5, max_it = 1000,
                         tol = sqrt(.Machine$double.eps), seeds = NULL) {

  field <- function(p) {
    if (!all(is.finite(p))) return(c(NaN, NaN))
    as.numeric(fun(c(x = p[1], y = p[2])))
  }
  inside <- function(p) {
    p[1] >= xlim[1] && p[1] <= xlim[2] && p[2] >= ylim[1] && p[2] <= ylim[2]
  }

  # spatial hash of the points of finished streams, with cells d_sep wide
  cells <- new.env(hash = TRUE)
  key <- function(i, j) paste(i, j)
  cell_of <- function(p) c(floor((p[1] - xlim[1]) / d_sep), floor((p[2] - ylim[1]) / d_sep))
  add_points <- function(xy) {
    for (r in seq_len(nrow(xy))) {
      k <- key(cell_of(xy[r, ])[1], cell_of(xy[r, ])[2])
      cells[[k]] <- rbind(cells[[k]], xy[r, , drop = FALSE])
    }
  }
  # distance from p to the nearest point of a finished stream
  nearest <- function(p) {
    c0 <- cell_of(p)
    best <- Inf
    for (i in c0[1] + -1:1) for (j in c0[2] + -1:1) {
      pts <- cells[[key(i, j)]]
      if (!is.null(pts)) best <- min(best, sqrt((pts[, 1] - p[1])^2 + (pts[, 2] - p[2])^2))
    }
    best
  }

  # unit direction of the field, or NULL where it is undefined or vanishes
  direction <- function(p, sign) {
    f <- field(p)
    s <- sqrt(sum(f^2))
    if (!all(is.finite(f)) || s < tol) return(NULL)
    sign * f / s
  }

  # grow one half of a stream from p0, forward (sign = 1) or backward (-1),
  # with rk4 steps of arc length `step`. own points closer than d_test count
  # as a collision once they are a few steps behind, which stops closed orbits
  grow <- function(p0, sign) {
    pts <- matrix(p0, 1)
    p <- p0
    lag <- ceiling(2 * d_test / step) + 1
    for (it in seq_len(max_it)) {
      k1 <- direction(p, sign); if (is.null(k1)) break
      k2 <- direction(p + step / 2 * k1, sign); if (is.null(k2)) break
      k3 <- direction(p + step / 2 * k2, sign); if (is.null(k3)) break
      k4 <- direction(p + step * k3, sign); if (is.null(k4)) break
      v <- (k1 + 2 * k2 + 2 * k3 + k4) / 6
      # a turn of more than 90 degrees in one step: a sink or a jump
      if (sum(k1 * k4) < 0) break
      p_new <- p + step * v
      if (!inside(p_new) || nearest(p_new) < d_test) break
      n_pts <- nrow(pts)
      if (n_pts > lag) {
        own <- pts[seq_len(n_pts - lag), , drop = FALSE]
        if (min(sqrt((own[, 1] - p_new[1])^2 + (own[, 2] - p_new[2])^2)) < d_test) break
      }
      pts <- rbind(pts, p_new)
      p <- p_new
    }
    pts
  }

  streams <- list()
  queue <- list()

  try_seed <- function(p) {
    if (!inside(p) || nearest(p) < d_sep) return(NULL)
    if (is.null(direction(p, 1))) return(NULL)
    back <- grow(p, -1)
    fwd <- grow(p, 1)
    xy <- rbind(back[nrow(back):1, , drop = FALSE], fwd[-1, , drop = FALSE])
    if (nrow(xy) < 2) return(NULL)
    xy
  }

  accept <- function(xy) {
    add_points(xy)
    streams[[length(streams) + 1]] <<- xy
    queue[[length(queue) + 1]] <<- xy
  }

  # candidate seeds d_sep to either side of a stream, at points along it
  side_seeds <- function(xy) {
    n <- nrow(xy)
    if (n < 2) return(list())
    out <- list()
    every <- max(1, round(d_sep / step))
    for (i in seq(1, n, by = every)) {
      a <- xy[max(1, i - 1), ]; b <- xy[min(n, i + 1), ]
      tng <- b - a
      len <- sqrt(sum(tng^2))
      if (len == 0) next
      nrm <- c(-tng[2], tng[1]) / len
      out[[length(out) + 1]] <- xy[i, ] + d_sep * nrm
      out[[length(out) + 1]] <- xy[i, ] - d_sep * nrm
    }
    out
  }

  if (is.null(seeds)) seeds <- matrix(c(mean(xlim), mean(ylim)), 1)

  for (s in seq_len(nrow(seeds))) {
    xy <- try_seed(seeds[s, ])
    if (!is.null(xy)) accept(xy)
    while (length(queue) > 0) {
      current <- queue[[1]]
      queue[[1]] <- NULL
      for (cand in side_seeds(current)) {
        xy <- try_seed(cand)
        if (!is.null(xy)) accept(xy)
      }
    }
  }

  lapply(streams, function(xy) stream_frame(xy, field))
}

# the stream data frame of a path xy of the field `field`: arc length l and the
# time t it takes to follow the path at the field's speed
stream_frame <- function(xy, field) {
  xy <- unname(xy)
  n <- nrow(xy)
  d <- c(NA_real_, sqrt(diff(xy[, 1])^2 + diff(xy[, 2])^2))
  speed <- apply(xy, 1, function(p) sqrt(sum(field(p)^2)))
  dt <- if (n > 1) d[-1] / ((speed[-1] + speed[-n]) / 2) else numeric(0)
  t <- c(0, cumsum(dt))
  l <- c(0, cumsum(d[-1]))
  data.frame(
    t = t, x = xy[, 1], y = xy[, 2], d = d, l = l,
    avg_spd = if (n > 1 && t[n] > 0) l[n] / t[n] else NA_real_,
    norm = speed[ceiling(n / 2)]
  )
}
