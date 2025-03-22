library("hexSticker")

library("tidyverse")

library("ggvfields")

roots_of_unity <- function(n, origin = c(0,0), rot = 0, r = 1) {
  is.wholenumber <- function(x, tol = 1e-8) abs(n - round(n)) < tol
  stopifnot(is.wholenumber(n), n > 0)
  data.frame(
    "x" = r*cos(2*(0:(n-1))*pi/n + rot),
    "y" = r*sin(2*(0:(n-1))*pi/n + rot),
    "z" = complex(n),
    "k" = 0:(n-1L),
    "n" = as.integer(n)
  ) |>
    transform(
      "x" = x + origin[1],
      "y" = y + origin[2],
      "z" = complex(real = x, imaginary = y)
    ) |>
    zapsmall()
}

# roots_of_unity(5) |>
#   ggplot(aes(x,y)) +
#     geom_point() +
#     geom_path() +
#     coord_equal()
#
# roots_of_unity(5, rot = pi/2) |>
#   ggplot(aes(x,y)) +
#   geom_point() +
#   geom_path() +
#   coord_equal()
#
# roots_of_unity(5, origin = c(5, 10), rot = pi/2) |>
#   ggplot(aes(x,y)) +
#   geom_point() +
#   geom_path() +
#   coord_equal()


d <- 8

charge_positions <- roots_of_unity(d, rot = pi/2)[,c("x","y")] |> as.matrix()

# charge_positions <- rbind(charge_positions, c(0,0))

f <- efield_maker(
  charge_positions = charge_positions |> jitter(amount = .000001),
  # charges =  rep(+1, nrow(charge_positions)),
  charges =  c(1,1,-1,1,1,1,-1,1,1),
  log = TRUE
)
# f(1:2)

# grid <- grid_hex(xlim = 1.2*c(-1, 1), ylim = 1.2*c(-1, 1), d = .2) |>
#   filter(!near(x^2 + y^2, 0))

grid <- charge_positions |>
  split(1:nrow(charge_positions)) |>
  map_dfr(roots_of_unity, n = d, rot = pi/2, r = .25) |>
  select(x, y)

grid |>
  ggplot(aes(x, y)) +
  geom_point() +
  coord_equal()


ggplot() +
  geom_stream_field(
    fun = f,
    xlim = 1.2*c(-1,1), ylim = 1.2*c(-1,1),
    grid = grid,
    mapping = aes(alpha = after_stat(l)),
    L = .4
  ) +
  scale_color_viridis_c(option = "A", direction = +1) +
  coord_equal() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "gray20", color = "gray10"),
    legend.position = "none"
  )


library("scales")

pal_viridis(option = "A")(6) |> show_col()
pal_viridis(option = "D")(6) |> show_col()


set.seed(2434)

f <- function(u) c(-u[2], u[1])

r <- .25

grid <- grid_hex(xlim = c(-1, 1), ylim = c(-1, 1), d = r) |>
  purrr::modify(jitter, amount = r/2)

p <- ggplot() +
  geom_stream_field(
    fun = f,
    grid = grid,
    mapping = aes(alpha = after_stat(l)),
    L = 1.75*r,
    arrow = arrow(type = "closed", length = unit(.015, "npc"))
  ) +
  scale_color_viridis_c(option = "A", direction = 1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "gray20", color = "gray20"),
    legend.position = "none"
  )

sticker(
  p,
  package = "ggvfields",
  p_family = "Comic-Sans",
  filename = outfile,
  s_x = 1, s_y = 1,
  s_width = 2, s_height = 2,
  p_x = 1, p_y = 1,
  h_fill = "gray20", h_color = "gray10",
  p_color = "springgreen1", p_size = 25,
  white_around_sticker = TRUE
)

system(glue::glue("open {outfile}"))

# better font?



# vecs --------------------------------------------------------------------


df_letters <- tibble::tribble(
  ~letter, ~stroke, ~xstart, ~ystart, ~xend, ~yend,

  # G1 (offset = 0)
  "G1", 1, 0,   1,   1,   1,    # Top horizontal
  "G1", 2, 0,   1,   0,   0,    # Left vertical
  "G1", 3, 0,   0,   1,   0,    # Bottom horizontal
  "G1", 4, 1,   1,   1,   0.75,  # Right vertical (top)
  "G1", 5, 1,   0.5, 0.5, 0.5,  # Middle horizontal
  "G1", 5, 1,   0,   1, 0.5,  # bottm to half up

  # G2 (offset = 2)
  "G2", 1, 0+2,   1,   1+2,   1,      # Top horizontal: (2,1) to (3,1)
  "G2", 2, 0+2,   1,   0+2,   0,      # Left vertical: (2,1) to (2,0)
  "G2", 3, 0+2,   0,   1+2,   0,      # Bottom horizontal: (2,0) to (3,0)
  "G2", 4, 1+2,   1,   1+2,   0.75,   # Right vertical (top): (3,1) to (3,0.75)
  "G2", 5, 1+2,   0.5, 0.5+2, 0.5,    # Middle horizontal: (3,0.5) to (2.5,0.5)
  "G2", 6, 1+2,   0,   1+2,   0.5,    # Bottom to half up: (3,0) to (3,0.5)

  # V (offset = 4)
  "V", 1, 4.5, 0, 4, 1,         # Left diagonal
  "V", 2, 4.5, 0, 5, 1,          # Right diagonal

  # F (offset = 6)
  "F", 1, 6, 1, 6, 0,           # Vertical left
  "F", 2, 6, 1, 7, 1,           # Top horizontal
  "F", 3, 6, 0.5, 6.7, 0.5,      # Middle horizontal

  # I (offset = 8)
  "I", 1, 8, 1, 9, 1,           # Top horizontal
  "I", 2, 8.5, 1, 8.5, 0,        # Middle vertical
  "I", 3, 8, 0, 9, 0,           # Bottom horizontal

  # E (offset = 10)
  "E", 1, 10, 1, 10, 0,         # Vertical left
  "E", 2, 10, 1, 11, 1,         # Top horizontal
  "E", 3, 10, 0.5, 10.5, 0.5,    # Middle horizontal
  "E", 4, 10, 0, 11, 0,         # Bottom horizontal

  # L (offset = 12)
  "L", 1, 12, 1, 12, 0,         # Vertical left
  "L", 2, 12, 0, 13, 0,         # Bottom horizontal

  # D (offset = 14)
  "D", 1, 14,   1,   15,   .80,
  "D", 2, 15,   .80,   15,   .20,
  "D", 3, 15,   .20,   14,   0,
  "D", 4, 14,   0,   14,   1,

  # S (offset = 16) – re‑designed as a block S
  "S", 1, 16, 1, 17, 1,         # Top horizontal
  "S", 2, 16, 1, 16, 0.5,        # Left vertical (upper)
  "S", 3, 16, 0.5, 17, 0.5,      # Middle horizontal
  "S", 4, 17, 0.5, 17, 0,        # Right vertical (lower)
  "S", 5, 17, 0, 16, 0          # Bottom horizontal
)

df_letters |>
  ggplot(aes(x = xstart, y = ystart, xend = xend, yend = yend)) +
  geom_vector(normalize = FALSE, center = FALSE, colour = "black")


rescale_to <- function(v, xlim, ylim) {
  # v is input vector in x, y, xend, yend format
  x <- v[1]; y <- v[2]; xend <- v[3]; yend <- v[4]
  c(
    (x - xlim[1]) / diff(xlim),
    (y - ylim[1]) / diff(ylim),
    (xend - xlim[2]) / diff(xlim),
    (yend - ylim[2]) / diff(ylim)
  )
}


df_letters$xstart |> range()
df_letters$xend |> range()
df_letters$ystart |> range()
df_letters$yend |> range()


rescale <- function(u) (u - min(u)) / diff(range(u))

xtrans <- function(x, xlim = c(-1,1)) diff(xlim)*x + xlim[1]
ytrans <- function(y, ylim = c(-.15,.15)) diff(ylim)*y + ylim[1]

(p <- ggplot() +
    geom_stream_field(
      fun = f,
      grid = grid,
      mapping = aes(alpha = after_stat(l)),
      L = 1.75*r,
      arrow = arrow(type = "closed", length = unit(.015, "npc"))
    ) +
    scale_color_viridis_c(option = "A", direction = 1) +
    geom_vector(
      aes(
        x = xstart |> rescale() |> xtrans(),    y = ystart |> rescale() |> ytrans(),
        xend = xend   |> rescale() |> xtrans(), yend = yend   |> rescale() |> ytrans()
      ), data = df_letters, size = .65,
      normalize = FALSE, center = FALSE, colour = "springgreen1",
      arrow = arrow(type = "closed", length = unit(.0075, "npc"))
    ) +
    theme_void() +
    theme(
      # plot.background = element_rect(fill = "gray20", color = "gray20"),
      legend.position = "none"
    ))

sticker(
  p,
  # package = "ggvfields",
  package = "",
  filename = outfile,
  s_x = 1, s_y = 1,
  s_width = 2, s_height = 2,
  p_x = 1, p_y = 1,
  h_fill = "gray20", h_color = "gray10",
  p_color = "springgreen1", p_size = 25,
  # white_around_sticker = TRUE
)

system(glue::glue("open {outfile}"))
