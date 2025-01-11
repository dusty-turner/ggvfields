fun <- function(v) {
x <- v[1]
y <- v[2]
c(sin(x) + y, x - sin(y))
}
# Define domain limits
xlim <- c(-pi, pi)
ylim <- c(-pi, pi)
# Create the potential function heatmap
ggplot() +
 geom_potential(fun = fun, xlim = xlim, ylim = ylim)
