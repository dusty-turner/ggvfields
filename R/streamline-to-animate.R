#' Transition a streamplot to an animation ready object
#'
#' [animation_transition()] takes a streamline plot and alters it so that it
#' will work with gganimate syntax.
#'
#' @param plot A ggplot object from which the data and other plot attributes
#'   will be extracted to create the animation transition.
#'
#' @return A ggplot object with an animation transition effect applied.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(ggvfields)
#' library(gganimate)
#'
#' # Example user-defined function
#' f <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(-1 - x^2 + y, 1 + x - y^2)
#' }
#'
#' # Create a ggplot with the stream plot layer
#' p <- ggplot() +
#'   geom_streamplot(
#'     aes(rownum = after_stat(rownum)),
#'     fun = f, xlim = c(-3, 3), ylim = c(-3, 3)
#'   ) +
#'   coord_fixed() +
#'   theme_bw()
#'
#' # Create an animation transition plot
#' anim <- animation_transition(plot = p) +
#'   transition_reveal(rownum) +
#'   ease_aes('linear')
#'
#' # Animate the plot
#' anim <- animate(anim, nframes = 25, fps = 20, end_pause = 0, renderer = gifski_renderer())
#' }
#'
animation_transition <- function(plot) {

  out <-
    ggplot_build(plot)$data[[1]] |>
    ggplot(aes(x = x, y = y, group = id, alpha = rownum)) +
    geom_path() +
    # geom_path(arrow = plot$layers[[1]]$geom_params$arrow) +
    scale_alpha(guide = "none") +
    # plot$layers +
    plot$theme +
    plot$coordinates +
    plot$facet
  # plot$labels
  return(out)
}

utils::globalVariables(c("id", "rownum"))
