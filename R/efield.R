#' Electric field
#'
#' The vector field generated by collection of fixed electrical charges, as
#' dictated by Coulomb's law. This function is mainly used to provide examples
#' for visualizing vector fields with ggvfields.
#'
#' @param u The position of the test charge.
#' @param charge_positions The positions of the fixed charges generating the
#'   electric field. Defaulted in [efield_maker()].
#' @param charges The charges of the points placed at the positions of
#'   charge_positions. Defaulted in [efield_maker()].
#' @param k The constant of proportionality, defaulted to 1. See examples for a
#'   more rigorous use of physical constants.
#' @param q_test The test charge, defaulted to +1.
#' @param log Executes the transformation of the double log of the norm of the
#'   vectors.
#'
#' @return A vector containing the force felt by the test charge on account of
#'   the electric field.
#' @seealso \url{https://en.wikipedia.org/wiki/Coulomb%27s_law}
#' @name efield
#'
#' @examples
#' \dontrun{
#' # set a - charge at (-1,-1) and a + charge at (1,1)
#' charge_positions <- rbind(c(-1,-1), c(1,1))
#' charges <- c(-1, +1)
#'
#'
#' # calculate force on test charge (+1) at c(0,1), ignoring physical constants
#' efield(c(0,1), charge_positions, charges)
#'
#'
#' # efield_maker() simply wraps this function, defaulting to those charges
#' f <- efield_maker()
#' f(c(0,1))
#'
#' ggplot() +
#'   geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2)) +
#'   scale_color_viridis_c(trans = "log10")
#'
#' # electric constant from https://en.wikipedia.org/wiki/Vacuum_permittivity
#' ep0 <- 8.854187818814e-12
#' k <- (4*pi*ep0)^-1
#' efield(c(0,1), charge_positions, charges, k)
#' }


#' @rdname efield
#' @export efield
efield <- function(u, charge_positions, charges, k = 1, q_test = +1, log = FALSE) {

  F <- c(0,0)
  for (i in 1:nrow(charge_positions)) {
    u_charge <- charge_positions[i,]
    q_charge <- charges[i]
    dir <- normalize(u-u_charge)
    F <- F + k * q_test * q_charge / norm(u-u_charge)^2 * dir
  }

  if(log){
    r <- norm(F)
    theta <- atan2(F[2],F[1])
    R <- log(r + 1)
    # R <- log(log(log(r + 1) + 1))
    F <- c(R * cos(theta), R * sin(theta))
  }
  F
}



#' @rdname efield
#' @export efield_maker
efield_maker <- function(
  charge_positions = rbind(c(-1,-1), c(1,1)),
  charges = c(-1, +1),
  k = 1,
  q_test = +1,
  log = FALSE
) {
  function(u) efield(u, charge_positions, charges, k = 1, q_test = +1, log = log)
}
