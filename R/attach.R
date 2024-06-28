.onAttach <- function(...) {
  if(!interactive() || stats::runif(1) > 0.1) return()
  packageStartupMessage('  Please cite ggvfields! See citation("ggvfields") for details.')
}
