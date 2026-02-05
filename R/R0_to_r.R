#' Simple conversion of R0 to r
#'
#' Given that r = 1/ phi ($R_0$ - 1) then we can solve for phi
#' given some value of $R_0$ by phi = (R_0 -1) / r.
#'
#' @param r a number representing the instrinic growth rate
#' @param r_0 a number, the target basic reproduction number
#' @examples
#' r_to_gamma(1, 2)
#' r_to_gamma(1, 4)
#'
#' # Estimate COVID-19
#' r_to_gamma(c(.22, 0.2, 0.24), 2.5)
#' 
#' # H1N1 2009
#' r_to_gamma(0.37, 2.3)
#' 
#' @export
#'

r_to_gamma <- function(r, r_0 = 2.73) {
  infectious_duration <- (r_0 - 1) / r
  return(infectious_duration)
}
