#' Adjusted remainder function
#'
#' Return the same as a % b with b instead of 0.
#'
#' @param x an integer
#' @param y an integer
#'
#' @return an integer
#' @export
#'
#' @examples
#' \dontrun{
#' amod(8, 4)
#' }
amod <- function(x, y) {
  y + ((x %% -y))
}

#' Interval modulus
#'
#' An extension of the standard modulus which takes an interval as the modulus, rather than a divisor.
#' It shifts a real-valued `x` into the half-open real interval `[a..b)`  by adding a multiple of
#' the length  `b - a`.
#'
#' @param x a real value
#' @param a the lower end of the interval
#' @param b the upper end of the interval
#'
#' @return an integer
#' @export
#'
#' @examples
#' \dontrun{
#' mod3(123, 7, 11)
#' }
mod3 <- function(x, a, b) {
  if (a == b) {
    return(x)
  }

  a + ((x - a) %% (b - a))
}
