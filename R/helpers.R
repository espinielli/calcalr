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


# def summa(f, k, p):
#   """Return the sum of f(i) from i=k, k+1, ... till p(i) holds true or 0.
#     This is a tail recursive implementation."""
# return 0 if not p(k) else f(k) + summa(f, k + 1, p)

#' Return the sum of f(i) from i=k, k+1, ... till p(i) holds true or 0
#'
#' This is a tail recursive implementation.
#'
#' @param f A function (named, anonymous or formula).
#' @param k an integer index.
#' @param p A function (named, anonymous or formula).
#'
#' @return a number
#' @export
#'
#' @examples
#' \dontrun{
#' # sum of squares integers less than or equal to 4
#' summa(\(x) x**2, 1, \(i) i<=4)
#' }
summa <- function(f, k, p) {
  if (!p(k)) {
    return(0)
  }

  f(k) + summa(f, k + 1, p)
}
