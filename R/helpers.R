#' Return the whole part of m/n towards negative infinity
#'
#' @param m The numerator.
#' @param n The denominator.
#'
#' @return An Integer.
#' @export
#'
#' @examples
#' \dontrun{
#' quotient(4, 3)
#' }
quotient <- function(m, n) {
  return(floor(m / n))
}


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

#' Return first integer greater or equal to initial index, i, such that condition, p, holds
#'
#' @param i initial index
#' @param p A function (named, anonymous or formula).
#'
#' @return an integer
#' @export
#'
#' @examples
#' \dontrun{
#' nexti(0, \(i) i == 3)
#' }
nexti <- function(i, p) {
  if (p(i) == TRUE) return(i)
  nexti(i + 1, p)
}


#' Return last integer greater or equal to initial index, i, such that condition, p, holds
#'
#' @inheritParams nexti
#'
#' @return an integer
#' @export
#'
#' @examples
#' \dontrun{
#' final(0, \(i) i < 3)
#' }
final <- function(i, p) {
  if (!p(i) == TRUE) return(i - 1)
  final(i + 1, p)
}


#' Bisection search
#'
#' Bisection search for `x` in `[lo, hi]` such that condition `e` holds,
#' `p` determines when to go left.
#'
#' @param lo Lower end of the search interval.
#' @param hi Higher end of the search interval.
#' @param p A function (named, anonymous or formula).
#' @param e A function (named, anonymous or formula).
#'
#' @return A value inside the `[lo, hi]` interval.
#' @export
#'
binary_search <- function(lo, hi, p, e) {
  x = (lo + hi) / 2.0
  if (p(lo, hi)) return(x)
  if (e(x)) return(binary_search(lo, x, p, e))
  return(binary_search(x, hi, p, e))
}

#' Convert degrees to radians
#'
#' @param degs Degrees.
#'
#' @return A number
#' @export
#'
#' @examples
#' \dontrun{
#' radians(30)
#' }
radians <- function(degs) {
  degs * pi / 180
}

#' Convert  radians to degrees
#'
#' @param rads Radians.
#'
#' @return A number
#' @export
#'
#' @examples
#' \dontrun{
#' degrees(pi)
#' }
degrees <- function(rads) {
  rads * 180 / pi
}

#' Inverse of angular function
#'
#' @param f A function (named, anonymous or formula).
#' @param y A value where to invert `f`.
#' @param a Lower value of the search interval.
#' @param b Upper value of the search interval.
#' @param prec Required precision (default: 0.00001).
#'
#' @return A value.
#' @export
#'
invert_angular <- function(f, y, a, b, prec = 1e-5) {
  binary_search(a, b,
                \(l, h) { (h - l) <= prec },
                \(x) { ((f(x) - y) %% 360) < 180})
}


#' Return the month of date `date`
#'
#' @param date A date.
#'
#' @return The month component of a date.
#' @export
#'
#' @examples
#' \dontrun{
#' standard_month(c(1, 2, 3))
#' }
standard_month <- function(date) {
  return(date[2])
}

#' Return the day of date `date`
#'
#' @param date A date.
#'
#' @return The day component of a date.
#' @export
#'
#' @examples
#' \dontrun{
#' standard_day(c(1, 2, 3))
#' }
standard_day <- function(date) {
  return(date[3])
}

#' Return the year of date `date`
#'
#' @param date A date.
#'
#' @return The year component of a date.
#' @export
#'
#' @examples
#' \dontrun{
#' standard_year(c(1, 2, 3))
#' }
standard_year <- function(date) {
  return(date[1])
}
