#' Return the Egyptian date data structure
#'
#' @param year The Egyptian calendar year.
#' @param month The Egyptian calendar month.
#' @param day The Egyptian calendar day.
#'
#' @return An Egyptian calendar date.
#' @export
#'
#' @examples
#' \dontrun{
#' egyptian_date(1, 2, 3)
#' }
egyptian_date <- function(year, month, day) {
  c(year, month, day)
}

# EGYPTIAN_EPOCH <- fixed_from_jd(1448638)
EGYPTIAN_EPOCH <- -272787

#' Return the fixed date corresponding to Egyptian date `e_date`
#'
#' @param e_date An Egyptian calendar date.
#'
#' @return A Rata Die.
#' @export
#'
#' @examples
#' \dontrun{
#' fixed_from_egyptian(egyptian_date(1, 2, 3))
#' }
fixed_from_egyptian <- function(e_date) {
  month <- standard_month(e_date)
  day   <- standard_day(e_date)
  year  <- standard_year(e_date)
  return(EGYPTIAN_EPOCH + (365*(year - 1)) + (30*(month - 1)) + (day - 1))
}

#' Return the Egyptian date corresponding to fixed date `date`
#'
#' @param date A Rata Die.
#'
#' @return An Egyptian calendar date.
#' @export
#'
#' @examples
#' \dontrun{
#' egyptian_from_fixed(1234)
#' }
egyptian_from_fixed <- function(date) {
  days  <-  date - EGYPTIAN_EPOCH
  year  <-  1 + quotient(days, 365)
  month <-  1 + quotient((days %% 365), 30)
  day   <-  days - (365 * (year - 1)) - (30 * (month - 1)) + 1
  return(egyptian_date(year, month, day))
}
