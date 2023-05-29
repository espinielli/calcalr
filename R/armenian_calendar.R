#' Return the Armenian date data structure
#'
#' @param year  The Armeniam calendar year.
#' @param month The Armeniam calendar month.
#' @param day   The Armeniam calendar day.
#'
#' @return An Armenian date.
#' @export
#'
#' @examples
#' \dontrun{
#' armenian_date(1, 2, 3)
#' }
armenian_date <- function(year, month, day) {
  return(c(year, month, day))
}

# ARMENIAN_EPOCH <- rd(201443)
ARMENIAN_EPOCH <- 201443

#' Return the fixed date corresponding to Armenian date `a_date`
#'
#' @param a_date An Armenian calendar date.
#'
#' @return A Data Die.
#' @export
#'
#' @examples
#' \dontrun{
#' fixed_from_armenian(c(1, 2, 3))
#' }
fixed_from_armenian <- function(a_date) {
  month <- standard_month(a_date)
  day   <- standard_day(a_date)
  year  <- standard_year(a_date)
  return (ARMENIAN_EPOCH +
            fixed_from_egyptian(egyptian_date(year, month, day)) -
            EGYPTIAN_EPOCH)
}

#' Return the Armenian date corresponding to fixed date `date`
#'
#' @param date A Rata Die.
#'
#' @return An Armenian calendar date.
#' @export
#'
#' @examples
#' \dontrun{
#' armenian_from_fixed(1234)
#' }
armenian_from_fixed <- function(date) {
  return(egyptian_from_fixed(date + (EGYPTIAN_EPOCH - ARMENIAN_EPOCH)))
}
