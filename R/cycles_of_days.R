
SUNDAY    <- 0
MONDAY    <- 1
TUESDAY   <- 2
WEDNESDAY <- 3
THURSDAY  <- 4
FRIDAY    <- 5
SATURDAY  <- 6

name_of_day_of_week <- function(day_of_week) {
  DAYS_OF_WEEK_NAMES <- c(
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday"
  )
  DAYS_OF_WEEK_NAMES[day_of_week + 1]
}

#' Return day of the week from a fixed date
#'
#' @inheritParams kday_on_or_before
#'
#' @return an integer indicating the day of the week
#' @export
#' @family cycles of days
#' @examples
#' \dontrun{
#' day_of_week_from_fixed(253427)
#' }
day_of_week_from_fixed <- function(date) {
  magrittr::mod(date - rd(0) - SUNDAY, 7)
}


#' Return the fixed date of the k-day on or before fixed date 'date'.
#'
#' @param date a fixed date, i.e. a number of (eventually partial) days since epoch
#' @param k the weekday; k=0 means Sunday, k=1 means Monday, and so on.
#'
#' @return a fixed day number
#' @export
#' @family cycles of days
#' @examples
#' \dontrun{
#' kday_on_or_before(1, -61387)
#' }
kday_on_or_before <- function(k, date) {
  date - day_of_week_from_fixed(date - k)
}


#' Return the fixed date of the k-day nearest fixed date 'date'
#'
#' @inheritParams kday_on_or_before
#'
#' @return a fixed day number
#' @export
#' @family cycles of days
#' @examples
#' \dontrun{
#' kday_nearest(1, -61387)
#' }
kday_nearest <- function(k, date) {
  kday_on_or_before(k, date + 3)
}

#' Return the fixed date of the k-day after fixed date
#'
#' @inheritParams kday_on_or_before
#'
#' @return a fixed day number
#' @export
#' @family cycles of days
#' @examples
#' \dontrun{
#' kday_after(1, -61387)
#' }
kday_after <- function(k, date) {
  kday_on_or_before(k, date + 7)
}

# see lines 873-877 in calendrica-3.0.cl
#' Return the fixed date of the k-day before fixed date
#'
#' @inheritParams kday_on_or_before
#'
#' @return a fixed day number
#' @export
#' @family cycles of days
#' @examples
#' \dontrun{
#' kday_before(1, -61387)
#' }
kday_before <- function(k, date) {
  kday_on_or_before(k, date - 1)
}

#  #' Return the fixed date of n-th k-day after Gregorian date
#  #'
#  #'     If n>0, return the n-th k-day on or after  'g_date'.
#  #'     If n<0, return the n-th k-day on or before 'g_date'.
#  #'     If n=0, return BOGUS.
#  #'
#  #' @param n the n-th k-day
#  #'     * on or after `g_date` if n > 0
#  #'     * on or before `g_date` if n < 0
#  #'     * NA otherwise
#  #' @param g_date a Gregorian date
#  #' @inheritParams kday_on_or_before
#  #'
#  #' @return a fixed day number
#  #' @export
#  #'
#  #' @examples
#  nth_kday <- function(n, k, g_date) {
#    if (n > 0) {
#      return(7 * n + kday_before(k, fixed_from_gregorian(g_date)))
#    } else if (n < 0) {
#      return(7 * n + kday_after(k, fixed_from_gregorian(g_date)))
#    }
#
#    return(NA_integer_)
#  }





