
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
#' @param date a fixed date, i.e. a number of (eventually partial) days since epoch
#'
#' @return an integer indicating the day of the week
#' @export
#'
#' @examples
#' \dontrun{
#' day_of_week_from_fixed(253427)
#' }
day_of_week_from_fixed <- function(date) {
  magrittr::mod(date - rd(0) - SUNDAY, 7)
}


#' Return the fixed date of the k-day on or before fixed date 'date'.
#'
#' @param k the weekday; k=0 means Sunday, k=1 means Monday, and so on.
#' @inheritParams day_of_week_from_fixed
#'
#' @return a fixed day number
#' @export
#'
#' @examples
#' \dontrun{
#' kday_on_or_before(-61387)
#' }
kday_on_or_before <- function(k, date) {
  date - day_of_week_from_fixed(date - k)
}

