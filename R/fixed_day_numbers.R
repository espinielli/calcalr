#' Epoch definition.
#'
#' For Rata Die, R.D., it is 0 (but any other reference would do.)
#'
#' @return a number indicating the start of the calendar
#' @export
#'
#' @examples
#' \dontrun{
#' epoch()
#' }
epoch <- function() {
  return(0L)
}


#' Return the rata diem (number of days since epoch) of a moment in time
#'
#' @param tee a double indicating a moment in time
#'
#' @return number of (eventually partial) days since epoch
#' @export
#'
#' @examples
#' \dontrun{
#' rd(1234.5)
#' }
rd <- function(tee) {
  tee - epoch()
}

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
#'
#' @examples
#' \dontrun{
#' day_of_week_from_fixed(253427)
#' }
day_of_week_from_fixed <- function(date) {
  (date - rd(0) - SUNDAY) %% 7
}

#' Return fixed date from moment
#'
#' @param tee a moment in time
#'
#' @return a Rata Die
#' @export
#'
#' @examples
#' \dontrun{
#' fixed_from_moment(2.65)
#' }
fixed_from_moment <- function(tee) {
  floor(tee)
}

#' Return time from moment 'tee'
#'
#' @inheritParams fixed_from_moment
#'
#' @return factional part of a day
#' @export
#'
#' @examples
#' \dontrun{
#' time_from_moment(2.65)
#' }
time_from_moment <- function(tee){
  tee %% 1
}
