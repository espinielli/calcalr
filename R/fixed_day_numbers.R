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

#' Return the time of day data structure
#'
#' @param hour   A number representing hours.
#' @param minute A number representing minutes.
#' @param second A number representing seconds.
#'
#' @return A representation of time of day, i.e. a vector with 3 numbers.
#' @export
#'
#' @examples
#' \dontrun{
#' time_of_day(10, 25, 13)
#' }
time_of_day <- function(hour, minute, second) {
  c(hour, minute, second)
}

#' Return the hour of clock time `clock`
#'
#' @param clock The clock time.
#'
#' @return The hour.
#' @export
#'
#' @examples
#' \dontrun{
#' hour(3.5)
#' }
hour <- function(clock) {
  return(clock[1])
}

#' Return the minutes of clock time `clock`
#'
#' @inheritParams hour
#'
#' @return The minute
#' @export
#'
#' @examples
#' \dontrun{
#' minute(3.5)
#' }
minute <- function(clock) {
  return(clock[2])
}

#' Return the seconds of clock time `clock`
#'
#' @inheritParams hour
#'
#' @return The seconds
#' @export
#'
#' @examples
#' \dontrun{
#' seconds(3.5)
#' }
seconds <- function(clock) {
  return(clock[3])
}


#' Return clock time hour:minute:second from moment 'tee'
#'
#' @param tee A moment in time.
#'
#' @return The clock time.
#' @export
#'
#' @examples
#' \dontrun{
#' clock_from_moment(10.2345)
#' }
clock_from_moment <- function(tee) {
  time = time_from_moment(tee)
  hour = floor(time * 24)
  minute = floor((time * 24 * 60) %% 60)
  second = (time * 24 * 60 * 60) %% 60

  time_of_day(hour, minute, second)
}

#' Return time from moment `tee`
#'
#' @inheritParams clock_from_moment
#'
#' @return The time component of a moment.
#' @export
#'
#' @examples
#' \dontrun{
#' time_from_moment(4.3)
#' }
time_from_moment <- function(tee) {
  return(tee %% 1)
}

#' Return time of day from clock time `hms`
#'
#' @param hms The time vector.
#'
#' @return The numerical representation of the clock time.
#' @export
#'
#' @examples
#' \dontrun{
#' time_from_clock(c(11, 12, 13))
#' }
time_from_clock <- function(hms) {
  h <- hour(hms)
  m <- minute(hms)
  s <- seconds(hms)
  return(1/24 * (h + ((m + (s / 60)) / 60)))
}
