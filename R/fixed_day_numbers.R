#' Epoch definition.
#'
#' For Rata Diem, R.D., it is 0 (but any other reference would do.)
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


#' Return rata diem (number of days since epoch) of moment in time, tee.
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
