#' Example Dataset on SAFTEr
#'
#' An example dataset containing four subjects with sleep, work, crewing, and test
#' events, start and end times for those events, and test results for plotting in
#' the SAFTEr package. The variables are as follows:
#'
#'
#' @format A data frame with 358 rows and 5 variables:
#' \describe{
#'    \item{ID}{Subject ID of participant (A10, B52, AC130, D130)}
#'    \item{Event}{Event type of observation (Sleep, Work, Crewing, Test)}
#'    \item{Start}{Start datetime of event as a POSIX 'YYYY-mm-dd HH:MM' }
#'    \item{End}{End datetime of event as a POSIX 'YYYY-mm-dd HH:MM' }
#'    \item{Test_Results}{Data points for test events ranging from 60-100}
#'
#' }
#'
#' @keywords datasets
#'
#'
#'
"SAFTEr_xdata"
