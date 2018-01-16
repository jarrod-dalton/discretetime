#' Map Irregular Data to a Discrete Time Grid via Interpolation
#'
#' @description This function takes irregularly-spaced observations in time and,
#'   through a call to \code{approx}, interpolates the data at a user-specified,
#'   fixed grid of equally-spaced time points. The function is essentially a
#'   wrapper to \code{approx} that allows data frames as input (as opposed to
#'   individual vectors for the x variable and time variable). As such, it
#'   provides a useful framework for grouped data operations such as
#'   \code{plyr::ddply}.
#'
#' @param data A data frame
#' @param xvar Character string indicating the column name in \code{data}
#'   that contains the measurements
#' @param timevar Character string indicating the column name in \code{data}
#'   that contains the (numeric) time values associated with the measurements
#'   in \code{xvar}.
#' @param start Numeric value of the starting time over which the discrete time data
#'   will be generated. Defaults to \code{min(data$xvar)}.
#' @param end Numeric value of the ending time over which the discrete time data
#'   will be generated. Defaults to \code{max(data$xvar)}.
#' @param dt Numeric value representing the time increment of measurements over the
#'   time interval given by \code{start} and \code{end}. Must satisfy
#'   \code{dt <= (end - start)}.
#' @param method Interpolation method passed to \code{approx()}. Defaults to
#'   \code{"constant"}. See \code{help(approx)} for details.
#' @param rule Rule by which observations outside the interval
#'   \code{[min(data$xvar), max(data$xvar)]} are handled. Defaults to \code{rule=1};
#'   see \code{help(approx)} for details.
#' @param ... Other parameters passed to \code{approx()}.
#'
#' @details The function ignores columns in \code{data} other than \code{xvar} and
#'   \code{timevar}.
#'
#' @return A data frame containing equally-spaced, interpolated values of \code{xvar}
#'   over the specified time grid. Other variables in \code{data} are returned (assuming)
#' @export
#'
#' @examples
#' irregularData <- tibble(value=1:10, t=c(1,4,5,7,8,10,11,13,17,21))
#' mapToDiscreteTime(irregularData, end=24, dt=2)
#' mapToDiscreteTime(irregularData, end=24, dt=0.5)
mapToDiscreteTime <- function(data, xvar="value", timevar="t",
                              start, end, dt, method="constant",
                              rule=1, ...){
  stopifnot("data.frame" %in% class(data))
  stopifnot(is.character(xvar))
  stopifnot(is.character(timevar))
  if(length(xvar)>1){
    warning("Parameter 'xvar' has length >1. Using the first element.")
    xvar <- xvar[1]
  }
  if(length(timevar)>1){
    warning("Parameter 'timevar' has length >1. Using the first element.")
    timevar <- timevar[1]
  }
  if(!(xvar %in% names(data))) stop(paste0("Column '", xvar, "' not in names(data)."))
  if(!(timevar %in% names(data))) stop(paste0("Column '", timevar,
                                              "' not in names(data)."))
  x <- dplyr::pull(data, xvar)
  t <- dplyr::pull(data, timevar)
  if(!is.numeric(x)) stop(paste0("Column '",xvar,"' is not numeric."))
  if(!is.numeric(t)) stop(paste0("Column '",timevar,"' is not numeric."))
  if(missing(start)) {start <- min(t)} else{
    stopifnot(is.numeric(start))
    if(length(start)>1){
      warning("Parameter 'start' has length >1. Using the first element.")
      start <- start[1]
    }
  }
  if(missing(end)) {end <- max(t)} else{
    stopifnot(is.numeric(end))
    if(length(end)>1){
      warning("Parameter 'end' has length >1. Using the first element.")
      end <- end[1]
    }
  }
  stopifnot(start <= end)
  stopifnot(is.numeric(dt))
  if(length(dt)>1){
    warning("Parameter 'dt' has length >1. Using the first element.")
    dt <- dt[1]
  }
  stopifnot(dt <= (end-start))
  grd <- approx(t, x, xout=seq(start, end, by=dt),
                method=method, rule=rule, ...)
  return(tibble(!!timevar := grd$x, !!xvar := grd$y))
}

#u <- mapToDiscreteTime(zz, start=0, end=96, dt=4)
