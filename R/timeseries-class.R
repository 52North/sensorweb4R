#' @include resource-class.R
#' @include phenomenon-class.R
#' @include service-class.R
#' @include feature-class.R
#' @include offering-class.R
#' @include procedure-class.R
#' @include station-class.R
NULL

#' @export
setClass("StatusInterval",
         slots = list(lower = "numeric",
                      upper = "numeric",
                      color = "character",
                      name = "character"))
#' @export
StatusInterval <- function(lower = NA, upper = NA,
                           color = character(), name = character()) {
  return(new("StatusInterval", lower = lower, upper = upper,
             color = color, name = name))
}

#' @export
setClass("Value",
         slots = list(timestamp = "numeric",
                      value = "numeric"))

#' @export
Value <- function(timestamp=NA, value=NA) {
  return(new("Value", timestamp = timestamp, value = value))
}


#' @export
setClass("Timeseries",
         contains = "Resource",
         slots = list(phenomenon = "Phenomenon",
                      service = "Service",
                      feature = "Feature",
                      offering = "Offering",
                      procedure = "Procedure",
                      station = "Station",
                      uom = "character",
                      firstValue = "Value",
                      lastValue = "Value",
                      statusIntervals = "list"))

#' @export
Timeseries <- function(endpoint, id, label = character(), uom = character(),
                       phenomenon, service, feature, offering, procedure,
                       station, firstValue = Value(),
                       lastValue = Value()) {
  return(new("Timeseries", endpoint = endpoint, id = id, label = label,
             uom = uom, phenomenon = phenomenon, service = service,
             feature = feature, offering = offering, procedure = procedure,
             station = station, firstValue = firstValue, lastValue = lastValue))
}

setClassUnion("Timeseries_or_characters", c("Timeseries", "character"))
