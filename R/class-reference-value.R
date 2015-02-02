
#' @include virtual-class-api-resource.R
#' @include class-tvp.R
NULL

#' ReferenceValue
#'
#' Represents a reference value of a \linkS4class{Timeseries}.
#'
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @export
#' @rdname ReferenceValue-class
#' @name ReferenceValue-class
setClass("ReferenceValue",
         contains = c("TVP", "ApiResource"))

#' @export
#' @describeIn ReferenceValue-class Checks whether \code{x} is a \code{ReferenceValue}.
is.ReferenceValue <- function(x) is(x, "ReferenceValue")
#' @export
#' @describeIn ReferenceValue-class Coerces \code{x} into a \code{ReferenceValue}.
as.ReferenceValue <- function(x) as(x, "ReferenceValue")

setClassUnion("ReferenceValue_or_NULL",
              c("ReferenceValue", "NULL"))

#' @export
#' @describeIn ReferenceValue-class Constructs a new \code{ReferenceValue}
ReferenceValue <- function(id = character(),label = NULL,
                           endpoint = NULL, time = NULL, value = NULL) {
    id <- as.character(id)
    len <- length(id)
    label <- stretch(len, label, NA, as.character)
    endpoint <- stretch(len, endpoint, as.character(NA), as.Endpoint)
    time <- stretch(len, time, NA, as.POSIXct)
    value <- stretch(len, value, NA, as.numeric)
    new("ReferenceValue", endpoint = endpoint, id = id,
        label = label, time = time, value = value)
}

setAs("character", "ReferenceValue",
      function(from) ReferenceValue(id = from))
setAs("list", "ReferenceValue",
      function(from) concat.list(from))
rbind2.ReferenceValue <- function(x, y) {
    x <- as.ReferenceValue(x)
    y <- as.ReferenceValue(y)
    ReferenceValue(endpoint = rbind2(endpoint(x), endpoint(y)),
                   id = c(id(x), id(y)),
                   label = c(label(x), label(y)),
                   time = c(time(x), time(y)),
                   value = c(value(x), value(y)))
}

#' @rdname rbind2-methods
setMethod("rbind2",
          signature("ReferenceValue", "ReferenceValue"),
          function(x, y)
              rbind2.ReferenceValue(x, y))

#' @rdname rbind2-methods
setMethod("rbind2", signature("ReferenceValue", "ANY"),
          function(x, y)
              rbind2.ReferenceValue(x, as.ReferenceValue(y)))

#' @rdname rbind2-methods
setMethod("rbind2",
          signature("ANY", "ReferenceValue"),
          function(x, y)
              rbind2.ReferenceValue(as.ReferenceValue(x), y))

#' @rdname rep-methods
setMethod("rep",
          signature(x = "ReferenceValue"),
          function(x, ...)
              ReferenceValue(endpoint = rep(endpoint(x), ...),
                             id = rep(id(x), ...),
                             label = rep(label(x), ...),
                             time = rep(time(x), ...),
                             value = rep(value(x), ...)))


