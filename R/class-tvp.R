#' @include generic-methods.R
NULL

#' Time-Value-Pairs
#'
#' Class to contains the data of a \linkS4class{Timeseries} as time value
#' pairs.
#'
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @export
#' @rdname TVP-class
#' @name TVP-class
setClass("TVP",
         slots = list(time = "POSIXct",
                      value = "numeric"),
         validity = function(object) {
             errors <- assert.same.length(time = object@time,
                                          value = object@value)
             if (length(errors) == 0) TRUE else errors
         })

#' @export
#' @describeIn TVP-class Checks whether \code{x} is a \code{TVP}.
is.TVP <- function(x) is(x, "TVP")

#' @export
#' @describeIn TVP-class Coerces \code{x} into a \code{TVP}.
as.TVP <- function(x) as(x, "TVP")

setClassUnion("TVP_or_NULL", c("TVP", "NULL"))

#' @export
#' @describeIn TVP-class Constructs a new \code{TVP}.
TVP <- function(time = character(), value = numeric()) {
    len <- max(length(time), length(value))
    if (!"POSIXct" %in% class(time)) {
        time <- as.POSIXct(as.character(time))
    }
    time <- rep(time, length.out = len)
    value <- rep(as.numeric(value), length.out = len)
    return(new("TVP", time = time, value = value))
}

#' @rdname accessor-methods
setMethod("value", signature(x = "TVP"), function(x) x@value)

#' @rdname accessor-methods
setMethod("time", signature(x = "TVP"), function(x) x@time)

#' @rdname length-methods
setMethod("length", signature("TVP"), function(x) length(time(x)))

setAs("list", "TVP", function(from) concat.list(from))

rbind2.TVP <- function(x, y) {
    x <- as.TVP(x)
    y <- as.TVP(y)
    TVP(time = c(time(x), time(y)),
        value = c(value(x), value(y)))
}

#' @rdname rbind2-methods
setMethod("rbind2", signature("TVP", "TVP"),
          function(x, y) rbind2.TVP(x, y))

#' @rdname rbind2-methods
setMethod("rbind2", signature("TVP", "ANY"),
          function(x, y) rbind2.TVP(x, as.TVP(y)))

#' @rdname rbind2-methods
setMethod("rbind2", signature("ANY", "TVP"),
          function(x, y) rbind2.TVP(as.TVP(x), y))

#' @rdname rep-methods
setMethod("rep", signature(x = "TVP"), function(x, ...)
    TVP(time = rep(time(x), ...),
        value = rep(value(x), ...)))

#' @export
as.numeric.TVP <- function(x, ...) value(x)
setAs("TVP", "numeric", function(from) value(from))

#' @export
as.POSIXct.TVP <- function(x, ...) time(x)
setAs("TVP", "POSIXct", function(from) time(from))

setMethod("Compare",
          signature("TVP", "TVP") ,
          function(e1, e2) callGeneric(time(e1), time(e2)))


#' @export
hist.TVP <- function(x, ...) hist(value(x))

setMethod("hist", signature("TVP"),
          function(x, ...) hist.TVP(x))

#' @export
summary.TVP <- function(object, ...)
    summary(as.data.frame(object), ...)

setMethod("summary", signature("TVP"),
          function(object, ...) summary.TVP(object, ...))
