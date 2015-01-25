#' @include generic-methods.R
NULL

#' @export
setClass("TVP",
         slots = list(time = "POSIXct",
                      value = "numeric"),
         validity = function(object) {
             errors <- assert.same.length(time = object@time,
                                          value = object@value)
             if (length(errors) == 0) TRUE else errors
         })

#' @export
is.TVP <- function(x) is(x, "TVP")
#' @export
as.TVP <- function(x) as(x, "TVP")
#' @export
length.TVP <- function(x) length(time(x))

setClassUnion("TVP_or_NULL", c("TVP", "NULL"))

#' @export
TVP <- function(time = character(), value = numeric()) {
    len <- max(length(time), length(value))
    time <- rep(as.POSIXct(as.character(time)), length.out = len)
    value <- rep(as.numeric(value), length.out = len)
    return(new("TVP", time = time, value = value))
}

setMethod("value", signature(x = "TVP"), function(x) x@value)
setMethod("time", signature(x = "TVP"), function(x) x@time)
setMethod("length", signature("TVP"), length.TVP)

rbind2.TVP <- function(x, y) {
    x <- as.TVP(x)
    y <- as.TVP(y)
    TVP(time = c(time(x), time(y)),
        value = c(value(x), value(y)))
}
setMethod("rbind2", signature("TVP", "TVP"), function(x, y) rbind2.TVP(x, y))
setMethod("rbind2", signature("TVP", "ANY"), function(x, y) rbind2.TVP(x, as.TVP(y)))
setMethod("rbind2", signature("ANY", "TVP"), function(x, y) rbind2.TVP(as.TVP(x), y))
setMethod("rbind2", signature("ANY", "ANY"), function(x, y) rbind2.TVP(as.TVP(x), as.TVP(y)))
setMethod("rep", signature(x = "TVP"), function(x, ...)
    TVP(time = rep(time(x), ...),
        value = rep(value(x), ...)))

#' @export
as.numeric.TVP <- function(x, ...) value(x)
setAs("TVP", "numeric", function(from) value(from))

#' @export
as.POSIXct.TVP <- function(x, ...) time(x)
setAs("TVP", "POSIXct", function(from) time(from))
