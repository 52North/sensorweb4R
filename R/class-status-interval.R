#' @include generic-methods.R
NULL

#' @export
setClass("StatusInterval",
         slots = list(lower = "numeric",
                      upper = "numeric",
                      color = "character",
                      name = "character"),
         validity = function(object) {
             errors <- assert.same.length(name = object@name,
                                          lower = object@lower,
                                          upper = object@upper,
                                          color = object@color)
             if (length(errors) == 0) TRUE else errors
         })

#' @export
is.StatusInterval <- function(x) is(x, "StatusInterval")
#' @export
as.StatusInterval <- function(x) as(x, "StatusInterval")

setClassUnion("StatusInterval_or_NULL",
              c("StatusInterval", "NULL"))

#' @export
StatusInterval <- function(name,
                           lower = rep(as.numeric(NA), length(name)),
                           upper = rep(as.numeric(NA), length(name)),
                           color = rep(as.character(NA), length(name))) {
    return(new("StatusInterval",
               lower = lower,
               upper = upper,
               color = color,
               name = name))
}

#' @export
length.StatusInterval <- function(x) length(name(x))

setMethod("lower",
          signature(x = "StatusInterval"),
          function(x) x@lower)
setMethod("upper",
          signature(x = "StatusInterval"),
          function(x) x@upper)
setMethod("color",
          signature(x = "StatusInterval"),
          function(x) x@color)
setMethod("name",
          signature(x = "StatusInterval"),
          function(x) x@name)
setMethod("length",
          signature(x = "StatusInterval"),
          length.StatusInterval)

rbind2.StatusInterval <- function(x, y)  {
    x <- as.StatusInterval(x)
    y <- as.StatusInterval(y)
    StatusInterval(lower = c(lower(x), lower(y)),
                   upper = c(upper(x), upper(y)),
                   color = c(color(x), color(y)),
                   name = c(name(x), name(y)))
}
setMethod("rbind2", signature("StatusInterval", "StatusInterval"), function(x, y) concat.pair.StatusInterval(x, y))
setMethod("rbind2", signature("StatusInterval", "ANY"), function(x, y) concat.pair.StatusInterval(x, as.StatusInterval(y)))
setMethod("rbind2", signature("ANY", "StatusInterval"), function(x, y) concat.pair.StatusInterval(as.StatusInterval(x), y))
setMethod("rbind2", signature("ANY", "ANY"), function(x, y) concat.pair.StatusInterval(as.StatusInterval(x), as.StatusInterval(y)))
setMethod("rep", signature(x = "StatusInterval"), function(x, ...)
    StatusInterval(lower = rep(lower(x), ...),
                   upper = rep(upper(x), ...),
                   color = rep(color(x), ...),
                   name = rep(name(x), ...)))
