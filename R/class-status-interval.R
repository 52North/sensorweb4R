#' @include generic-methods.R
NULL

#' StatusInterval
#'
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @export
#' @rdname StatusInterval-class
#' @name StatusInterval-class
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
#' @describeIn StatusInterval-class Checks whether \code{x} is a \code{StatusInterval}.
is.StatusInterval <- function(x) is(x, "StatusInterval")

#' @export
#' @describeIn StatusInterval-class Coerces \code{x} into a \code{StatusInterval}.
as.StatusInterval <- function(x) as(x, "StatusInterval")

setClassUnion("StatusInterval_or_NULL",
              c("StatusInterval", "NULL"))

#' @export
#' @describeIn StatusInterval-class Constructs a new \code{StatusInterval}.
StatusInterval <- function(name = character(), lower = NULL,
                           upper = NULL, color = NULL) {
    name <- as.character(name)
    len <- length(name)
    lower <- stretch(len, lower, NA, as.numeric)
    upper <- stretch(len, upper, NA, as.numeric)
    color <- stretch(len, color, NA, as.character)
    return(new("StatusInterval", lower = lower, upper = upper,
               color = color, name = name))
}

#' @rdname accessor-methods
setMethod("lower",
          signature(x = "StatusInterval"),
          function(x) x@lower)

#' @rdname accessor-methods
setMethod("lower<-",
          signature(x = "StatusInterval",
                    value = "numeric_or_NULL"),
          function(x, value)
              x@lower <- stretch(length(x), lower, NA, as.numeric))

#' @rdname accessor-methods
setMethod("upper",
          signature(x = "StatusInterval"),
          function(x) x@upper)

#' @rdname accessor-methods
setMethod("upper<-",
          signature(x = "StatusInterval",
                    value = "numeric_or_NULL"),
          function(x, value)
              x@upper <- stretch(length(x), upper, NA, as.numeric))

#' @rdname accessor-methods
setMethod("color",
          signature(x = "StatusInterval"),
          function(x) x@color)

#' @rdname accessor-methods
setMethod("color<-",
          signature(x = "StatusInterval",
                    value = "character_or_NULL"),
          function(x, value)
              x@color <- stretch(length(x), color, NA, as.character))

#' @rdname accessor-methods
setMethod("name",
          signature(x = "StatusInterval"),
          function(x) x@name)

#' @rdname length-methods
setMethod("length",
          signature(x = "StatusInterval"),
          function(x) length(name(x)))

rbind2.StatusInterval <- function(x, y)  {
    x <- as.StatusInterval(x)
    y <- as.StatusInterval(y)
    StatusInterval(lower = c(lower(x), lower(y)),
                   upper = c(upper(x), upper(y)),
                   color = c(color(x), color(y)),
                   name = c(name(x), name(y)))
}

setAs("list", "StatusInterval", function(from) concat.list(from))

#' @rdname rbind2-methods
setMethod("rbind2", signature("StatusInterval", "StatusInterval"),
          function(x, y) rbind2.StatusInterval(x, y))

#' @rdname rbind2-methods
setMethod("rbind2", signature("StatusInterval", "ANY"),
          function(x, y) rbind2.StatusInterval(x, as.StatusInterval(y)))

#' @rdname rbind2-methods
setMethod("rbind2", signature("ANY", "StatusInterval"),
          function(x, y) rbind2.StatusInterval(as.StatusInterval(x), y))

#' @rdname rep-methods
setMethod("rep", signature(x = "StatusInterval"),
          function(x, ...)
              StatusInterval(lower = rep(lower(x), ...),
                             upper = rep(upper(x), ...),
                             color = rep(color(x), ...),
                             name = rep(name(x), ...)))
