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

#' @export
length.StatusInterval <- function(x) length(name(x))

setMethod("lower",
          signature(x = "StatusInterval"),
          function(x) x@lower)
setMethod("lower<-",
          signature(x = "StatusInterval",
                    value = "numeric_or_NULL"),
          function(x, value)
              x@lower <- stretch(length(x), lower, NA, as.numeric))

setMethod("upper",
          signature(x = "StatusInterval"),
          function(x) x@upper)
setMethod("upper<-",
          signature(x = "StatusInterval",
                    value = "numeric_or_NULL"),
          function(x, value)
              x@upper <- stretch(length(x), upper, NA, as.numeric))

setMethod("color",
          signature(x = "StatusInterval"),
          function(x) x@color)
setMethod("color<-",
          signature(x = "StatusInterval",
                    value = "character_or_NULL"),
          function(x, value)
              x@color <- stretch(length(x), color, NA, as.character))

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

setAs("list", "StatusInterval", function(from) concat.list(from))

setMethod("rbind2", signature("StatusInterval", "StatusInterval"),
          function(x, y) rbind2.StatusInterval(x, y))
setMethod("rbind2", signature("StatusInterval", "ANY"),
          function(x, y) rbind2.StatusInterval(x, as.StatusInterval(y)))
setMethod("rbind2", signature("ANY", "StatusInterval"),
          function(x, y) rbind2.StatusInterval(as.StatusInterval(x), y))
setMethod("rbind2", signature("ANY", "ANY"),
          function(x, y) rbind2.StatusInterval(as.StatusInterval(x),
                                                    as.StatusInterval(y)))
setMethod("rep", signature(x = "StatusInterval"),
          function(x, ...)
              StatusInterval(lower = rep(lower(x), ...),
                             upper = rep(upper(x), ...),
                             color = rep(color(x), ...),
                             name = rep(name(x), ...)))
