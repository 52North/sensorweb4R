#' @include generic-methods.R
NULL

#' HTTP Resource
#'
#' Virtual class representing a HTTP resource.
#'
#' @export
#' @rdname HttpResource-class
#' @name HttpResource-class
setClass("HttpResource", contains = c("VIRTUAL"))

#' @export
#' @describeIn HttpResource-class Checks whether \code{x} is a \code{HttpResource}.
is.HttpResource <- function(x) is(x, "HttpResource")

#' @export
#' @describeIn HttpResource-class Coerces \code{x} into a \code{HttpResource}.
as.HttpResource <- function(x) as(x, "HttpResource")

#' @rdname url-methods
setMethod("subresourceURL",
          signature(x = "HttpResource"),
          function(x, ...) {
              paste(resourceURL(x), ..., sep = "/")
          })

setMethod("Compare",
          signature("HttpResource", "HttpResource") ,
          function(e1, e2) callGeneric(resourceURL(e1), resourceURL(e2)))
