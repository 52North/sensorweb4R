#' @include generic-methods.R
NULL

#' @export
is.HttpResource <- function(x) is(x, "HttpResource")
as.HttpResource <- function(x) as(x, "HttpResource")

#' @export
setClass("HttpResource",
         contains = c("VIRTUAL"),
         slots = list())

setMethod("subresourceURL",
          signature(x = "HttpResource"),
          function(x, ...) {
              paste(resourceURL(x), ..., sep = "/")
          })
