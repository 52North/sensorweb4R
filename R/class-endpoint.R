#' @include generic-methods.R
#' @include helper-methods.R
#' @include virtual-class-http-resource.R
NULL

#' @export
setClass("Endpoint",
         contains = "HttpResource",
         slots = list(url = "character"))

#' @export
is.Endpoint <- function(x) is(x, "Endpoint")
#' @export
as.Endpoint <- function(x) as(x, "Endpoint")

#' @export
length.Endpoint <- function(x) length(resourceURL(x))

#' @export
Endpoint <- function(url = character(), ...)
    new("Endpoint", url = .normalizeURL(as.character(url)), ...)

setClassUnion("Endpoint_or_characters",
              c("Endpoint", "character"))

setClassUnion("Endpoint_or_NULL",
              c("Endpoint", "NULL"))

setMethod("resourceURL",
          signature(x = "Endpoint"),
          function(x) x@url)

setMethod("stationsURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "stations"))

setMethod("servicesURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "services"))

setMethod("timeseriesURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "timeseries"))

setMethod("categoriesURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "categories"))

setMethod("offeringsURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "offerings"))

setMethod("featuresURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "features"))

setMethod("proceduresURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "procedures"))

setMethod("phenomenaURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "phenomena"))

setMethod("length",
          signature(x = "Endpoint"),
          length.Endpoint)

setAs("character", "Endpoint", function(from) Endpoint(url = from))


rbind2.Endpoint <- function(x, y) {
    x <- as.Endpoint(x)
    y <- as.Endpoint(y)
    Endpoint(url = c(resourceURL(x), resourceURL(y)))
}
setMethod("rbind2", signature("Endpoint", "Endpoint"), function(x, y) concat.pair.Endpoint(x, y))
setMethod("rbind2", signature("Endpoint", "ANY"), function(x, y) concat.pair.Endpoint(x, as.Endpoint(y)))
setMethod("rbind2", signature("ANY", "Endpoint"), function(x, y) concat.pair.Endpoint(as.Endpoint(x), y))
setMethod("rbind2", signature("ANY", "ANY"), function(x, y) concat.pair.Endpoint(as.Endpoint(x), as.Endpoint(y)))
setMethod("rep", signature(x = "Endpoint"), function(x, ...) Endpoint(url = rep(resourceURL(x), ...)))
