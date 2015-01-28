#' @include generic-methods.R
#' @include helper-methods.R
#' @include virtual-class-http-resource.R
NULL

#' @title Endpoint class
#' @description A class representing a Timeseries API endpoint.
#' @slot url The URL.
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @exportClass Endpoint
#' @rdname Endpoint-class
#' @name Endpoint-class
setClass("Endpoint",
         contains = "HttpResource",
         slots = list(url = "character"))

#' @export
#' @describeIn Endpoint-class Checks wether \code{x} is an \code{Endpoint}.
is.Endpoint <- function(x) is(x, "Endpoint")

#' @export
#' @describeIn Endpoint-class Coerces \code{x} into an \code{Endpoint}.
as.Endpoint <- function(x) as(x, "Endpoint")

#' @export
#' @rdname length-methods
setMethod("length",
          signature("Endpoint"),
          function(x) length(resourceURL(x)))

normalize.URL <- function(x) {
    x <- stringi::stri_replace_last_regex(x, "\\?.*", "")
    x <- stringi::stri_replace_last_regex(x, "#.*", "")
    x <- stringi::stri_trim_right(x, pattern = "[^/]")
    return(x)
}

#' @export
#' @describeIn Endpoint-class Constructs a new \code{Endpoint}.
Endpoint <- function(url = character(), ...) {
    new("Endpoint", url = normalize.URL(as.character(url)), ...)
}

setClassUnion("Endpoint_or_characters", c("Endpoint", "character"))

setClassUnion("Endpoint_or_NULL", c("Endpoint", "NULL"))

#' @rdname url-methods
setMethod("resourceURL",
          signature(x = "Endpoint"),
          function(x) x@url)

#' @rdname url-methods
setMethod("stationsURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "stations"))

#' @rdname url-methods
setMethod("servicesURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "services"))

#' @rdname url-methods
setMethod("timeseriesURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "timeseries"))

#' @rdname url-methods
setMethod("categoriesURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "categories"))

#' @rdname url-methods
setMethod("offeringsURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "offerings"))

#' @rdname url-methods
setMethod("featuresURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "features"))

#' @rdname url-methods
setMethod("proceduresURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "procedures"))

#' @rdname url-methods
setMethod("phenomenaURL",
          signature(x = "Endpoint"),
          function(x) subresourceURL(x, "phenomena"))

setAs("character", "Endpoint",
      function(from) Endpoint(url = from))
setAs("list", "Endpoint",
      function(from) concat.list(from))

rbind2.Endpoint <- function(x, y) {
    Endpoint(url = c(resourceURL(as.Endpoint(x)),
                     resourceURL(as.Endpoint(y))))
}

#' @rdname rbind2-methods
setMethod("rbind2",
          signature("Endpoint", "Endpoint"),
          function(x, y) rbind2.Endpoint(x, y))

#' @rdname rbind2-methods
setMethod("rbind2",
          signature("Endpoint", "ANY"),
          function(x, y) rbind2.Endpoint(x, as.Endpoint(y)))

#' @rdname rbind2-methods
setMethod("rbind2",
          signature("ANY", "Endpoint"),
          function(x, y) rbind2.Endpoint(as.Endpoint(x), y))

#' @rdname rep-methods
setMethod("rep", signature(x = "Endpoint"),
          function(x, ...) Endpoint(url = rep(resourceURL(x), ...)))

#' @export
random.Timeseries <- function(e) {
    random <- function(x, n = 1) x[sample(seq_len(length(x)), n)]
    srv <- random(services(e))
    sta <- random(stations(e, service = srv))
    ts <- random(timeseries(e, station = sta))
    fetch(ts)
}
