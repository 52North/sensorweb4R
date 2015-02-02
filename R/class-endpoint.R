#' @include generic-methods.R
#' @include helper-methods.R
#' @include virtual-class-http-resource.R
NULL

#' @title Endpoint class
#' @description A class representing a Timeseries API endpoint.
#' @slot url The URL.
#' @slot label A human readable name.
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @exportClass Endpoint
#' @rdname Endpoint-class
#' @name Endpoint-class
setClass("Endpoint",
         contains = "HttpResource",
         slots = list(label = "character",
                      url = "character"),
         validity = function(object) {
             errors <- assert.same.length(url = object@url,
                                          label = object@label)
             if (length(errors) == 0) TRUE else errors
         })

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
Endpoint <- function(url = character(), label = NULL, ...) {
    url <- normalize.URL(as.character(url))
    label <- stretch(length(url), label, NA, as.character)
    new("Endpoint", url = url, label = label)
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

#' @rdname accessor-methods
setMethod("label",
          signature(x = "Endpoint"),
          function(x) x@label)

#' @rdname accessor-methods
setMethod("label<-",
          signature(x = "Endpoint",
                    value = "character_or_NULL"),

          function(x, value) {
              x@label <- stretch(length(x), value, as.character(NA), as.character)
              invisible(x)
          })

#' @rdname accessor-methods
setMethod("names",
          signature(x = "Endpoint"),
          function(x) sensorweb4R::label(x))

#' @rdname accessor-methods
setMethod("names<-",
          signature(x = "Endpoint",
                    value = "character_or_NULL"),
          function(x, value) {
              sensorweb4R::label(x) <- value
              invisible(x)
          })

setAs("character", "Endpoint",
      function(from) Endpoint(url = from))

setAs("list", "Endpoint",
      function(from) concat.list(from))

rbind2.Endpoint <- function(x, y) {
    x <- as.Endpoint(x)
    y <- as.Endpoint(y)
    Endpoint(url = c(resourceURL(x), resourceURL(y)),
             label = c(label(x), label(y)))
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
          function(x, ...) Endpoint(url = rep(resourceURL(x), ...),
                                    label = rep(label(x), ...)))

#' @export
random.Timeseries <- function(e) {
    random <- function(x, n = 1) x[sample(seq_len(length(x)), n)]
    srv <- random(services(e))
    sta <- random(stations(e, service = srv))
    ts <- random(timeseries(e, station = sta))
    fetch(ts)
}


#' Example API endpoints.
#'
#' \code{sensorweb_api_endpoints} returns an instance of \linkS4class{Endpoint}
#' that can be used for testing.
#'
#' @return R object with the further endpoints offered by the service
#' @author Daniel Nuest \email{d.nuest@@52north.org}
#' @author Christian Autermann \email{c.autermann@@52north.org}
#'
#' @export
#'
#' @examples
#' example.endpoints()
#' services(example.endpoints()[1])
example.endpoints <- function() {
    Endpoint(url = c("http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/",
                     "http://sosrest.irceline.be/api/v1/",
                     "http://www.fluggs.de/sos2/api/v1/",
                     "http://sensors.geonovum.nl/sos/api/v1/"),
             label = c("52N Demo", "IRCEL-CELINE", "WV", "Geonovum"))
}
