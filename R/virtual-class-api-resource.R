#' @include generic-methods.R
#' @include helper-methods.R
#' @include virtual-class-http-resource.R
#' @include class-endpoint.R
#' @include class-unions.R
NULL


#' API Resource
#'
#' A virtual class representing a Timeseries API resource.
#'
#' @slot id The identifier.
#' @slot label The human-readable name.
#' @slot endpoint The associated \linkS4class{Endpoint}.
#'
#' @family API Resources
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @exportClass ApiResource
#' @rdname ApiResource-class
#' @name ApiResource-class
setClass("ApiResource",
         contains = c("VIRTUAL",
                      "HttpResource"),
         slots = list(id = "character",
                      label = "character",
                      endpoint = "Endpoint"),
         validity = function(object) {
             errors <- assert.same.length(id = object@id,
                                          label = object@label,
                                          endpoint = object@endpoint)
             if (length(errors) == 0) TRUE else errors
         })

#' @export
#' @describeIn ApiResource-class Checks whether \code{x} is an \code{ApiResource}.
is.ApiResource <- function(x) is(x, "ApiResource")

#' @export
#' @describeIn ApiResource-class Coerces \code{x} to an \code{ApiResource}.
as.ApiResource <- function(x) as(x, "ApiResource")

#' @rdname length-methods
setMethod("length",
          signature("ApiResource"),
          function(x) length(id(x)))

#' @export
#' @describeIn ApiResource-class Corces the \code{ApiResource} \code{x} into a \code{list}.
as.list.ApiResource <- function(x, ...)
    lapply(seq_len(length(x)), function(i) x[i])

setMethod("as.list",
          signature(x = "ApiResource"),
          as.list.ApiResource)

#' @export
#' @describeIn ApiResource-class Returns the unique \code{ApiResource}s in \code{x}.
unique.ApiResource <- function(x, ...)
    x[sapply(unique(id(x)), function(id) which.max(id(x) == id))]

setMethod("unique",
          signature(x = "ApiResource"),
          unique.ApiResource)

#' @rdname accessor-methods
setMethod("id",
          signature(x = "ApiResource"),
          function(x) x@id)

#' @rdname accessor-methods
setMethod("label",
          signature(x = "ApiResource"),
          function(x) x@label)

#' @rdname accessor-methods
setMethod("label<-",
          signature(x = "ApiResource",
                    value = "character_or_NULL"),
          function(x, value) {
              x@label <- stretch(length(x), value, as.character(NA), as.character)
              invisible(x)
          })

#' @rdname accessor-methods
setMethod("names",
          signature(x = "ApiResource"),
          function(x) sensorweb4R::label(x))

#' @rdname accessor-methods
setMethod("names<-",
          signature(x = "ApiResource",
                    value = "character_or_NULL"),
          function(x, value) {
              sensorweb4R::label(x) <- value
              invisible(x)
          })
#' @rdname accessor-methods
setMethod("endpoint",
          signature(x = "ApiResource"),
          function(x) x@endpoint)

#' @rdname accessor-methods
setMethod("endpoint<-",
          signature(x = "ApiResource",
                    value = "Endpoint_or_NULL"),
          function(x, value) {
              x@endpoint <- stretch(length(x), value, as.character(NA), as.Endpoint)
              invisible(x)
          })

setMethod("Compare",
          signature("ApiResource", "ApiResource") ,
          function(e1, e2) callGeneric(id(e1), id(e2)))

collection.name <- function(x) {
    switch(class(x),
           Service = "services",
           Station = "stations",
           Timeseries = "timeseries",
           Category = "categories",
           Offering = "offerings",
           Feature = "features",
           Procedure = "procedures",
           Phenomenon = "phenomena")
}

class.name <- function(x) {
    switch(x,
           services = Service,
           stations = Station,
           timeseries = Timeseries,
           categories = Category,
           offerings = Offering,
           features = Feature,
           procedures = Procedure,
           phenomena = Phenomenon)
}

#' @export
#' @describeIn ApiResource-class Parses the given URIs into their respective \code{ApiResource}.
fromURI <- function(uri) {
    regex <- function(pattern, text) {
        result <- regexec(pattern, text)
        length <- sapply(result, attr, "match.length")
        result <- simplify2array(result)
        function(x) {
            begin <- result[x,]
            end <- begin + length[x,] - 1
            substr(text, begin, end)
        }
    }
    pattern <- "(^.*/v1/)([^/]*)/([^/?]*)"
    result <- regex(pattern, as.character(uri))
    ept <- result(2)
    type <- result(3)
    id <- result(4)
    type.unique <- unique(type)
    o <- lapply(type.unique, function(x) {
        f <- class.name(x)
        fetch(unique(f(id = id[x == type],
                endpoint = Endpoint(ept[x == type]))))
    })
    names(o) <- type.unique
    o
}

#' @rdname url-methods
setMethod("resourceURL",
          signature(x = "ApiResource"),
          function(x) {
              if (length(x) == 0) return(character())
              ifelse(is.na(x), NA,
                     paste(resourceURL(endpoint(x)),
                           collection.name(x),
                           id(x),
                           sep = "/"))
          })

setMethod("is.na",
          signature(x = "ApiResource"),
          function(x) is.na(id(x)))
