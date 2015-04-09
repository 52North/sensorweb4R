#' @include generic-methods.R
#' @include helper-methods.R
#' @include virtual-class-api-resource.R
#' @include class-unions.R
NULL

#' Service
#'
#' @slot serviceURL the URL of the service
#' @slot version the version of the service
#' @slot type the type of the service
#' @slot supportsFirstLatest wether the service supports first/lastest values
#' @slot quantities a \code{data.frame} containing the quantities of the offered resources
#' @family API Resources
#' @name Service-class
#' @rdname Service-class
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @exportClass Service
setClass("Service",
         contains = "ApiResource",
         slots = list(serviceURL = "character",
                      version = "character",
                      type = "character",
                      supportsFirstLatest = "logical",
                      quantities = "data.frame"),
         validity = function(object) {
             errors <- assert.same.length(id = object@id,
                                          serviceURL = object@serviceURL,
                                          version = object@version,
                                          type = object@type,
                                          supportsFirstLatest = object@supportsFirstLatest,
                                          quantities = object@quantities)
             if (length(errors) == 0) TRUE else errors
         })

#' @export
#' @describeIn Service-class Constructs a new \code{Service} object.
Service <- function(id = character(),
                    label = NULL,
                    serviceURL = NULL,
                    version = NULL,
                    type = NULL,
                    supportsFirstLatest = NULL,
                    quantities = NULL,
                    endpoint = NULL) {
    id <- as.character(id)
    len <- length(id)
    label <- stretch(len, label, as.character(NA), as.character)
    serviceURL <- stretch(len, serviceURL, as.character(NA), as.character)
    version <- stretch(len, version, as.character(NA), as.character)
    type <- stretch(len, type, as.character(NA), as.character)
    supportsFirstLatest <- stretch(len, supportsFirstLatest, as.logical(NA), as.logical)
    endpoint <- stretch(len, endpoint, as.character(NA), as.Endpoint)

    quantities <- create.quantities(len, quantities)

    new("Service",
        endpoint = endpoint,
        id = id,
        label = label,
        serviceURL = serviceURL,
        version = version,
        type = type,
        supportsFirstLatest = supportsFirstLatest,
        quantities = quantities)

}

#' @export
#' @describeIn Service-class Checks if \code{x} is a \code{Service}.
is.Service <- function(x) is(x, "Service")

#' @export
#' @describeIn Service-class Coerces \code{x} into a \code{Service}.
as.Service <- function(x) as(x, "Service")

setClassUnion("Service_or_NULL", c("Service", "NULL"))
setClassUnion("Service_or_characters", c("Service", "character"))

#' @rdname accessor-methods
setMethod("serviceURL",
          signature(x = "Service"),
          function(x) x@serviceURL)

#' @rdname accessor-methods
setMethod("version",
          signature(x = "Service"),
          function(x) x@version)

#' @rdname accessor-methods
setMethod("type",
          signature(x = "Service"),
          function(x) x@type)

#' @rdname accessor-methods
setMethod("supportsFirstLatest",
          signature(x = "Service"),
          function(x) x@supportsFirstLatest)

#' @rdname accessor-methods
setMethod("quantities",
          signature(x = "Service"),
          function(x) x@quantities)

#' @rdname accessor-methods
setMethod("serviceURL<-",
          signature(x = "Service",
                    value = "character_or_NULL"),
          function(x, value) {
              x@serviceURL <- stretch(length(x), value, as.character(NA), as.character)
              invisible(x)
          })

#' @rdname accessor-methods
setMethod("version<-",
          signature(x = "Service",
                    value = "character_or_NULL"),
          function(x, value) {
              x@version <- stretch(length(x), value, as.character(NA), as.character)
              invisible(x)
          })

#' @rdname accessor-methods
setMethod("type<-",
          signature(x = "Service",
                    value = "character_or_NULL"),
          function(x, value) {
              x@type <- stretch(length(x), value, as.character(NA), as.character)
              invisible(x)
          })

#' @rdname accessor-methods
setMethod("supportsFirstLatest<-",
          signature(x = "Service",
                    value = "logical_or_NULL"),
          function(x, value) {
              x@supportsFirstLatest <- stretch(length(x), value, as.logical(NA), as.logical)
              invisible(x)
          })

#' @rdname accessor-methods
setMethod("quantities<-",
          signature(x = "Service",
                    value = "data.frame_or_NULL"),
          function(x, value) {
              x@quantities <- create.quantities(length(x), value)
              invisible(x)
          })

default.quantities <- function(size) {
    values <- as.numeric(rep(NA, size))
    data.frame(stations = values,
               procedures = values,
               timeseries = values,
               features = values,
               offerings = values,
               categories = values,
               phenomena = values)
}

create.quantities <- function(len, quantities) {
    if (len == 0)
        default.quantities(0)
    else if (is.null(quantities) ||
                 dim(quantities)[1] == 0)
        default.quantities(len)
    else if (dim(quantities)[1] == 1)
        rep(quantities, length.out = len)
    else quantities
}

setAs("character", "Service", function(from) Service(id = from))
setAs("list", "Service", function(from) concat.list(from))

rbind2.Service <- function(x, y) {
    x <- as.Service(x)
    y <- as.Service(y)
    Service(endpoint = rbind2(endpoint(x), endpoint(y)),
            id = c(id(x), id(y)),
            label = c(label(x), label(y)),
            serviceURL = c(serviceURL(x), serviceURL(y)),
            version = c(version(x), version(y)),
            type = c(type(x), type(y)),
            supportsFirstLatest = c(supportsFirstLatest(x), supportsFirstLatest(y)),
            quantities = rbind(quantities(x), quantities(y)))
}

#' @rdname rbind2-methods
setMethod("rbind2", signature("Service", "Service"),
          function(x, y) rbind2.Service(x, y))

#' @rdname rbind2-methods
setMethod("rbind2", signature("Service", "ANY"),
          function(x, y) rbind2.Service(x, as.Service(y)))

#' @rdname rbind2-methods
setMethod("rbind2", signature("ANY", "Service"),
          function(x, y) rbind2.Service(as.Service(x), y))

#' @rdname rep-methods
setMethod("rep", signature(x = "Service"), function(x, ...)
    Service(endpoint = rep(endpoint(x), ...),
            id = rep(id(x), ...),
            label = rep(label(x), ...),
            serviceURL = rep(serviceURL(x), ...),
            version = rep(version(x), ...),
            type = rep(type(x), ...),
            supportsFirstLatest = rep(supportsFirstLatest(x), ...),
            quantities = rep(quantities(x), ...)))
