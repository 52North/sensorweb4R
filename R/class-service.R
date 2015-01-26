#' @include generic-methods.R
#' @include helper-methods.R
#' @include virtual-class-api-resource.R
#' @include class-unions.R
NULL

#' @export
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
is.Service <- function(x) is(x, "Service")
#' @export
as.Service <- function(x) as(x, "Service")

setClassUnion("Service_or_NULL", c("Service", "NULL"))
setClassUnion("Service_or_characters", c("Service", "character"))

setMethod("serviceURL",
          signature(x = "Service"),
          function(x) x@serviceURL)

setMethod("version",
          signature(x = "Service"),
          function(x) x@version)

setMethod("type",
          signature(x = "Service"),
          function(x) x@type)

setMethod("supportsFirstLatest",
          signature(x = "Service"),
          function(x) x@supportsFirstLatest)

setMethod("quantities",
          signature(x = "Service"),
          function(x) x@quantities)

setMethod("serviceURL<-",
          signature(x = "Service",
                    value = "character_or_NULL"),
          function(x, value) {
              x@serviceURL <- stretch(length(x), value, as.character(NA), as.character)
              invisible(x)
          })

setMethod("version<-",
          signature(x = "Service",
                    value = "character_or_NULL"),
          function(x, value) {
              x@version <- stretch(length(x), value, as.character(NA), as.character)
              invisible(x)
          })

setMethod("type<-",
          signature(x = "Service",
                    value = "character_or_NULL"),
          function(x, value) {
              x@type <- stretch(length(x), value, as.character(NA), as.character)
              invisible(x)
          })

setMethod("supportsFirstLatest<-",
          signature(x = "Service",
                    value = "logical_or_NULL"),
          function(x, value) {
              x@supportsFirstLatest <- stretch(length(x), value, as.logical(NA), as.logical)
              invisible(x)
          })

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

#' @export
Service <- function(id = character(),
                    label = NULL,
                    serviceURL = NULL,
                    version = NULL,
                    type = NULL,
                    supportsFirstLatest = NULL,
                    quantities = NULL,
                    endpoint = NULL) {

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
setMethod("rbind2", signature("Service", "Service"),
          function(x, y) rbind2.Service(x, y))
setMethod("rbind2", signature("Service", "ANY"),
          function(x, y) rbind2.Service(x, as.Service(y)))
setMethod("rbind2", signature("ANY", "Service"),
          function(x, y) rbind2.Service(as.Service(x), y))
setMethod("rbind2", signature("ANY", "ANY"),
          function(x, y) rbind2.Service(as.Service(x),
                                             as.Service(y)))

setMethod("rep", signature(x = "Service"), function(x, ...)
    Service(endpoint = rep(endpoint(x), ...),
            id = rep(id(x), ...),
            label = rep(label(x), ...),
            serviceURL = rep(serviceURL(x), ...),
            version = rep(version(x), ...),
            type = rep(type(x), ...),
            supportsFirstLatest = rep(supportsFirstLatest(x), ...),
            quantities = rep(quantities(x), ...)))
