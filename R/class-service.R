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
                    value = "character"),
          function(x, value) {
              check_length(x, value);
              x@serviceURL <- value
              invisible(x)
          })

setMethod("version<-",
          signature(x = "Service",
                    value = "character"),
          function(x, value) {
              check_length(x, value);
              x@version <- value
              invisible(x)
          })

setMethod("type<-",
          signature(x = "Service",
                    value = "character"),
          function(x, value) {
              check_length(x, value);
              x@type <- value
              invisible(x)
          })

setMethod("supportsFirstLatest<-",
          signature(x = "Service",
                    value = "logical"),
          function(x, value) {
              check_length(x, value);
              x@supportsFirstLatest <- value
              invisible(x)
          })

setMethod("quantities<-",
          signature(x = "Service",
                    value = "data.frame"),
          function(x, value) {
              if (length(x) != dim(value)[1]) {
                  stop(paste("incorrect length", length(x), "!=", dim(value)[2]))
              }
              x@quantities <- value
              invisible(x)
          })


#' @export
Service <- function(id,
                    label = NULL,
                    serviceURL = NULL,
                    version = NULL,
                    type = NULL,
                    supportsFirstLatest = NULL,
                    quantities = NULL,
                    endpoint = NULL) {

    l <- length(id)
    if (is.null(label)) label <- as.character(NA)
    if (length(label) == 1) label <- rep(label, l)
    if (is.null(serviceURL)) serviceURL <- as.character(NA)
    if (length(serviceURL) == 1) serviceURL <- rep(serviceURL, l)
    if (is.null(version)) version <- as.character(NA)
    if (length(version) == 1) version <- rep(version, l)
    if (is.null(type)) type <- as.character(NA)
    if (length(type) == 0) type <- rep(type, l)
    if (is.null(supportsFirstLatest)) supportsFirstLatest <- as.logical(NA)
    if (length(supportsFirstLatest)) supportsFirstLatest <- rep(supportsFirstLatest, l)
    if (is.null(quantities)) quantities <- data.frame()
    if (is.null(endpoint)) endpoint <- Endpoint(as.character(NA))
    if (length(endpoint) == 1) endpoint <- rep(endpoint, l)
    if (length(endpoint) == 0) endpoint <- rep(endpoint, l)
    x <- new("Service",
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

rbind2.Service <- function(x, y) {
    x <- as.Service(x)
    y <- as.Service(y)
    Service(endpoint = concat(endpoint(x), endpoint(y)),
            id = c(id(x), id(y)),
            label = c(label(x), label(y)),
            serviceURL = c(serviceURL(x), serviceURL(y)),
            version = c(version(x), version(y)),
            type = c(type(x), type(y)),
            supportsFirstLatest = c(supportsFirstLatest(x), supportsFirstLatest(y)),
            quantities = rbind(quantities(x), quantities(y)))
}
setMethod("rbind2", signature("Service", "Service"), function(x, y) concat.pair.Service(x, y))
setMethod("rbind2", signature("Service", "ANY"), function(x, y) concat.pair.Service(x, as.Service(y)))
setMethod("rbind2", signature("ANY", "Service"), function(x, y) concat.pair.Service(as.Service(x), y))
setMethod("rbind2", signature("ANY", "ANY"), function(x, y) concat.pair.Service(as.Service(x), as.Service(y)))

setMethod("rep", signature(x = "Service"), function(x, ...)
    Service(endpoint = rep(endpoint(x), ...),
            id = rep(id(x), ...),
            label = rep(label(x), ...),
            serviceURL = rep(serviceURL(x), ...),
            version = rep(version(x), ...),
            type = rep(type(x), ...),
            supportsFirstLatest = rep(supportsFirstLatest(x), ...),
            quantities = rep(quantities(x), ...)))
