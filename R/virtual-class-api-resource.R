#' @include generic-methods.R
#' @include helper-methods.R
#' @include virtual-class-http-resource.R
#' @include class-endpoint.R
NULL


#' @export
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
is.ApiResource <- function(x) is(x, "ApiResource")
#' @export
as.ApiResource <- function(x) as(x, "ApiResource")

#' @export
length.ApiResource <- function(x) length(id(x))

setMethod("id",
          signature(x = "ApiResource"),
          function(x) x@id)

setMethod("label",
          signature(x = "ApiResource"),
          function(x) x@label)

setMethod("label<-",
          signature(x = "ApiResource",
                    value = "character"),
          function(x, value) {
              check_length(x, value)
              x@label <- value
              invisible(x)
          })

setMethod("endpoint",
          signature(x = "ApiResource"),
          function(x) x@endpoint)

setMethod("endpoint<-",
          signature(x = "ApiResource",
                    value = "Endpoint"),
          function(x, value) {
              x@endpoint <- value
              invisible(x)
          })

setMethod("resourceURL",
          signature(x = "ApiResource"),
          function(x) {
              paste(resourceURL(endpoint(x)),
                    .collectionName(x),
                    id(x),
                    sep = "/")
          })

setMethod("length",
          signature(x = "ApiResource"),
          length.ApiResource)

#' @export
unique.ApiResource <- function(x, ...) {
    x[sapply(id(x), function(id) which.max(id(x) == id))]
}

setMethod("unique",
          signature(x = "ApiResource"),
          unique.ApiResource)

