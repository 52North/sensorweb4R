#' @include generic-methods.R
#' @include helper-methods.R
#' @include virtual-class-api-resource.R
#' @include class-service.R
NULL

#' Category
#'
#' @family API Resources
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @export
#' @name Category-class
#' @rdname Category-class
setClass("Category",
         contains = "ApiResource",
         slots = list(service = "Service"),
         validity = function(object) {
             errors <- assert.same.length(id = object@id,
                                          service = object@service)
             if (length(errors) == 0) TRUE else errors
         })

#' @export
#' @describeIn Category-class Checks whether \code{x} is a \code{Category}.
is.Category <- function(x) is(x, "Category")

#' @export
#' @describeIn Category-class Coerces \code{x} into a \code{Category}.
as.Category <- function(x) as(x, "Category")

#' @export
rbind.Category <- concat

setClassUnion("Category_or_characters",
              c("Category", "character"))

setClassUnion("Category_or_NULL",
              c("Category", "NULL"))

#' @export
#' @describeIn Category-class Creates a new \code{Category}.
Category <- function(id = character(), label = NULL,
                     endpoint = NULL, service = NULL) {
    id <- as.character(id)
    len <- length(id)
    label <- stretch(len, label, as.character(NA), as.character)
    endpoint <- stretch(len, endpoint, as.character(NA), as.Endpoint)
    service <- stretch(len, service, as.character(NA), as.Service)
    return(new("Category", id = id, endpoint = endpoint,
               label = label, service = service))
}

#' @rdname api-relations
setMethod("service",
          signature(x = "Category"),
          function(x) x@service)

#' @rdname api-relations
setMethod("service<-",
          signature(x = "Category",
                    value = "Service_or_NULL"),
          function(x, value) {
              x@service <- stretch(length(x), value, as.character(NA), as.Service)
              invisible(x)
          })

setAs("character", "Category", function(from) Category(id = from))
setAs("list", "Category", function(from) concat.list(from))

rbind2.Category <- function(x, y) {
    x <- as.Category(x)
    y <- as.Category(y)
    Category(endpoint = rbind2(endpoint(x), endpoint(y)),
             id = c(id(x), id(y)),
             label = c(label(x), label(y)),
             service = rbind2(service(x), service(y)))
}

#' @rdname rbind2-methods
setMethod("rbind2", signature("Category", "Category"),
          function(x, y) rbind2.Category(x, y))

#' @rdname rbind2-methods
setMethod("rbind2", signature("Category", "ANY"),
          function(x, y) rbind2.Category(x, as.Category(y)))

#' @rdname rbind2-methods
setMethod("rbind2", signature("ANY", "Category"),
          function(x, y) rbind2.Category(as.Category(x), y))

#' @rdname rep-methods
setMethod("rep", signature(x = "Category"),
          function(x, ...)
              Category(endpoint = rep(endpoint(x), ...),
                       id = rep(id(x), ...),
                       label = rep(label(x), ...),
                       service = rep(service(x), ...)))
