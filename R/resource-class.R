#' @include httpresource-class.R
#' @include endpoint-class.R
#' @include generic-methods.R
NULL

#' @export
setClass("Resource",
         contains = "HttpResource",
         slots = c(id = "character",
                   label = "character",
                   endpoint = "Endpoint"))

setMethod("resourceURL", "Resource", function(x, ...) {
    url <- resourceURL(endpoint(x))
    resource <- switch(class(x),
                       Service = "services",
                       Station = "stations",
                       Timeseries = "timeseries",
                       Category = "categories",
                       Offering = "offerings",
                       Feature = "features",
                       Procedure = "procedures",
                       Phenomenon = "phenomena")
    paste(url, resource, id(x), sep = "/")
})

setMethod("id", "Resource",
          function(x, ...) x@id)

setMethod("label", "Resource",
          function(x, ...) x@label)

setMethod("endpoint", "Resource",
          function(x, ...) x@endpoint)

setMethod("coerce", signature(from = "Resource", to = "character"),
          function(from, to, strict) as.character.Resource(from))

#' @export
as.character.Resource <- function(x, ...) id(x)
