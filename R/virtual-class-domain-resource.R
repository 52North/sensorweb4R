#' @include generic-methods.R
#' @include helper-methods.R
#' @include virtual-class-api-resource.R
#' @include class-service.R
NULL


#' @export
setClass("DomainResource",
         contains = c("VIRTUAL",
                      "ApiResource"),
         slots = list(domainId = "character",
                      service = "Service_or_NULL"))

#' @export
DomainResource <- function(type,
                           endpoint,
                           id,
                           label = rep(as.character(NA), length(id)),
                           domainId = rep(as.character(NA), length(id)),
                           service = NULL) {
    return(new(type,
               endpoint = endpoint,
               id = id,
               label = label,
               service = service,
               domainId = domainId))
}

#' @export
is.DomainResource <- function(x) is(x, "DomainResource")
#' @export
as.DomainResource <- function(x) as(x, "DomainResource")

setMethod("domainId",
          signature(x = "DomainResource"),
          function(x) x@domainId)

setMethod("domainId<-",
          signature(x = "DomainResource",
                    value = "character"),
          function(x, value) {
              check_length(x, value)
              x@domainId <- value
              invisible(x)
          })

setMethod("service",
          signature(x = "DomainResource"),
          function(x) x@service)

setMethod("service<-",
          signature(x = "DomainResource",
                    value = "Service"),
          function(x, value) {
              check_length(x, value)
              x@service <- value
              invisible(x)
          })


rbind2.DomainResource <- function(x, y) {
    x <- as.DomainResource(x)
    y <- as.DomainResource(y)
    new(class(x),
        endpoint = concat(endpoint(x), endpoint(y)),
        id = c(id(x), id(y)),
        label = c(label(x), label(y)),
        domainId = c(domainId(x), domainId(y)),
        service = concat(service(x), service(y)))
}

setMethod("rbind2", signature("DomainResource", "DomainResource"), function(x, y) concat.pair.DomainResource(x, y))
setMethod("rbind2", signature("DomainResource", "ANY"), function(x, y) concat.pair.DomainResource(x, as.DomainResource(y)))
setMethod("rbind2", signature("ANY", "DomainResource"), function(x, y) concat.pair.DomainResource(as.DomainResource(x), y))
setMethod("rbind2", signature("ANY", "ANY"), function(x, y) concat.pair.DomainResource(as.DomainResource(x), as.DomainResource(y)))
