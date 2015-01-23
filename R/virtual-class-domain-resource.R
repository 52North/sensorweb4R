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
DomainResource <- function(type, id = character(), label = NULL,
                           domainId = NULL, service = NULL,
                           endpoint= NULL) {
    id <- as.character(id)
    len <- length(id)
    label <- stretch(len, label, NA, as.character)
    domainId <- stretch(len, domainId, NA, as.character)
    service <- stretch(len, service, as.character(NA), as.Service)
    endpoint <- stretch(len, endpoint, as.character(NA), as.Endpoint)
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
              x@domainId <- stretch(length(x), value, NA, as.character)
              invisible(x)
          })

setMethod("service",
          signature(x = "DomainResource"),
          function(x) x@service)

setMethod("service<-",
          signature(x = "DomainResource",
                    value = "Service"),
          function(x, value) {
              x@service <- stretch(length(x), value, as.character(NA), as.Service)
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

setMethod("rbind2", signature("DomainResource", "DomainResource"),
          function(x, y) rbind2.DomainResource(x, y))
setMethod("rbind2", signature("DomainResource", "ANY"),
          function(x, y) rbind2.DomainResource(x, as.DomainResource(y)))
setMethod("rbind2", signature("ANY", "DomainResource"),
          function(x, y) rbind2.DomainResource(as.DomainResource(x), y))
setMethod("rbind2", signature("ANY", "ANY"),
          function(x, y) rbind2.DomainResource(as.DomainResource(x), as.DomainResource(y)))

setMethod("rep", signature(x = "DomainResource"),
          function(x, ...) {
              cnstr <- get(class(x), mode = "function")
              cnstr(endpoint = rep(endpoint(x), ...),
                    id = rep(id(x), ...),
                    label = rep(label(x), ...),
                    service = rep(service(x), ...),
                    domainId = rep(domainId(x), ...))
          })
