#' @include resource-class.R
#' @include service-class.R
NULL

#' @export
setClass("DomainResource",
         contains = "Resource",
         slots = list(domainId = "character",
                      service = "Service"))
