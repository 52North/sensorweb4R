#' @include domainresource-class.R
NULL

#' @export
setClass("Feature",
         contains = "DomainResource")

#' @export
Feature <- function(endpoint, id, label = character(), service, domainId = character()) {
  return(new("Feature", endpoint = endpoint, id = id, label = label, service = service, domainId = domainId))
}

setClassUnion("Feature_or_characters", c("Feature", "character"))
