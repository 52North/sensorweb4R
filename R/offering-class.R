#' @include domainresource-class.R
NULL

#' @export
setClass("Offering",
         contains = "DomainResource")

#' @export
Offering <- function(endpoint, id, label = character(), service, domainId = character()) {
  return(new("Offering", endpoint = endpoint, id = id, label = label, service = service, domainId = domainId))
}

setClassUnion("Offering_or_characters", c("Offering", "character"))
