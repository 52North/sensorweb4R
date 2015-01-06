#' @include domainresource-class.R
NULL

#' @export
setClass("Phenomenon",
         contains = "DomainResource")

#' @export
Phenomenon <- function(endpoint, id, label = character(), service, domainId = character()) {
  return(new("Phenomenon", endpoint = endpoint, id = id, label = label, service = service, domainId = domainId))
}

setClassUnion("Phenomenon_or_characters", c("Phenomenon", "character"))
