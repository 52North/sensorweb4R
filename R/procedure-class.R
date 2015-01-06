#' @include domainresource-class.R
NULL

#' @export
setClass("Procedure",
         contains = "DomainResource")

#' @export
Procedure <- function(endpoint, id, label = character(), service, domainId = character()) {
  return(new("Procedure", endpoint = endpoint, id = id, label = label, service = service, domainId = domainId))
}

setClassUnion("Procedure_or_characters", c("Procedure", "character"))
