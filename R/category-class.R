
#' @include resource-class.R
#' @include service-class.R
NULL

#' @export
setClass("Category",
         contains = "Resource",
         slots = list(service = "Service"))

#' @export
Category <- function(endpoint, id, label = character(), service) {
  return(new("Category", endpoint = endpoint, id = id, label = label, service = service))
}

setClassUnion("Category_or_characters", c("Category", "character"))
