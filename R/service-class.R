#' @include resource-class.R
NULL

#' @export
setClass("Service",
         contains = "Resource",
         slots = list(serviceUrl = "character",
                      version = "character",
                      type = "character",
                      supportsFirstLatest = "logical",
                      quantities = "list"))

#' @export
Service <- function(endpoint, id, label = character(),
                    serviceUrl = character(), version = character(),
                    type = character(), supportsFirstLatest = FALSE,
                    quantities = list(), ...) {
  return(new("Service", endpoint = endpoint, id = id,
             serviceUrl = serviceUrl, version = version, type = type,
             supportsFirstLatest = supportsFirstLatest,
             quantities = quantities, ...))
}

setClassUnion("Service_or_characters", c("Service", "character"))
