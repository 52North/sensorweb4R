#' @include virtual-class-domain-resource.R
NULL

#' @export
setClass("Feature",
         contains = "DomainResource")

#' @export
is.Feature <- function(x) is(x, "Feature")
#' @export
as.Feature <- function(x) as(x, "Feature")

setClassUnion("Feature_or_characters",
              c("Feature", "character"))

setClassUnion("Feature_or_NULL",
              c("Feature", "NULL"))

#' @inheritParams DomainResource
#' @export
Feature <- function(...)
    DomainResource(type = "Feature", ...)

setAs("character", "Feature", function(from) Feature(id = from))
