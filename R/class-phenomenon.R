#' @include virtual-class-domain-resource.R
NULL

#' @export
setClass("Phenomenon",
         contains = "DomainResource")

#' @export
is.Phenomenon <- function(x) is(x, "Phenomenon")
#' @export
as.Phenomenon <- function(x) as(x, "Phenomenon")

setClassUnion("Phenomenon_or_characters",
              c("Phenomenon", "character"))

setClassUnion("Phenomenon_or_NULL",
              c("Phenomenon", "NULL"))

#' @inheritParams DomainResource
#' @export
Phenomenon <- function(...)
    DomainResource(type = "Phenomenon", ...)

setAs("character", "Phenomenon", function(from) Phenomenon(id = from))
