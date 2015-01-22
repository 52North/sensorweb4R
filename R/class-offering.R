#' @include virtual-class-domain-resource.R
NULL

#' @export
setClass("Offering",
         contains = "DomainResource")

#' @export
is.Offering <- function(x) is(x, "Offering")
#' @export
as.Offering <- function(x) as(x, "Offering")
#' @export
rbind.Category <- concat

setClassUnion("Offering_or_characters",
              c("Offering", "character"))

setClassUnion("Offering_or_NULL",
              c("Offering", "NULL"))

#' @inheritParams DomainResource
#' @export
Offering <- function(...)
    DomainResource(type = "Offering", ...)

setAs("character", "Offering", function(from) Offering(id = from))
