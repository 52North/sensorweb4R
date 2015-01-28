#' @include virtual-class-domain-resource.R
NULL

#' Offering
#'
#' Represents a offering.
#'
#' @family API Resources
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname Offering-class
#' @name Offering-class
#' @export
setClass("Offering",
         contains = "DomainResource")

#' @export
#' @describeIn Offering-class Checks whether \code{x} is a \code{Offering}.
is.Offering <- function(x) is(x, "Offering")

#' @export
#' @describeIn Offering-class Coerces \code{x} into a \code{Offering}.
as.Offering <- function(x) as(x, "Offering")
#' @export
rbind.Category <- concat

setClassUnion("Offering_or_characters",
              c("Offering", "character"))

setClassUnion("Offering_or_NULL",
              c("Offering", "NULL"))

#' @inheritParams DomainResource
#' @export
#' @describeIn Offering-class Constructs a new \code{Offering}.
Offering <- function(...)
    DomainResource(type = "Offering", ...)

setAs("character", "Offering", function(from) Offering(id = from))
setAs("list", "Offering", function(from) concat.list(from))
