#' @include virtual-class-domain-resource.R
NULL

#' Phenomenon
#'
#' Represents a phenomenon.
#'
#' @family API Resources
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname Phenomenon-class
#' @name Phenomenon-class
#' @export

setClass("Phenomenon",
         contains = "DomainResource")

#' @export
#' @describeIn Phenomenon-class Checks whether \code{x} is a \code{Phenomenon}.
is.Phenomenon <- function(x) is(x, "Phenomenon")

#' @export
#' @describeIn Phenomenon-class Coerces \code{x} into a \code{Phenomenon}.
as.Phenomenon <- function(x) as(x, "Phenomenon")

setClassUnion("Phenomenon_or_characters",
              c("Phenomenon", "character"))

setClassUnion("Phenomenon_or_NULL",
              c("Phenomenon", "NULL"))

#' @inheritParams DomainResource
#' @export
#' @describeIn Phenomenon-class Constructs a new \code{Phenomenon}.
Phenomenon <- function(...)
    DomainResource(type = "Phenomenon", ...)

setAs("character", "Phenomenon",
      function(from) Phenomenon(id = from))
setAs("list", "Phenomenon",
      function(from) concat.list(from))
