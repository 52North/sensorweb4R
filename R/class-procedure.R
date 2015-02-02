#' @include virtual-class-domain-resource.R
NULL

#' Procedure
#'
#' Represents a procedure.
#'
#' @family API Resources
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname Procedure-class
#' @name Procedure-class
#' @export
setClass("Procedure",
         contains = "DomainResource")

#' @export
#' @describeIn Procedure-class Checks whether \code{x} is a \code{Procedure}.
is.Procedure <- function(x) is(x, "Procedure")

#' @export
#' @describeIn Procedure-class Coerces \code{x} into a \code{Procedure}.
as.Procedure <- function(x) as(x, "Procedure")

setClassUnion("Procedure_or_characters",
              c("Procedure", "character"))

setClassUnion("Procedure_or_NULL",
              c("Procedure", "NULL"))

#' @inheritParams DomainResource
#' @export
#' @describeIn Procedure-class Constructs a new \code{Procedure}.
Procedure <- function(...)
    DomainResource(type = "Procedure", ...)

setAs("character", "Procedure", function(from) Procedure(id = from))
setAs("list", "Procedure", function(from) concat.list(from))
