#' @include virtual-class-domain-resource.R
NULL


#' Feature
#'
#' Represents a feature.
#'
#' @family API Resources
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname Feature-class
#' @name Feature-class
#' @export
setClass("Feature",
         contains = "DomainResource")

#' @export
#' @describeIn Feature-class Checks whether \code{x} is a \code{Feature}.
is.Feature <- function(x) is(x, "Feature")

#' @export
#' @describeIn Feature-class Coerces \code{x} into a \code{Feature}.
as.Feature <- function(x) as(x, "Feature")

setClassUnion("Feature_or_characters",
              c("Feature", "character"))

setClassUnion("Feature_or_NULL",
              c("Feature", "NULL"))

#' @inheritParams DomainResource
#' @export
#' @describeIn Feature-class Constructs a new \code{Feature}.
Feature <- function(...)
    DomainResource(type = "Feature", ...)

setAs("character", "Feature", function(from) Feature(id = from))
setAs("list", "Feature", function(from) concat.list(from))
