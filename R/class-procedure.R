#' @include virtual-class-domain-resource.R
NULL

#' @export
setClass("Procedure",
         contains = "DomainResource")

#' @export
is.Procedure <- function(x) is(x, "Procedure")
#' @export
as.Procedure <- function(x) as(x, "Procedure")

setClassUnion("Procedure_or_characters",
              c("Procedure", "character"))

setClassUnion("Procedure_or_NULL",
              c("Procedure", "NULL"))

#' @inheritParams DomainResource
#' @export
Procedure <- function(...)
    DomainResource(type = "Procedure", ...)

setAs("character", "Procedure", function(from) Procedure(id = from))
setAs("list", "Procedure", function(from) concat.list(from))
