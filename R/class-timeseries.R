#' @include generic-methods.R
#' @include class-phenomenon.R
#' @include class-service.R
#' @include class-feature.R
#' @include class-offering.R
#' @include class-procedure.R
#' @include class-category.R
#' @include class-station.R
#' @include class-tvp.R
NULL

#' @export
setClass("Timeseries",
         contains = "ApiResource",
         slots = list(uom = "character",
                      phenomenon = "Phenomenon",
                      service = "Service",
                      feature = "Feature",
                      offering = "Offering",
                      procedure = "Procedure",
                      category = "Category",
                      station = "Station",
                      firstValue = "TVP",
                      lastValue = "TVP",
                      referenceValues = "list",
                      statusIntervals = "list"),
         validity = function(object) {
             errors <- assert.same.length(id = object@id,
                                          uom = object@uom,
                                          phenomenon = object@phenomenon,
                                          service = object@service,
                                          feature = object@feature,
                                          offering = object@offering,
                                          procedure = object@procedure,
                                          category = object@category,
                                          station = object@station,
                                          firstValue = object@firstValue,
                                          lastValue = object@lastValue,
                                          referenceValues = object@referenceValues,
                                          statusIntervals = object@statusIntervals)
             if (length(errors) == 0) TRUE else errors
         })

#' @export
is.Timeseries <- function(x) is(x, "Timeseries")
#' @export
as.Timeseries <- function(x) as(x, "Timeseries")

setClassUnion("Timeseries_or_characters",
              c("Timeseries", "character"))

setClassUnion("Timeseries_or_NULL",
              c("Timeseries", "NULL"))


#' @export
Timeseries <- function(id,
                       label = rep(as.character(NA), length(id)),
                       uom = rep(as.character(NA), length(id)),
                       endpoint = rep(Endpoint(as.character(NA)), length(id)),
                       phenomenon = rep(Phenomenon(as.character(NA)), length(id)),
                       service = rep(Service(as.character(NA)), length(id)),
                       feature = rep(Feature(as.character(NA)), length(id)),
                       offering = rep(Offering(as.character(NA)), length(id)),
                       procedure = rep(Procedure(as.character(NA)), length(id)),
                       category = rep(Category(as.character(NA)), length(id)),
                       station = rep(Station(as.character(NA)), length(id)),
                       statusIntervals = rep(list(StatusInterval(as.character(NA)), length(id))),
                       firstValue = rep(TVP(as.character(NA)), length(id)),
                       lastValue = rep(TVP(as.character(NA)), length(id)),
                       referenceValues = rep(ReferenceValue(as.character(NA)), length(id))) {
    return(new("Timeseries",
               endpoint = endpoint,
               id = id,
               label = label,
               uom = uom,
               phenomenon = phenomenon,
               service = service,
               category = category,
               feature = feature,
               offering = offering,
               procedure = procedure,
               station = station,
               referenceValues = referenceValues,
               firstValue = firstValue,
               lastValue = lastValue))
}
setMethod("stations",
          signature(x = "Timeseries"),
          function(x) x@station)

setMethod("service",
          signature(x = "Timeseries"),
          function(x) x@service)

setMethod("feature",
          signature(x = "Timeseries"),
          function(x) x@feature)

setMethod("offering",
          signature(x = "Timeseries"),
          function(x) x@offering)

setMethod("category",
          signature(x = "Timeseries"),
          function(x) x@category)

setMethod("procedure",
          signature(x = "Timeseries"),
          function(x) x@procedure)

setMethod("station",
          signature(x = "Timeseries"),
          function(x) x@station)

setMethod("uom",
          signature(x = "Timeseries"),
          function(x) x@uom)

setMethod("phenomenon",
          signature(x = "Timeseries"),
          function(x) x@phenomenon)

setMethod("firstValue",
          signature(x = "Timeseries"),
          function(x) x@firstValue)

setMethod("lastValue",
          signature(x = "Timeseries"),
          function(x) x@lastValue)

setMethod("statusIntervals",
          signature(x = "Timeseries"),
          function(x) x@statusIntervals)

setMethod("getDataURL",
          signature(x = "Timeseries"),
          function(x) subresourceURL(x, "getData"))

setAs("character", "Timeseries", function(from) Timeseries(id = from))


rbind2.Timeseries <- function(x, y) {
    x <- as.Timeseries(x)
    y <- as.Timeseries(y)

    Timeseries(endpoint = concat(endpoint(x), endpoint(y)),
               id = c(id(x), id(y)),
               label = c(label(x), label(y)),
               uom = c(uom(x), uom(y)),
               phenomenon = rbind(phenomenon(x), phenomenon(y)),
               service = rbind(service(x), service(y)),
               category = rbind(category(x), category(y)),
               feature = rbind(feature(x), feature(y)),
               offering = rbind(offering(x), offering(y)),
               procedure = rbind(procedure(x), procedure(y)),
               station = rbind(station(x), station(y)),
               referenceValues = rbind(referenceValues(x), referenceValues(y)),
               firstValue = rbind(firstValue(x), firstValue(y)),
               lastValue = rbind(lastValue(x), lastValue(y)))
}
setMethod("rbind2", signature("Timeseries", "Timeseries"), function(x, y) concat.pair.Timeseries(x, y))
setMethod("rbind2", signature("Timeseries", "ANY"), function(x, y) concat.pair.Timeseries(x, as.Timeseries(y)))
setMethod("rbind2", signature("ANY", "Timeseries"), function(x, y) concat.pair.Timeseries(as.Timeseries(x), y))
setMethod("rbind2", signature("ANY", "ANY"), function(x, y) concat.pair.Timeseries(as.Timeseries(x), as.Timeseries(y)))

setMethod("rep", signature(x = "Timeseries"), function(x, ...)
    Timeseries(endpoint = rep(endpoint(x), ...),
               id = rep(id(x), ...),
               label = rep(label(x), ...),
               uom = rep(uom(x), ...),
               phenomenon = rep(phenomenon(x), ...),
               service = rep(service(x), ...),
               category = rep(category(x), ...),
               feature = rep(feature(x), ...),
               offering = rep(offering(x), ...),
               procedure = rep(procedure(x), ...),
               station = rep(station(x), ...),
               referenceValues = rep(referenceValues(x), ...),
               firstValue = rep(firstValue(x), ...),
               lastValue = rep(lastValue(x), ...)))
