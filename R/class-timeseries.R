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

#' Timeseries
#'
#' Represents a timeseries.
#'
#' @family API Resources
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname Timeseries-class
#' @name Timeseries-class
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
#' @describeIn Timeseries-class Checks whether \code{x} is a \code{Timeseries}.
is.Timeseries <- function(x) is(x, "Timeseries")
#' @export
#' @describeIn Timeseries-class Coerces \code{x} into a \code{Timeseries}.
as.Timeseries <- function(x) as(x, "Timeseries")

setClassUnion("Timeseries_or_characters",
              c("Timeseries", "character"))

setClassUnion("Timeseries_or_NULL",
              c("Timeseries", "NULL"))

create.value <- function(len, x) {
    if (len == 0)
        TVP()
    else if (is.null(x) || length(x) == 0)
        rep(TVP(NA), len)
    else if (length(x) == 1)
        rep(as.TVP(x), len)
    else as.TVP(x)
}

create.StatusIntervals <- function(len, x) {
    if (len == 0)
        list()
    else if (is.null(x) || length(x) == 0)
        rep(list(StatusInterval()), length.out = len)
    else if (length(x) == 1) {
        if (is.StatusInterval(x))
            rep(list(x), length.out = len)
        else
            rep(x, length.out = len)
    }
    else x
}

create.ReferenceValues <- function(len, x) {
    if (len == 0)
        list()
    else if (is.null(x) || length(x) == 0)
        rep(list(ReferenceValue()), length.out = len)
    else if (length(x) == 1) {
        if (is.ReferenceValue(x))
            rep(list(x), length.out = len)
        else
            rep(x, length.out = len)
    } else if (length(x) == 1) {

    }
    else as.list(x)
}

#' @export
#' @describeIn Timeseries-class Constructs a new \code{Timeseries}.
Timeseries <- function(id = character(), label = NULL, uom = NULL, endpoint = NULL,
                       phenomenon = NULL, service = NULL, feature = NULL, offering = NULL,
                       procedure = NULL, category = NULL, station = NULL, statusIntervals = NULL,
                       firstValue = NULL, lastValue = NULL, referenceValues = NULL) {
    id <- as.character(id)
    len <- length(id)
    label <- stretch(len, label, as.character(NA), as.character)
    endpoint <- stretch(len, endpoint, as.character(NA), as.Endpoint)
    service <- stretch(len, service, as.character(NA), as.Service)
    uom <- stretch(len, uom, as.character(NA), as.character)

    phenomenon <- stretch(len, phenomenon, as.character(NA), as.Phenomenon)
    feature <- stretch(len, feature, as.character(NA), as.Feature)
    offering <- stretch(len, offering, as.character(NA), as.Offering)
    procedure <- stretch(len, procedure, as.character(NA), as.Procedure)
    category <- stretch(len, category, as.character(NA), as.Category)
    station <- stretch(len, station, as.character(NA), as.Station)
    firstValue <- create.value(len, firstValue)
    lastValue <- create.value(len, lastValue)
    referenceValues <- create.ReferenceValues(len, referenceValues)
    statusIntervals <- create.StatusIntervals(len, statusIntervals)

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
               statusIntervals = statusIntervals,
               firstValue = firstValue,
               lastValue = lastValue))
}

#' @rdname accessor-methods
setMethod("referenceValues",
          signature(x = "Timeseries"),
          function(x) x@referenceValues)

#' @rdname accessor-methods
setMethod("referenceValues<-",
          signature(x = "Timeseries",
                    value = "ReferenceValue_or_NULL"),
          function(x, value) {
              x@referenceValues <- create.ReferenceValues(length(x), referenceValues)
              invisible(x)
          })

#' @rdname api-relations
setMethod("service",
          signature(x = "Timeseries"),
          function(x) x@service)

#' @rdname api-relations
setMethod("service<-",
          signature(x = "Timeseries",
                    value = "Service_or_NULL"),
          function(x, value) {
              x@service <- stretch(length(x), value, as.character(NA), as.Service)
              invisible(x)
          })

#' @rdname api-relations
setMethod("feature",
          signature(x = "Timeseries"),
          function(x) x@feature)

#' @rdname api-relations
setMethod("feature<-",
          signature(x = "Timeseries",
                    value = "Feature_or_NULL"),
          function(x, value) {
              x@feature <- stretch(length(x), value, as.character(NA), as.Feature)
              invisible(x)
          })

#' @rdname api-relations
setMethod("offering",
          signature(x = "Timeseries"),
          function(x) x@offering)

#' @rdname api-relations
setMethod("offering<-",
          signature(x = "Timeseries",
                    value = "Offering_or_NULL"),
          function(x, value) {
              x@offering <- stretch(length(x), value, as.character(NA), as.Offering)
              invisible(x)
          })

#' @rdname api-relations
setMethod("category",
          signature(x = "Timeseries"),
          function(x) x@category)

#' @rdname api-relations
setMethod("category<-",
          signature(x = "Timeseries",
                    value = "Category_or_NULL"),
          function(x, value) {
              x@category <- stretch(length(x), value, as.character(NA), as.Category)
              invisible(x)
          })

#' @rdname api-relations
setMethod("procedure",
          signature(x = "Timeseries"),
          function(x) x@procedure)

#' @rdname api-relations
setMethod("procedure<-",
          signature(x = "Timeseries",
                    value = "Procedure_or_NULL"),
          function(x, value) {
              x@procedure <- stretch(length(x), value, as.character(NA), as.Procedure)
              invisible(x)
          })

#' @rdname api-relations
setMethod("station",
          signature(x = "Timeseries"),
          function(x) x@station)

#' @rdname api-relations
setMethod("station<-",
          signature(x = "Timeseries",
                    value = "Station_or_NULL"),
          function(x, value) {
              x@station <- stretch(length(x), value, as.character(NA), as.Station)
              invisible(x)
          })

#' @rdname accessor-methods
setMethod("uom",
          signature(x = "Timeseries"),
          function(x) x@uom)

#' @rdname accessor-methods
setMethod("uom<-",
          signature(x = "Timeseries",
                    value = "character_or_NULL"),
          function(x, value) {
              x@uom <- stretch(length(x), value, NA, as.character)
              invisible(x)
          })

#' @rdname api-relations
setMethod("phenomenon",
          signature(x = "Timeseries"),
          function(x) x@phenomenon)

#' @rdname api-relations
setMethod("phenomenon<-",
          signature(x = "Timeseries",
                    value = "Phenomenon_or_NULL"),
          function(x, value) {
              x@phenomenon <- stretch(length(x), value, as.character(NA), as.Phenomenon)
              invisible(x)
          })

#' @rdname accessor-methods
setMethod("firstValue",
          signature(x = "Timeseries"),
          function(x) x@firstValue)

#' @rdname accessor-methods
setMethod("firstValue<-",
          signature(x = "Timeseries",
                    value = "TVP_or_NULL"),
          function(x, value) {
              x@firstValue <- create.value(length(x), value)
              x
          })

#' @rdname accessor-methods
setMethod("lastValue",
          signature(x = "Timeseries"),
          function(x) x@lastValue)

#' @rdname accessor-methods
setMethod("lastValue<-",
          signature(x = "Timeseries",
                    value = "TVP_or_NULL"),
          function(x, value) {
              x@lastValue <- create.value(length(x), value)
              x
          })

#' @rdname accessor-methods
setMethod("statusIntervals",
          signature(x = "Timeseries"),
          function(x) x@statusIntervals)

#' @rdname url-methods
setMethod("getDataURL",
          signature(x = "Timeseries"),
          function(x) subresourceURL(x, "getData"))

setAs("character", "Timeseries", function(from) Timeseries(id = from))
setAs("list", "Timeseries", function(from) concat.list(from))

rbind2.Timeseries <- function(x, y) {
    x <- as.Timeseries(x)
    y <- as.Timeseries(y)

    Timeseries(endpoint = rbind2(endpoint(x), endpoint(y)),
               id = c(id(x), id(y)),
               label = c(label(x), label(y)),
               uom = c(uom(x), uom(y)),
               phenomenon = rbind2(phenomenon(x), phenomenon(y)),
               service = rbind2(service(x), service(y)),
               category = rbind2(category(x), category(y)),
               feature = rbind2(feature(x), feature(y)),
               offering = rbind2(offering(x), offering(y)),
               procedure = rbind2(procedure(x), procedure(y)),
               station = rbind2(station(x), station(y)),
               referenceValues = c(referenceValues(x), referenceValues(y)),
               firstValue = rbind2(firstValue(x), firstValue(y)),
               statusIntervals = c(statusIntervals(x), statusIntervals(y)),
               lastValue = rbind2(lastValue(x), lastValue(y)))
}

#' @rdname rbind2-methods
setMethod("rbind2", signature("Timeseries", "Timeseries"),
          function(x, y) rbind2.Timeseries(x, y))

#' @rdname rbind2-methods
setMethod("rbind2", signature("Timeseries", "ANY"),
          function(x, y) rbind2.Timeseries(x, as.Timeseries(y)))

#' @rdname rbind2-methods
setMethod("rbind2", signature("ANY", "Timeseries"),
          function(x, y) rbind2.Timeseries(as.Timeseries(x), y))

#' @rdname rep-methods
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
               statusIntervals = rep(statusIntervals(x), ...),
               firstValue = rep(firstValue(x), ...),
               lastValue = rep(lastValue(x), ...)))

