#' @include virtual-class-api-resource.R
#' @include class-category.R
#' @include virtual-class-domain-resource.R
#' @include class-endpoint.R
#' @include class-feature.R
#' @include virtual-class-http-resource.R
#' @include class-offering.R
#' @include class-phenomenon.R
#' @include class-procedure.R
#' @include class-reference-value.R
#' @include class-service.R
#' @include class-station.R
#' @include class-status-interval.R
#' @include class-timeseries.R
#' @include class-tvp.R
NULL

#' @export
as.character.ApiResource <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("ApiResource", "character", function(from) as.character.ApiResource(from))
setMethod("as.character", signature("ApiResource"), as.character.ApiResource)

#' @export
print.ApiResource <- function(x, ...) print(toString.ApiResource(x, ...))
setMethod("print", signature("ApiResource"), print.ApiResource)
setMethod("show", signature("ApiResource"), function(object) print.ApiResource(object))

#' @export
toString.ApiResource <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("ApiResource"), toString.ApiResource)

#' @export
as.character.Category <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Category", "character", function(from) as.character.Category(from))
setMethod("as.character", signature("Category"), as.character.Category)

#' @export
print.Category <- function(x, ...) print(toString.Category(x, ...))
setMethod("print", signature("Category"), print.Category)
setMethod("show", signature("Category"), function(object) print.Category(object))

#' @export
toString.Category <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("Category"), toString.Category)

#' @export
as.character.DomainResource <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("DomainResource", "character", function(from) as.character.DomainResource(from))
setMethod("as.character", signature("DomainResource"), as.character.DomainResource)

#' @export
print.DomainResource <- function(x, ...) print(toString.DomainResource(x, ...))
setMethod("print", signature("DomainResource"), print.DomainResource)
setMethod("show", signature("DomainResource"), function(object) print.DomainResource(object))

#' @export
toString.DomainResource <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("DomainResource"), toString.DomainResource)

#' @export
as.character.Endpoint <- function(x, ...) noquote(resourceURL(x))
setAs("Endpoint", "character", function(from) as.character.Endpoint(from))
setMethod("as.character", signature("Endpoint"), as.character.Endpoint)

#' @export
print.Endpoint <- function(x, ...) print(toString.Endpoint(x, ...))
setMethod("print", signature("Endpoint"), print.Endpoint)
setMethod("show", signature("Endpoint"), function(object) print.Endpoint(object))

#' @export
toString.Endpoint <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("Endpoint"), toString.Endpoint)

#' @export
as.character.Feature <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Feature", "character", function(from) as.character.Feature(from))
setMethod("as.character", signature("Feature"), as.character.Feature)

#' @export
print.Feature <- function(x, ...) print(toString.Feature(x, ...))
setMethod("print", signature("Feature"), print.Feature)
setMethod("show", signature("Feature"), function(object) print.Feature(object))

#' @export
toString.Feature <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("Feature"), toString.Feature)

#' @export
as.character.HttpResource <- function(x, ...) resourceURL(x)
setAs("HttpResource", "character", function(from) as.character.HttpResource(from))
setMethod("as.character", signature("HttpResource"), as.character.HttpResource)

#' @export
print.HttpResource <- function(x, ...) print(toString.HttpResource(x, ...))
setMethod("print", signature("HttpResource"), print.HttpResource)
setMethod("show", signature("HttpResource"), function(object) print.HttpResource(object))

#' @export
toString.HttpResource <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("HttpResource"), toString.HttpResource)

#' @export
as.character.Offering <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Offering", "character", function(from) as.character.Offering(from))
setMethod("as.character", signature("Offering"), as.character.Offering)

#' @export
print.Offering <- function(x, ...) print(toString.Offering(x, ...))
setMethod("print", signature("Offering"), print.Offering)
setMethod("show", signature("Offering"), function(object) print.Offering(object))

#' @export
toString.Offering <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("Offering"), toString.Offering)

#' @export
as.character.Phenomenon <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Phenomenon", "character", function(from) as.character.Phenomenon(from))
setMethod("as.character", signature("Phenomenon"), as.character.Phenomenon)

#' @export
print.Phenomenon <- function(x, ...) print(toString.Phenomenon(x, ...))
setMethod("print", signature("Phenomenon"), print.Phenomenon)
setMethod("show", signature("Phenomenon"), function(object) print.Phenomenon(object))

#' @export
toString.Phenomenon <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("Phenomenon"), toString.Phenomenon)

#' @export
as.character.Procedure <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Procedure", "character", function(from) as.character.Procedure(from))
setMethod("as.character", signature("Procedure"), as.character.Procedure)

#' @export
print.Procedure <- function(x, ...) print(toString.Procedure(x, ...))
setMethod("print", signature("Procedure"), print.Procedure)
setMethod("show", signature("Procedure"), function(object) print.Procedure(object))

#' @export
toString.Procedure <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("Procedure"), toString.Procedure)

#' @export
as.character.ReferenceValue <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("ReferenceValue", "character", function(from) as.character.ReferenceValue(from))
setMethod("as.character", signature("ReferenceValue"), as.character.ReferenceValue)

#' @export
print.ReferenceValue <- function(x, ...) print(toString.ReferenceValue(x, ...))
setMethod("print", signature("ReferenceValue"), print.ReferenceValue)
setMethod("show", signature("ReferenceValue"), function(object) print.ReferenceValue(object))

#' @export
toString.ReferenceValue <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("ReferenceValue"), toString.ReferenceValue)

#' @export
as.character.Service <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Service", "character", function(from) as.character.Service(from))
setMethod("as.character", signature("Service"), as.character.Service)

#' @export
print.Service <- function(x, ...) print(toString.Service(x, ...))
setMethod("print", signature("Service"), print.Service)
setMethod("show", signature("Service"), function(object) print.Service(object))

#' @export
toString.Service <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("Service"), toString.Service)

#' @export
as.character.Station <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Station", "character", function(from) as.character.Station(from))
setMethod("as.character", signature("Station"), as.character.Station)

#' @export
print.Station <- function(x, ...) print(toString.Station(x, ...))
setMethod("print", signature("Station"), print.Station)
setMethod("show", signature("Station"), function(object) print.Station(object))

#' @export
toString.Station <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("Station"), toString.Station)

as.character.StatusInterval <- function(x, ...) {
    if (length(x) == 0) paste0(class(x),"()")
    else paste0(name(x), "[", lower(x), ", ", upper(x), "]: ", color(x))
}
setAs("StatusInterval", "character", function(from) as.character.StatusInterval(from))
setMethod("as.character", signature("StatusInterval"), as.character.StatusInterval)

#' @export
print.StatusInterval <- function(x, ...) print(toString.StatusInterval(x, ...))
setMethod("print", signature("StatusInterval"), print.StatusInterval)
setMethod("show", signature("StatusInterval"), function(object) print.StatusInterval(object))

#' @export
toString.StatusInterval <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("StatusInterval"), toString.StatusInterval)

#' @export
as.character.Timeseries <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Timeseries", "character", function(from) as.character.Timeseries(from))
setMethod("as.character", signature("Timeseries"), as.character.Timeseries)

#' @export
print.Timeseries <- function(x, ...) print(toString.Timeseries(x, ...))
setMethod("print", signature("Timeseries"), print.Timeseries)
setMethod("show", signature("Timeseries"), function(object) print.Timeseries(object))

#' @export
toString.Timeseries <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("Timeseries"), toString.Timeseries)

#' @export
as.character.TVP <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else paste0(time(x), ": ", value(x))
setAs("TVP", "character", function(from) as.character.TVP(from))
setMethod("as.character", signature("TVP"), as.character.TVP)

#' @export
print.TVP <- function(x, ...) print(toString.TVP(x, ...))
setMethod("print", signature("TVP"), print.TVP)
setMethod("show", signature("TVP"), function(object) print.TVP(object))

#' @export
toString.TVP <- function(x, ...) noquote(as.character(x))
setMethod("toString", signature("TVP"), toString.TVP)
