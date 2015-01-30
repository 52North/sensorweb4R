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
#' @method as.character ApiResource
#' @rdname string-methods
as.character.ApiResource <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("ApiResource", "character", function(from) as.character.ApiResource(from))
#' @rdname string-methods
setMethod("as.character", signature("ApiResource"), as.character.ApiResource)


#' @export
#' @rdname string-methods
print.ApiResource <- function(x, ...) print(toString.ApiResource(x, ...))
#' @rdname string-methods
setMethod("print", signature("ApiResource"), print.ApiResource)
#' @rdname string-methods
setMethod("show", signature("ApiResource"), function(object) print.ApiResource(object))

#' @export
#' @rdname string-methods
toString.ApiResource <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("ApiResource"), toString.ApiResource)

#' @export
#' @rdname string-methods
as.character.Category <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Category", "character", function(from) as.character.Category(from))
#' @rdname string-methods
setMethod("as.character", signature("Category"), as.character.Category)

#' @export
#' @rdname string-methods
print.Category <- function(x, ...) print(toString.Category(x, ...))
#' @rdname string-methods
setMethod("print", signature("Category"), print.Category)
#' @rdname string-methods
setMethod("show", signature("Category"), function(object) print.Category(object))

#' @export
#' @rdname string-methods
toString.Category <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("Category"), toString.Category)

#' @export
#' @rdname string-methods
as.character.DomainResource <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("DomainResource", "character", function(from) as.character.DomainResource(from))
#' @rdname string-methods
setMethod("as.character", signature("DomainResource"), as.character.DomainResource)

#' @export
#' @rdname string-methods
print.DomainResource <- function(x, ...) print(toString.DomainResource(x, ...))
#' @rdname string-methods
setMethod("print", signature("DomainResource"), print.DomainResource)
#' @rdname string-methods
setMethod("show", signature("DomainResource"), function(object) print.DomainResource(object))

#' @export
#' @rdname string-methods
toString.DomainResource <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("DomainResource"), toString.DomainResource)

#' @export
#' @rdname string-methods
as.character.Endpoint <- function(x, ...) noquote(resourceURL(x))
setAs("Endpoint", "character", function(from) as.character.Endpoint(from))
#' @rdname string-methods
setMethod("as.character", signature("Endpoint"), as.character.Endpoint)

#' @export
#' @rdname string-methods
print.Endpoint <- function(x, ...) print(toString.Endpoint(x, ...))
#' @rdname string-methods
setMethod("print", signature("Endpoint"), print.Endpoint)
#' @rdname string-methods
setMethod("show", signature("Endpoint"), function(object) print.Endpoint(object))

#' @export
#' @rdname string-methods
toString.Endpoint <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("Endpoint"), toString.Endpoint)

#' @export
#' @rdname string-methods
as.character.Feature <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Feature", "character", function(from) as.character.Feature(from))
#' @rdname string-methods
setMethod("as.character", signature("Feature"), as.character.Feature)

#' @export
#' @rdname string-methods
print.Feature <- function(x, ...) print(toString.Feature(x, ...))
#' @rdname string-methods
setMethod("print", signature("Feature"), print.Feature)
#' @rdname string-methods
setMethod("show", signature("Feature"), function(object) print.Feature(object))

#' @export
#' @rdname string-methods
toString.Feature <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("Feature"), toString.Feature)

#' @export
#' @rdname string-methods
as.character.HttpResource <- function(x, ...) resourceURL(x)
setAs("HttpResource", "character", function(from) as.character.HttpResource(from))
#' @rdname string-methods
setMethod("as.character", signature("HttpResource"), as.character.HttpResource)

#' @export
#' @rdname string-methods
print.HttpResource <- function(x, ...) print(toString.HttpResource(x, ...))
#' @rdname string-methods
setMethod("print", signature("HttpResource"), print.HttpResource)
#' @rdname string-methods
setMethod("show", signature("HttpResource"), function(object) print.HttpResource(object))

#' @export
#' @rdname string-methods
toString.HttpResource <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("HttpResource"), toString.HttpResource)

#' @export
#' @rdname string-methods
as.character.Offering <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Offering", "character", function(from) as.character.Offering(from))
#' @rdname string-methods
setMethod("as.character", signature("Offering"), as.character.Offering)

#' @export
#' @rdname string-methods
print.Offering <- function(x, ...) print(toString.Offering(x, ...))
#' @rdname string-methods
setMethod("print", signature("Offering"), print.Offering)
#' @rdname string-methods
setMethod("show", signature("Offering"), function(object) print.Offering(object))

#' @export
#' @rdname string-methods
toString.Offering <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("Offering"), toString.Offering)

#' @export
#' @rdname string-methods
as.character.Phenomenon <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Phenomenon", "character", function(from) as.character.Phenomenon(from))
#' @rdname string-methods
setMethod("as.character", signature("Phenomenon"), as.character.Phenomenon)

#' @export
#' @rdname string-methods
print.Phenomenon <- function(x, ...) print(toString.Phenomenon(x, ...))
#' @rdname string-methods
setMethod("print", signature("Phenomenon"), print.Phenomenon)
#' @rdname string-methods
setMethod("show", signature("Phenomenon"), function(object) print.Phenomenon(object))

#' @export
#' @rdname string-methods
toString.Phenomenon <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("Phenomenon"), toString.Phenomenon)

#' @export
#' @rdname string-methods
as.character.Procedure <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Procedure", "character", function(from) as.character.Procedure(from))
#' @rdname string-methods
setMethod("as.character", signature("Procedure"), as.character.Procedure)

#' @export
#' @rdname string-methods
print.Procedure <- function(x, ...) print(toString.Procedure(x, ...))
#' @rdname string-methods
setMethod("print", signature("Procedure"), print.Procedure)
#' @rdname string-methods
setMethod("show", signature("Procedure"), function(object) print.Procedure(object))

#' @export
#' @rdname string-methods
toString.Procedure <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("Procedure"), toString.Procedure)

#' @export
#' @rdname string-methods
as.character.ReferenceValue <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("ReferenceValue", "character", function(from) as.character.ReferenceValue(from))
#' @rdname string-methods
setMethod("as.character", signature("ReferenceValue"), as.character.ReferenceValue)

#' @export
#' @rdname string-methods
print.ReferenceValue <- function(x, ...) print(toString.ReferenceValue(x, ...))
#' @rdname string-methods
setMethod("print", signature("ReferenceValue"), print.ReferenceValue)
#' @rdname string-methods
setMethod("show", signature("ReferenceValue"), function(object) print.ReferenceValue(object))

#' @export
#' @rdname string-methods
toString.ReferenceValue <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("ReferenceValue"), toString.ReferenceValue)

#' @export
#' @rdname string-methods
as.character.Service <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Service", "character", function(from) as.character.Service(from))
#' @rdname string-methods
setMethod("as.character", signature("Service"), as.character.Service)

#' @export
#' @rdname string-methods
print.Service <- function(x, ...) print(toString.Service(x, ...))
#' @rdname string-methods
setMethod("print", signature("Service"), print.Service)
#' @rdname string-methods
setMethod("show", signature("Service"), function(object) print.Service(object))

#' @export
#' @rdname string-methods
toString.Service <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("Service"), toString.Service)

#' @export
#' @rdname string-methods
as.character.Station <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Station", "character", function(from) as.character.Station(from))
#' @rdname string-methods
setMethod("as.character", signature("Station"), as.character.Station)

#' @export
#' @rdname string-methods
print.Station <- function(x, ...) print(toString.Station(x, ...))
#' @rdname string-methods
setMethod("print", signature("Station"), print.Station)
#' @rdname string-methods
setMethod("show", signature("Station"), function(object) print.Station(object))

#' @export
#' @rdname string-methods
toString.Station <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("Station"), toString.Station)

#' @export
#' @rdname string-methods
as.character.StatusInterval <- function(x, ...) {
    if (length(x) == 0) paste0(class(x),"()")
    else paste0(name(x), "[", lower(x), ", ", upper(x), "]: ", color(x))
}
setAs("StatusInterval", "character", function(from) as.character.StatusInterval(from))
#' @rdname string-methods
setMethod("as.character", signature("StatusInterval"), as.character.StatusInterval)

#' @export
#' @rdname string-methods
print.StatusInterval <- function(x, ...) print(toString.StatusInterval(x, ...))
#' @rdname string-methods
setMethod("print", signature("StatusInterval"), print.StatusInterval)
#' @rdname string-methods
setMethod("show", signature("StatusInterval"), function(object) print.StatusInterval(object))

#' @export
#' @rdname string-methods
toString.StatusInterval <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("StatusInterval"), toString.StatusInterval)

#' @export
#' @rdname string-methods
as.character.Timeseries <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else id(x)
setAs("Timeseries", "character", function(from) as.character.Timeseries(from))
#' @rdname string-methods
setMethod("as.character", signature("Timeseries"), as.character.Timeseries)

#' @export
#' @rdname string-methods
print.Timeseries <- function(x, ...) print(toString.Timeseries(x, ...))
#' @rdname string-methods
setMethod("print", signature("Timeseries"), print.Timeseries)
#' @rdname string-methods
setMethod("show", signature("Timeseries"), function(object) print.Timeseries(object))

#' @export
#' @rdname string-methods
toString.Timeseries <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("Timeseries"), toString.Timeseries)

#' @export
#' @rdname string-methods
as.character.TVP <- function(x, ...) if (length(x) == 0) paste0(class(x),"()") else paste0(time(x), ": ", value(x))
setAs("TVP", "character", function(from) as.character.TVP(from))
#' @rdname string-methods
setMethod("as.character", signature("TVP"), as.character.TVP)

#' @export
#' @rdname string-methods
print.TVP <- function(x, ...) print(toString.TVP(x, ...))
#' @rdname string-methods
setMethod("print", signature("TVP"), print.TVP)
#' @rdname string-methods
setMethod("show", signature("TVP"), function(object) print.TVP(object))

#' @export
#' @rdname string-methods
toString.TVP <- function(x, ...) noquote(as.character(x))
#' @rdname string-methods
setMethod("toString", signature("TVP"), toString.TVP)
