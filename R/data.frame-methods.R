#' @include virtual-class-api-resource.R
#' @include class-category.R
#' @include virtual-class-domain-resource.R
#' @include class-endpoint.R
#' @include virtual-class-http-resource.R
#' @include class-reference-value.R
#' @include class-service.R
#' @include class-station.R
#' @include class-status-interval.R
#' @include class-timeseries.R
#' @include class-tvp.R
NULL


#' data.frame coercion
#'
#' Methods to coerce to a  \code{data.frame}.
#'
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname data.frame-methods
#' @name data.frame-methods
NULL


#' @export
#' @rdname data.frame-methods
as.data.frame.ApiResource <- function(x, ...)
	data.frame(endpoint = endpoint(x),
		       id = id(x),
		       label = label(x))
setAs("ApiResource", "data.frame", function(from) as.data.frame.ApiResource(from))
setMethod("as.data.frame", signature(x = "ApiResource"), as.data.frame.ApiResource)


#' @export
#' @rdname data.frame-methods
as.data.frame.Category <- function(x, ...)
	data.frame(endpoint = endpoint(x),
		       id = id(x),
		       label = label(x),
		       service = service(x))
setAs("Category", "data.frame", function(from) as.data.frame.Category(from))
#' @rdname data.frame-methods
setMethod("as.data.frame", signature(x = "Category"), as.data.frame.Category)


#' @export
#' @rdname data.frame-methods
as.data.frame.DomainResource <- function(x, ...)
	data.frame(endpoint = endpoint(x),
	           id = id(x),
	           label = label(x),
	           service = service(x),
	           domainId = domainId(x))
setAs("DomainResource", "data.frame", function(from) as.data.frame.DomainResource(from))
#' @rdname data.frame-methods
setMethod("as.data.frame", signature(x = "DomainResource"), as.data.frame.DomainResource)


#' @export
#' @rdname data.frame-methods
as.data.frame.Endpoint <- function(x, ...)
	data.frame(url = resourceURL(x))
setAs("Endpoint", "data.frame", function(from) as.data.frame.Endpoint(from))
#' @rdname data.frame-methods
setMethod("as.data.frame", signature(x = "Endpoint"), as.data.frame.Endpoint)


#' @export
#' @rdname data.frame-methods
as.data.frame.HttpResource <- function(x, ...)
	data.frame(url = resourceURL(x))
setAs("HttpResource", "data.frame", function(from) as.data.frame.HttpResource(from))
#' @rdname data.frame-methods
setMethod("as.data.frame", signature(x = "HttpResource"), as.data.frame.HttpResource)


#' @export
#' @rdname data.frame-methods
as.data.frame.ReferenceValue <- function(x, ...)
	data.frame(endpoint = endpoint(x),
		       id = id(x),
		       label = label(x),
		       time = time(x),
		       value = value(x))
setAs("ReferenceValue", "data.frame", function(from) as.data.frame.ReferenceValue(from))
#' @rdname data.frame-methods
setMethod("as.data.frame", signature(x = "ReferenceValue"), as.data.frame.ReferenceValue)


#' @export
#' @rdname data.frame-methods
as.data.frame.Service <- function(x, ...)
    data.frame(endpoint = endpoint(x),
    	       id = id(x),
               label = label(x),
               serviceURL = serviceURL(x),
               version = version(x),
               type = type(x),
               supportsFirstLatest = supportsFirstLatest(x),
               quantities = quantities(x))
setAs("Service", "data.frame", function(from) as.data.frame.Service(from))
#' @rdname data.frame-methods
setMethod("as.data.frame", signature(x = "Service"), as.data.frame.Service)


#' @export
#' @rdname data.frame-methods
as.data.frame.Station <- function(x, ...)
	data.frame(endpoint = endpoint(x),
               id = id(x),
               label = label(x),
               geometry = geometry(x))
setAs("Station", "data.frame", function(from) as.data.frame.Station(from))
#' @rdname data.frame-methods
setMethod("as.data.frame", signature(x = "Station"), as.data.frame.Station)


#' @export
#' @rdname data.frame-methods
as.data.frame.StatusInterval <- function(x, ...)
	data.frame(lower = lower(x),
		       upper = upper(x),
		       color = color(x),
		       name = name(x))
setAs("StatusInterval", "data.frame", function(from) as.data.frame.StatusInterval(from))
#' @rdname data.frame-methods
setMethod("as.data.frame", signature(x = "StatusInterval"), as.data.frame.StatusInterval)


#' @export
#' @rdname data.frame-methods
as.data.frame.Timeseries <- function(x, ...)
    data.frame(endpoint = endpoint(x),
               id = id(x),
               label = label(x),
               uom = uom(x),
               phenomenon = phenomenon(x),
               service = service(x),
               category = category(x),
               feature = feature(x),
               offering = offering(x),
               procedure = procedure(x),
               station = station(x),
               firstValue = firstValue(x),
               lastValue = lastValue(x))
setAs("Timeseries", "data.frame", function(from) as.data.frame.Timeseries(from))
#' @rdname data.frame-methods
setMethod("as.data.frame", signature(x = "Timeseries"), as.data.frame.Timeseries)


#' @export
#' @rdname data.frame-methods
as.data.frame.TVP <- function(x, ...)
	data.frame(time = time(x),
               value = value(x))
setAs("TVP", "data.frame", function(from) as.data.frame.TVP(from))
#' @rdname data.frame-methods
setMethod("as.data.frame", signature(x = "TVP"), as.data.frame.TVP)
