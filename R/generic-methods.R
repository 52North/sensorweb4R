#' @name rbind2-methods
#' @title rbind2 Methods
#' @description \code{rbind2} combines to objects into one.
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname rbind2-methods
NULL

#' @name rep-methods
#' @title rep Methods
#' @description S4 generic version of \code{rep}.
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname rep-methods
NULL


#' @title URL Methods
#' @description Gets the URL of a resource.
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname url-methods
#' @name url-methods
#' @docType methods
NULL

#' @name length-methods
#' @title length Methods
#' @description Gets the length/count of an resource.
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname length-methods
NULL

#' @name string-methods
#' @title String Methods
#' @description Methods and related functions to coerce objects from or to characters.
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname string-methods
NULL

#' @name subset-methods
#' @title Extraction and Replacement Methods
#' @description Methods for extraction and replacement.
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname subset-methods
NULL

#' @details \code{resourceURL} gets the URL of the resource \code{x}.
#' @export
#' @rdname url-methods
setGeneric("resourceURL", function(x) standardGeneric("resourceURL"))

#' @details \code{subresourceURL} gets the URL of a sub-resource of \code{x}.
#' @export
#' @rdname url-methods
setGeneric("subresourceURL", function(x, ...) standardGeneric("subresourceURL"))

#' @details \code{stationsURL} gets the URL of a \linkS4class{Station} collection associated with \code{x}.
#' @export
#' @rdname url-methods
setGeneric("stationsURL", function(x) standardGeneric("stationsURL"))

#' @details \code{servicesURL} gets the URL of a \linkS4class{Service} collection associated with \code{x}.
#' @export
#' @rdname url-methods
setGeneric("servicesURL", function(x) standardGeneric("servicesURL"))

#' @details \code{timeseriesURL} gets the URL of a \linkS4class{Timeseries} collection associated with \code{x}.
#' @export
#' @rdname url-methods
setGeneric("timeseriesURL", function(x) standardGeneric("timeseriesURL"))

#' @details \code{categoriesURL} gets the URL of a \linkS4class{Category} collection associated with \code{x}.
#' @export
#' @rdname url-methods
setGeneric("categoriesURL", function(x) standardGeneric("categoriesURL"))

#' @details \code{offeringsURL} gets the URL of a \linkS4class{Offering} collection associated with \code{x}.
#' @export
#' @rdname url-methods
setGeneric("offeringsURL", function(x) standardGeneric("offeringsURL"))

#' @details \code{featuresURL} gets the URL of a \linkS4class{Feature} collection associated with \code{x}.
#' @export
#' @rdname url-methods
setGeneric("featuresURL", function(x) standardGeneric("featuresURL"))

#' @details \code{proceduresURL} gets the URL of a \linkS4class{Procedure} collection associated with \code{x}.
#' @export
#' @rdname url-methods
setGeneric("proceduresURL", function(x) standardGeneric("proceduresURL"))

#' @details \code{phenomenaURL} gets the URL of a \linkS4class{Phenomenon} collection associated with \code{x}.
#' @export
#' @rdname url-methods
setGeneric("phenomenaURL", function(x) standardGeneric("phenomenaURL"))

#' @details \code{phenomenaURL} gets the URL of the \code{getData} endpoint associated with \code{x}.
#' @export
#' @rdname url-methods
setGeneric("getDataURL", function(x) standardGeneric("getDataURL"))

#' @name query-methods
#' @title Query Methods
#' @description Queries an \linkS4class{Endpoint}.
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname query-methods
NULL

#' @export
#' @rdname query-methods
setGeneric("stations", function(x, ...) standardGeneric("stations"))

#' @export
#' @rdname query-methods
setGeneric("timeseries", function(x, ...) standardGeneric("timeseries"))

#' @export
#' @rdname query-methods
setGeneric("services", function(x, ...) standardGeneric("services"))

#' @export
#' @rdname query-methods
setGeneric("offerings", function(x, ...) standardGeneric("offerings"))

#' @export
#' @rdname query-methods
setGeneric("procedures", function(x, ...) standardGeneric("procedures"))

#' @export
#' @rdname query-methods
setGeneric("phenomena", function(x, ...) standardGeneric("phenomena"))

#' @export
#' @rdname query-methods
setGeneric("categories", function(x, ...) standardGeneric("categories"))

#' @export
#' @rdname query-methods
setGeneric("features", function(x, ...) standardGeneric("features"))

#' @export
#' @rdname query-methods
setGeneric("getData", function(x, ...) standardGeneric("getData"))


#' @name accessor-methods
#' @title Accessor Methods
#' @description Getter and setters for slots.
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname accessor-methods
NULL

#' @export
#' @rdname accessor-methods
setGeneric("id", function(x) standardGeneric("id"))

#' @export
#' @rdname accessor-methods
setGeneric("label", function(x) standardGeneric("label"))

#' @export
#' @rdname accessor-methods
setGeneric("label<-", function(x, value) standardGeneric("label<-"))

#' @export
#' @rdname accessor-methods
setGeneric("endpoint", function(x) standardGeneric("endpoint"))

#' @export
#' @rdname accessor-methods
setGeneric("endpoint<-", function(x, value) standardGeneric("endpoint<-"))

#' @export
#' @rdname accessor-methods
setGeneric("serviceURL", function(x) standardGeneric("serviceURL"))

#' @export
#' @rdname accessor-methods
setGeneric("serviceURL<-", function(x, value) standardGeneric("serviceURL<-"))

#' @export
#' @rdname accessor-methods
setGeneric("version", function(x) standardGeneric("version"))

#' @export
#' @rdname accessor-methods
setGeneric("version<-", function(x, value) standardGeneric("version<-"))

#' @export
#' @rdname accessor-methods
setGeneric("type", function(x) standardGeneric("type"))

#' @export
#' @rdname accessor-methods
setGeneric("type<-", function(x, value) standardGeneric("type<-"))

#' @export
#' @rdname accessor-methods
setGeneric("supportsFirstLatest", function(x) standardGeneric("supportsFirstLatest"))

#' @export
#' @rdname accessor-methods
setGeneric("supportsFirstLatest<-", function(x, value) standardGeneric("supportsFirstLatest<-"))

#' @export
#' @rdname accessor-methods
setGeneric("quantities", function(x) standardGeneric("quantities"))

#' @export
#' @rdname accessor-methods
setGeneric("quantities<-", function(x, value) standardGeneric("quantities<-"))

#' @export
#' @rdname accessor-methods
setGeneric("domainId", function(x) standardGeneric("domainId"))

#' @export
#' @rdname accessor-methods
setGeneric("domainId<-", function(x, value) standardGeneric("domainId<-"))

#' @export
#' @rdname accessor-methods
setGeneric("color", function(x) standardGeneric("color"))

#' @export
#' @rdname accessor-methods
setGeneric("color<-", function(x, value) standardGeneric("color<-"))

#' @export
#' @rdname accessor-methods
setGeneric("upper", function(x) standardGeneric("upper"))

#' @export
#' @rdname accessor-methods
setGeneric("upper<-", function(x, value) standardGeneric("upper<-"))

#' @export
#' @rdname accessor-methods
setGeneric("lower", function(x) standardGeneric("lower"))

#' @export
#' @rdname accessor-methods
setGeneric("lower<-", function(x, value) standardGeneric("lower<-"))

#' @export
#' @rdname accessor-methods
setGeneric("name", function(x) standardGeneric("name"))

#' @export
#' @rdname accessor-methods
setGeneric("name<-", function(x, value) standardGeneric("name<-"))

#' @export
#' @rdname accessor-methods
setGeneric("time", function(x) standardGeneric("time"))

#' @export
#' @rdname accessor-methods
setGeneric("value", function(x) standardGeneric("value"))

#' @export
#' @rdname accessor-methods
setGeneric("uom", function(x) standardGeneric("uom"))

#' @export
#' @rdname accessor-methods
setGeneric("uom<-", function (x, value) standardGeneric("uom<-"))

#' @export
#' @rdname accessor-methods
setGeneric("firstValue", function(x) standardGeneric("firstValue"))

#' @export
#' @rdname accessor-methods
setGeneric("firstValue<-", function(x, value) standardGeneric("firstValue<-"))

#' @export
#' @rdname accessor-methods
setGeneric("lastValue", function(x) standardGeneric("lastValue"))

#' @export
#' @rdname accessor-methods
setGeneric("lastValue<-", function(x, value) standardGeneric("lastValue<-"))

#' @export
#' @rdname accessor-methods
setGeneric("statusIntervals", function(x) standardGeneric("statusIntervals"))

#' @export
#' @rdname accessor-methods
setGeneric("referenceValues", function(x) standardGeneric("referenceValues"))

#' @export
#' @rdname accessor-methods
setGeneric("referenceValues<-", function(x, value) standardGeneric("referenceValues<-"))

#' @export
#' @rdname accessor-methods
setGeneric("geometry<-", function (x, value) standardGeneric("geometry<-"))

#' @name api-relations
#' @title API Relation Getters and Setters
#' @description #' Common getter and setter for relations between API resources.
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname api-relations
NULL

#' @details
#' \code{service} gets the \linkS4class{Service} of the resource.
#' @export
#' @rdname api-relations
setGeneric("service", function(x)
    standardGeneric("service"))

#' @details
#' \code{procedure<-} sets the \linkS4class{Procedure} of the resource.
#' @export
#' @rdname api-relations
setGeneric("service<-", function (x, value)
    standardGeneric("service<-"))

#' @details
#' \code{feature} gets the \linkS4class{Feature} of the resource.
#' @export
#' @rdname api-relations
setGeneric("feature", function(x)
    standardGeneric("feature"))

#' @details
#' \code{feature<-} sets the \linkS4class{Feature} of the resource.
#' @export
#' @rdname api-relations
setGeneric("feature<-", function (x, value)
    standardGeneric("feature<-"))

#' @details
#' \code{offering} gets the \linkS4class{Offering} of the resource.
#' @export
#' @rdname api-relations
setGeneric("offering", function(x)
    standardGeneric("offering"))

#' @details
#' \code{offering<-} sets the \linkS4class{Offering} of the resource.
#' @export
#' @rdname api-relations
setGeneric("offering<-", function (x, value)
    standardGeneric("offering<-"))

#' @details
#' \code{procedure} gets the \linkS4class{Procedure} of the resource.
#' @export
#' @rdname api-relations
setGeneric("procedure", function(x)
    standardGeneric("procedure"))

#' @details
#' \code{procedure<-} sets the \linkS4class{Procedure} of the resource.
#' @export
#' @rdname api-relations
setGeneric("procedure<-", function (x, value)
    standardGeneric("procedure<-"))

#' @details
#' \code{station} gets the \linkS4class{Station} of the resource.
#' @export
#' @rdname api-relations
setGeneric("station", function(x)
    standardGeneric("station"))

#' @details
#' \code{station<-} sets the \linkS4class{Station} of the resource.
#' @export
#' @rdname api-relations
setGeneric("station<-", function (x, value)
    standardGeneric("station<-"))

#' @details
#' \code{category} gets the \linkS4class{Category} of the resource.
#' @export
#' @rdname api-relations
setGeneric("category", function(x)
    standardGeneric("category"))

#' @details
#' \code{category<-} sets the \linkS4class{Category} of the resource.
#' @export
#' @rdname api-relations
setGeneric("category<-", function (x, value)
    standardGeneric("category<-"))

#' @details
#' \code{phenomenon} gets the \linkS4class{Phenomenon} of the resource.
#' @export
#' @rdname api-relations
setGeneric("phenomenon", function(x)
    standardGeneric("phenomenon"))


#' @details
#' \code{phenomenon<-} sets the \linkS4class{Phenomenon} of the resource.
#' @export
#' @rdname api-relations
setGeneric("phenomenon<-", function (x, value)
    standardGeneric("phenomenon<-"))

#' @title Fetch methods
#' @description \code{fetch} fetches recursively all metadata for the supplied API resource.
#' @param x the API resource to fetch
#' @param ... Further arguments that may be needed by the actual implementation
#' @return an object of the same class as \code{x} with all meta data added
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @export
#' @aliases fetch
#' @name fetch-methods
#' @rdname fetch-methods
#' @examples
#' \dontrun{
#' fetch(Timeseries(id = ids, endpoint = Endpoint(e)))
#' }
setGeneric("fetch", function(x, ...) standardGeneric("fetch"))

concat <- function(x, ...) {
    if (nargs() < 3) rbind2(x, ...)
    else rbind2(x, Recall(...))
}

concat.list <- function(list) {
    do.call(concat, list)
}
