
#' @export
setGeneric("resourceURL", function(x) standardGeneric("resourceURL"))

#' @export
setGeneric("subresourceURL", function(x, ...) standardGeneric("subresourceURL"))

#' @export
setGeneric("stationsURL", function(x) standardGeneric("stationsURL"))

#' @export
setGeneric("servicesURL", function(x) standardGeneric("servicesURL"))

#' @export
setGeneric("timeseriesURL", function(x) standardGeneric("timeseriesURL"))

#' @export
setGeneric("categoriesURL", function(x) standardGeneric("categoriesURL"))

#' @export
setGeneric("offeringsURL", function(x) standardGeneric("offeringsURL"))

#' @export
setGeneric("featuresURL", function(x) standardGeneric("featuresURL"))

#' @export
setGeneric("proceduresURL", function(x) standardGeneric("proceduresURL"))

#' @export
setGeneric("phenomenaURL", function(x) standardGeneric("phenomenaURL"))

#' @export
setGeneric("getDataURL", function(x) standardGeneric("getDataURL"))

#' @export
setGeneric("stations", function(x, ...) standardGeneric("stations"))

#' @export
setGeneric("timeseries", function(x, ...) standardGeneric("timeseries"))

#' @export
setGeneric("services", function(x, ...) standardGeneric("services"))

#' @export
setGeneric("offerings", function(x, ...) standardGeneric("offerings"))

#' @export
setGeneric("procedures", function(x, ...) standardGeneric("procedures"))

#' @export
setGeneric("phenomena", function(x, ...) standardGeneric("phenomena"))

#' @export
setGeneric("categories", function(x, ...) standardGeneric("categories"))

#' @export
setGeneric("features", function(x, ...) standardGeneric("features"))

#' @export
setGeneric("id", function(x) standardGeneric("id"))

#' @export
setGeneric("label", function(x) standardGeneric("label"))

#' @export
setGeneric("endpoint", function(x) standardGeneric("endpoint"))

#' @export
setGeneric("getData", function(x, ...) standardGeneric("getData"))

#' @export
setGeneric("serviceURL", function(x) standardGeneric("serviceURL"))

#' @export
setGeneric("version", function(x) standardGeneric("version"))

#' @export
setGeneric("type", function(x) standardGeneric("type"))

#' @export
setGeneric("supportsFirstLatest", function(x) standardGeneric("supportsFirstLatest"))

#' @export
setGeneric("quantities", function(x) standardGeneric("quantities"))

#' @export
setGeneric("service", function(x) standardGeneric("service"))

#' @export
setGeneric("domainId", function(x) standardGeneric("domainId"))

#' @export
setGeneric("color", function(x) standardGeneric("color"))

#' @export
setGeneric("color<-", function(x, value) standardGeneric("color<-"))

#' @export
setGeneric("upper", function(x) standardGeneric("upper"))

#' @export
setGeneric("upper<-", function(x, value) standardGeneric("upper<-"))

#' @export
setGeneric("lower", function(x) standardGeneric("lower"))

#' @export
setGeneric("lower<-", function(x, value) standardGeneric("lower<-"))

#' @export
setGeneric("name", function(x) standardGeneric("name"))

#' @export
setGeneric("name<-", function(x, value) standardGeneric("name<-"))

#' @export
setGeneric("time", function(x) standardGeneric("time"))

#' @export
setGeneric("value", function(x) standardGeneric("value"))

#' @export
setGeneric("timeseries", function(x, ...) standardGeneric("timeseries"))

#' @export
setGeneric("feature", function(x) standardGeneric("feature"))

#' @export
setGeneric("offering", function(x) standardGeneric("offering"))

#' @export
setGeneric("procedure", function(x) standardGeneric("procedure"))

#' @export
setGeneric("station", function(x) standardGeneric("station"))

#' @export
setGeneric("uom", function(x) standardGeneric("uom"))

#' @export
setGeneric("firstValue", function(x) standardGeneric("firstValue"))

#' @export
setGeneric("firstValue<-", function(x, value) standardGeneric("firstValue<-"))

#' @export
setGeneric("lastValue", function(x) standardGeneric("lastValue"))

#' @export
setGeneric("lastValue<-", function(x, value) standardGeneric("lastValue<-"))

#' @export
setGeneric("statusIntervals", function(x) standardGeneric("statusIntervals"))

#' @export
setGeneric("category", function(x) standardGeneric("category"))

#' @export
setGeneric("phenomenon", function(x) standardGeneric("phenomenon"))

#' @export
setGeneric("fetch", function(x, ...) standardGeneric("fetch"))

#' @export
setGeneric("service<-", function(x, value) standardGeneric("service<-"))

#' @export
setGeneric("domainId<-", function(x, value) standardGeneric("domainId<-"))

#' @export
setGeneric("label<-", function(x, value) standardGeneric("label<-"))

#' @export
setGeneric("cache", function(x) standardGeneric("cache"))

#' @export
setGeneric("cache<-", function(x, value) standardGeneric("cache<-"))

#' @export
setGeneric("serviceURL<-", function(x, value) standardGeneric("serviceURL<-"))

#' @export
setGeneric("version<-", function(x, value) standardGeneric("version<-"))

#' @export
setGeneric("supportsFirstLatest<-", function(x, value) standardGeneric("supportsFirstLatest<-"))

#' @export
setGeneric("type<-", function(x, value) standardGeneric("type<-"))

#' @export
setGeneric("quantities<-", function(x, value) standardGeneric("quantities<-"))

#' @export
setGeneric("endpoint<-", function(x, value) standardGeneric("endpoint<-"))

#' @export
setGeneric("geometry<-", function (x, value) standardGeneric("geometry<-"))

#' @export
setGeneric("offering<-", function (x, value) standardGeneric("offering<-"))

#' @export
setGeneric("procedure<-", function (x, value) standardGeneric("procedure<-"))

#' @export
setGeneric("phenomenon<-", function (x, value) standardGeneric("phenomenon<-"))

#' @export
setGeneric("category<-", function (x, value) standardGeneric("category<-"))

#' @export
setGeneric("service<-", function (x, value) standardGeneric("service<-"))

#' @export
setGeneric("station<-", function (x, value) standardGeneric("station<-"))

#' @export
setGeneric("feature<-", function (x, value) standardGeneric("feature<-"))

#' @export
setGeneric("uom<-", function (x, value) standardGeneric("uom<-"))

#' @export
setGeneric("referenceValues", function(x) standardGeneric("referenceValues"))

#' @export
setGeneric("referenceValues<-", function(x, value) standardGeneric("referenceValues<-"))

concat <- function(x, ...) {
    if (nargs() < 3) rbind2(x, ...)
    else rbind2(x, Recall(...))
}
