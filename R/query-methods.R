#' @include virtual-class-api-resource.R
#' @include virtual-class-domain-resource.R
#' @include class-category.R
#' @include class-endpoint.R
#' @include class-feature.R
#' @include class-offering.R
#' @include class-phenomenon.R
#' @include class-procedure.R
#' @include class-service.R
#' @include class-station.R
#' @include class-timeseries.R
NULL

stop.if.no.query <- function(query) {
    f <- function(x) {
        is.null(x) || length(x) == 0
    }
    if (all(sapply(query, f)))
        stop("No filter query is given")
}

as.parameter.list <- function(x) {
    if (is.null(x) || length(x) == 0) NULL
    else paste(x, collapse=",")
}

#' Query Helper
#'
#' Transforms the parameters to a query list.
#'
#' @param service A \linkS4class{Service} or character identifier to filter with.
#' @param category A \linkS4class{Category} or character identifier to filter with.
#' @param phenomenon A \linkS4class{Phenomenon} or character identifier to filter with.
#' @param station A \linkS4class{Station} or character identifier to filter with.
#' @param timespan A \linkS4class{Interval} or a character vector to filter with.
#' @param crs A CRS identifier.
#' @param bbox A bouding box character vector.
#' @param near A point character vector.
#' @param locale A locale character vector
#' @keywords internal
as.query <- function(service = NULL, category = NULL, phenomenon = NULL,
                     crs = NULL, bbox = NULL, near = NULL, timespan = NULL,
                     station = NULL, locale = NULL) {
    # TODO check length of parameters
    query <- list(timespan = as.parameter.list(timespan),
                  service = as.parameter.list(service),
                  category = as.parameter.list(category),
                  phenomenon = as.parameter.list(phenomenon),
                  station = as.parameter.list(station),
                  crs = as.parameter.list(crs),
                  bbox = as.parameter.list(bbox),
                  near = as.parameter.list(near))
    query[!as.logical(lapply(query, is.null))]
}

format.interval <- function(x)
    paste(format(lubridate::with_tz(c(lubridate::int_start(x),
                                      lubridate::int_end(x)), "UTC"),
                 "%Y-%m-%dT%H:%M:%SZ"), collapse = '/')

as.timespan.parameter <- function(x, ...) {
    if (is.null(x) || is.na(x)) NULL
    else if (is.character(x)) x
    else if (is.interval(x)) format.interval(x)
    else x
}

as.logical.parameter <- function(x, ...) {
    if (is.null(x) || is.na(x)) NULL
    else ifelse(as.logical(x), "true", "false")
}

fetch.resource <- function(x, ...)
    fetch.resourceURL(resourceURL(x), ...)

as.query.string <- function(...) {
    query <- list(...)
    paste(names(query), query, collapse = "&", sep = "=")
}

fetch.resourceURL <- function(x, ...) {
    args <-  list(...)
    query <- if(is.null(args$query)) list()
    else do.call(as.query.string, args$query)

    tofetch <- unique(x)

    lapply(tofetch[!(paste(tofetch, query, sep="?") %in% get.cache.keys())],
           function(url) {
               key <- paste(url, query, sep="?")
               value <- get.json(url, ...)
               set.cache.value(key, value)
           })

    lapply(paste(x, query, sep="?"),
           function(url) get.cache.value(url))
}

get.json <- function(url, ...) {
    p <- list(...)
    q <- ifelse(!is.null(p$query), do.call(as.query.string, p$query), "")
    futile.logger::flog.debug("Requesting %s?%s", url, q)
    response <- httr::GET(url, httr::add_headers(Accept="application/json"), ...)
    httr::stop_for_status(response)
    jsonlite::fromJSON(httr::content(response, "text"))
}

get.and.parse <- function(endpoint, query, fun.url, fun.parse)
    fun.parse(endpoint, get.json(fun.url(endpoint), query = query))


path <- function(x, path, ...) {
    x <- lapply(x, "[[", path)
    if (nargs() == 2) x else Recall(x, ...)
}

simplify.list <- function(x, path) {
    if (!missing(path)) x <- lapply(x, "[[", path)
    do.call(mapply, c(list(FUN=c, SIMPLIFY=F), x))
}

#' @rdname query-methods
#' @inheritParams as.query
setMethod("stations",
          signature(x = "Endpoint"),
          function(x, ...)
              get.and.parse(x, as.query(...), stationsURL, Station.parse))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("phenomena",
          signature(x = "Endpoint"),
          function(x, ...)
              get.and.parse(x, as.query(...), phenomenaURL, Phenomenon.parse))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("categories",
          signature(x = "Endpoint"),
          function(x, ...)
              get.and.parse(x, as.query(...), categoriesURL, Category.parse))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("features",
          signature(x = "Endpoint"),
          function(x, ...)
              get.and.parse(x, as.query(...), featuresURL, Feature.parse))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("offerings",
          signature(x = "Endpoint"),
          function(x, ...)
              get.and.parse(x, as.query(...), offeringsURL, Offering.parse))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("procedures",
          signature(x = "Endpoint"),
          function(x, ...)
              get.and.parse(x, as.query(...), proceduresURL, Procedure.parse))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("timeseries",
          signature(x = "Endpoint"),
          function(x, ...)
              get.and.parse(x, as.query(...), timeseriesURL, Timeseries.parse))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("services",
          signature(x = "Endpoint"),
          function(x, ...)
              get.and.parse(x, as.query(...), servicesURL, Service.parse))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("stations",
          signature(x = "Service"),
          function(x, ...)
              stations(endpoint(x), service = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("phenomena",
          signature(x = "Service"),
          function(x, ...)
              phenomena(endpoint(x), service = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("features",
          signature(x = "Service"),
          function(x, ...)
              features(endpoint(x), service = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("categories",
          signature(x = "Service"),
          function(x, ...)
              categories(endpoint(x), service = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("timeseries",
          signature(x = "Service"),
          function(x, ...)
              timeseries(endpoint(x), service = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("offerings",
          signature(x = "Service"),
          function(x, ...)
              offerings(endpoint(x), service = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("procedures",
          signature(x = "Service"),
          function(x, ...)
              procedures(endpoint(x), service = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("services",
          signature(x = "Station"),
          function(x, ...)
              services(endpoint(x), station = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("phenomena",
          signature(x = "Station"),
          function(x, ...)
              phenomena(endpoint(x), station = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("features",
          signature(x = "Station"),
          function(x, ...)
              features(endpoint(x), station = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("categories",
          signature(x = "Station"),
          function(x, ...)
              categories(endpoint(x), station = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("timeseries",
          signature(x = "Station"),
          function(x, ...)
              timeseries(endpoint(x), station = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("offerings",
          signature(x = "Station"),
          function(x, ...)
              offerings(endpoint(x), station = x, ...))

#' @rdname query-methods
#' @inheritParams as.query
setMethod("procedures",
          signature(x = "Station"),
          function(x, ...)
              procedures(endpoint(x), station = x, ...))

#' @rdname fetch-methods
setMethod("fetch",
          signature(x = "Service"),
          function(x, ...) {
              tmp <- fetch.resource(x)
              fetched <- simplify.list(tmp)
              label(x) <- fetched$label
              serviceURL(x) <- fetched$serviceUrl
              version(x) <- fetched$version
              type(x) <- fetched$type
              supportsFirstLatest(x) <- fetched$supportsFirstLatest
              quantities(x) <- as.data.frame(t(sapply(tmp, "[[", "quantities")))
              x
          })

#' @rdname fetch-methods
setMethod("fetch",
          signature(x = "DomainResource"),
          function(x, ...) {
              tmp <- fetch.resource(x)
              fetched <- simplify.list(tmp)
              service <- Service(endpoint = endpoint(x),
                                 id = simplify.list(tmp, "service")$id)
              label(x) <- fetched$label
              domainId(x) <- fetched$domainId
              service(x) <- fetch(service)
              x
          })

#' @rdname fetch-methods
setMethod("fetch",
          signature(x = "Category"),
          function(x, ...) {
              tmp <- fetch.resource(x)
              fetched <- simplify.list(tmp)
              service <- Service(endpoint = endpoint(x),
                                 id = simplify.list(tmp, "service")$id)
              label(x) <- fetched$label
              service(x) <- fetch(service)
              x
          })

#' @rdname fetch-methods
setMethod("fetch",
          signature(x = "Station"),
          function(x, ...) {
              tmp <- fetch.resource(x)
              props <- simplify.list(tmp, "properties")
              label(x) <- props$label
              geometries <- lapply(tmp, "[[", "geometry")
              coordinates <- lapply(geometries, "[[", "coordinates")
              geometry(x) <- SpatialPoints.parse(list(coordinates = coordinates))
              x
          })

#' @rdname fetch-methods
setMethod("fetch",
          signature(x = "Timeseries"),
          function(x, ...) {
              tmp <- fetch.resource(x)

              label(x) <- as.character(path(tmp, "label"))
              uom(x) <- as.character(path(tmp, "uom"))

              station(x) <- fetch(Station(id = as.character(path(tmp, "station", "properties", "id")), endpoint = endpoint(x)))
              service(x) <- fetch(Service(id = as.character(path(tmp, "parameters", "service", "id")), endpoint = endpoint(x)))
              offering(x) <- fetch(Offering(id = as.character(path(tmp, "parameters", "offering", "id")), endpoint = endpoint(x)))
              #feature(x) <- fetch(Feature(id = as.character(path(tmp, "parameters", "feature", "id")), endpoint = endpoint(x)))
              procedure(x) <- fetch(Procedure(id = as.character(path(tmp, "parameters", "procedure", "id")), endpoint = endpoint(x)))
              phenomenon(x) <- fetch(Phenomenon(id = as.character(path(tmp, "parameters", "phenomenon", "id")), endpoint = endpoint(x)))
              category(x) <- fetch(Category(id = as.character(path(tmp, "parameters", "category", "id")), endpoint = endpoint(x)))

              list.as.numeric <- function(x)
                  sapply(x, function(x)
                      if(is.null(x)) NA else x)

              list.as.tvp <- function(x) {
                  ts <- list.as.numeric(path(x, "timestamp"))
                  v <- list.as.numeric(path(x, "value"))
                  values <- list(timestamp = ts, value = v)
                  TimeseriesData.parse(list(values=values))
              }

              list.as.rv <- function(x) {

              }

              firstValue(x) <- list.as.tvp(path(tmp, "firstValue"))
              lastValue(x) <- list.as.tvp(path(tmp, "lastValue"))

              # TODO reference value
              # TODO statusInterval

              x
          })

#' @rdname query-methods
setMethod("getData",
          signature(x = "Timeseries"),
          function(x,
                   generalize = FALSE,
                   timespan = NULL, ...) {
              query <- list(generalize = as.logical.parameter(generalize),
                            timespan = as.timespan.parameter(timespan))
              tmp <- fetch.resourceURL(getDataURL(x), query=query)
              lapply(tmp, TimeseriesData.parse)
          });
