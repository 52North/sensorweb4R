

as.query <- function(service = NULL,
                     category = NULL,
                     phenomenon = NULL,
                     crs = NULL,
                     bbox = NULL,
                     near = NULL,
                     timespan = NULL,
                     station = NULL,
                     locale = NULL) {
    list(timespan = .as.parameter.list(timespan),
         service = .as.parameter.list(service),
         category = .as.parameter.list(category),
         phenomenon = .as.parameter.list(phenomenon),
         station = .as.parameter.list(station),
         crs = .as.parameter.list(crs),
         bbox = .as.parameter.list(bbox),
         near = .as.parameter.list(near))
}

setMethod("stations",
          signature(x = "Endpoint"),
          function(x, ...) {
              query <- as.query(...)
              futile.logger::flog.debug(toString(query))
              .get_and_parse(x, query, stationsURL, Station.parse)
          })

setMethod("timeseries",
          signature(x = "Endpoint"),
          function(x, ...) {
              query <- as.query(...)
              .get_and_parse(x, query, timeseriesURL, Timeseries.parse)
          })

setMethod("services",
          signature(x = "Endpoint"),
          function(x, ...) {
              query <- as.query(...)
              .get_and_parse(x, query, servicesURL, Service.parse)
          })


setMethod("fetch",
          signature(x = "Service"),
          function(x, ...) {
              tmp <- .fetch.resource(x)
              fetched <- .simplify.list(tmp)
              label(x) <- fetched$label
              serviceURL(x) <- fetched$serviceUrl
              version(x) <- fetched$version
              type(x) <- fetched$type
              supportsFirstLatest(x) <- fetched$supportsFirstLatest
              quantities(x) <- as.data.frame(t(sapply(tmp, "[[", "quantities")))
              x
          })

setMethod("fetch",
          signature(x = "DomainResource"),
          function(x, ...) {
              tmp <- .fetch.resource(x)
              fetched <- .simplify.list(tmp)
              service <- Service(endpoint = endpoint(x),
                                 id = .simplify.list(tmp, "service")$id)
              label(x) <- fetched$label
              domainId(x) <- fetched$domainId
              service(x) <- fetch(service)
              x
          })

setMethod("fetch",
          signature(x = "Category"),
          function(x, ...) {
              tmp <- .fetch.resource(x)
              fetched <- .simplify.list(tmp)
              service <- Service(endpoint = endpoint(x),
                                 id = .simplify.list(tmp, "service")$id)
              label(x) <- fetched$label
              service(x) <- fetch(service)
              x
          })

setMethod("fetch",
          signature(x = "Station"),
          function(x, ...) {
              tmp <- .fetch.resource(x)
              props <- .simplify.list(tmp, "properties")
              label(x) <- props$label
              geometries <- lapply(tmp, "[[", "geometry")
              coordinates <- lapply(geometries, "[[", "coordinates")
              geometry(x) <- SpatialPoints.parse(list(coordinates = coordinates))
              x
          })

path <- function(x, path, ...) {
    x <- lapply(x, "[[", path)
    if (nargs() == 2) x else Recall(x, ...)
}

setMethod("fetch",
          signature(x = "Timeseries"),
          function(x, ...) {
              tmp <- .fetch.resource(x)

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

setMethod("getData",
          signature(x = "Timeseries"),
          function(x,
                   generalize = FALSE,
                   timespan = NULL, ...) {
              query <- list(generalize = generalize, timespan = timespan)
              tmp <- .fetch.resourceURL(getDataURL(x), query=query)
              lapply(tmp, TimeseriesData.parse)
          });
