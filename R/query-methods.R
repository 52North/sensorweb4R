

setMethod("stations",
          signature(x = "Endpoint"),
          function(x,
                   service = NULL,
                   category = NULL,
                   phenomenon = NULL,
                   crs = NULL,
                   bbox = NULL,
                   near = NULL,
                   ...) {
              query <- list(
                  bbox = bbox,
                  near = near,
                  service = .as.parameter.list(service),
                  category = .as.parameter.list(category),
                  phenomenon = .as.parameter.list(phenomenon)
              )
              futile.logger::flog.debug(toString(query))
              .stopifnoquery(query)
              query[["crs"]] <- crs
              .get_and_parse(x, query, stationsURL, Station.parse)
          })

setMethod("timeseries",
          signature(x = "Endpoint"),
          function(x,
                   timespan = NULL,
                   service = NULL,
                   category = NULL,
                   phenomenon = NULL,
                   ...) {
              query <- list(timespan = timespan,
                            service = .as.parameter.list(service),
                            category = .as.parameter.list(category),
                            phenomenon = .as.parameter.list(phenomenon))
              .stopifnoquery(query)
              query[["expanded"]] = TRUE
              .get_and_parse(x, query, timeseriesURL, Timeseries.parse)
          })

setMethod("services",
          signature(x = "Endpoint"),
          function(x,
                   service = NULL,
                   category = NULL,
                   station = NULL,
                   phenomenon = NULL,
                   locale = NULL, ...) {
              query <- list(service = .as.parameter.list(service),
                            category = .as.parameter.list(category),
                            station = .as.parameter.list(station),
                            phenomenon = .as.parameter.list(phenomenon),
                            expanded = TRUE)
              .get_and_parse(x, query, servicesURL, Service.parse)
          })


setMethod("fetch",
          signature(x = "Service"),
          function(x, ...) {
              tmp <- .fetch.resource(x)
              fetched <- .simplify.list(tmp)
              label(x) <- as.character(fetched$label)
              serviceURL(x) <- as.character(fetched$serviceUrl)
              version(x) <- as.character(fetched$version)
              type(x) <- as.character(fetched$type)
              supportsFirstLatest(x) <- as.logical(fetched$supportsFirstLatest)
              quantities(x) <- as.data.frame(t(sapply(tmp, "[[", "quantities")))
              x
          })

setMethod("fetch",
          signature(x = "DomainResource"),
          function(x, ...) {
              tmp <- .fetch.resource(x)
              fetched <- .simplify.list(tmp)
              service <- .simplify.list(tmp, "service")
              service <- Service(endpoint(x), as.character(service$id))
              label(x) <- as.character(fetched$label)
              domainId(x) <- as.character(fetched$domainId)
              service(x) <- fetch(service)
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
              #.get_and_parse(x, query, getDataURL, TimeseriesData.parse)
          });
