#' @include generic-methods.R
#' @include parse-methods.R
NULL



#service= "Service_or_character",
#category = "Category_or_character",
#phenomenon = "Phenomenon_or_character",
#crs = "CRS",
#bbox = "matrix",
#near = "numeric"
#' @importClassesFrom sp CRS
setMethod("stations",
          signature(x = "Endpoint"),
          function(x, service = NULL, category = NULL, phenomenon = NULL,
                   crs = NULL, bbox = NULL, near = NULL,
                   ...) {

              query <- list(
                  bbox = bbox,
                  near = near,
                  service = as.character(service),
                  category = as.character(category),
                  phenomenon = as.character(phenomenon)
              )

              .stopifnoquery(query)
              query[["crs"]] <- crs
              .get_and_parse(x, query, stationsURL, .parseStations)
          })

setMethod("timeseries", "Endpoint",
          function(x,
                   timespan = NULL,
                   generalize = FALSE,
                   forceLatestValues = FALSE,
                   service = NULL,
                   category = NULL,
                   phenomenon = NULL,
                   ...) {

              query <- list(bbox = bbox,
                            near = near,
                            service = service,
                            category = category,
                            phenomenon = phenomenon)

              .stopifnoquery(query)
              query[["crs"]] <- crs
              .get_and_parse(x, query, timeseriesURL, .parseTimeseries)
          })

setMethod("services", "Endpoint",
          function(x,
                   service = NULL,
                   category = NULL,
                   station = NULL,
                   phenomenon = NULL,
                   locale = NULL,
                   ...) {
              query <- list(service = service,
                            category = category,
                            station = station,
                            phenomenon = phenomenon,
                            expanded = TRUE)
              .get_and_parse(x, query, servicesURL, .parseServices)
          })
