# Copyright 2014 52°North Initiative for Geospatial Open Source Software GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Station Endpoint URL
#'
#' Constructs the station endpoint URL for a given Timeseries API.
#'
#' @param endpoint the Timeseries API endpoint
#' @return the URL of the station endpoint
#' @examples
#' \dontrun{
#' stations_url('http://sensorweb.demo.52north.org/tsapi')
#' }
#'
.stations_url <- function(endpoint) {
    return(paste(endpoint, 'api/v1/stations', sep='/'))
}

.geojson_to_spdf <- function(json) {
    geom <- t(sapply(json$geometry$coordinates, as.numeric))

    for (property in names(json$properties)) {
        json[[property]] <- json$properties[[property]]
    }

    json$geometry <- NULL
    json$properties <- NULL
    json$type <- NULL
    sp::coordinates(json) <- geom
    json@proj4string = sp::CRS("+proj=longlat +datum=WGS84")
    return(json)
}

#' Request Stations
#'
#' Requests station data from the Timeseries API using
#' the supplied filters.
#'
#' @param endpoint the base URL of the Timeseries API
#' @param service only return resources where the service id does match
#' @param category only return resources where the category id does match
#' @param phenomenon only return resources where the phenomenon id does match
#' @param crs the CRS to retrieve
#' @param bbox the BBox to retrieve
#' @param near the \code{near} filter
#' @export
#' @return a \code{sp::SpatialPointsDataFrame} containing matched stations
#' @examples
#' \dontrun{
#' stations <- get_stations('http://sensorweb.demo.52north.org/tsapi',
#'                          service = 'srv_6d9ccea8d609ecb74d4a512922bb7cee')
#' }
get_stations <- function(endpoint, service=NULL, category=NULL,
                         phenomenon=NULL, crs=NULL, bbox=NULL, near=NULL) {
    queryParameter <- list(crs = crs, bbox = bbox, near = near,
                           service = service, category = category,
                           phenomenon = phenomenon)
    json <- .get_json(.stations_url(endpoint), query=queryParameter)
    stations <- .geojson_to_spdf(json)
    return(stations)
}

#' Get Nearest Stations
#'
#' \code{get_nearest_stations} gets the \code{n} stations nearest
#' to \code{station}.
#'
#' @param station the station identifier
#' @param stations the \code{SpatialPointsDataFrame} containing
#'        the stations
#' @param dm the distance matrix for the stations (generated
#'        if \code{NULL})
#' @param n the number of stations that should be returned
#' @return a subset of \code{stations} orderd by their distance
#'          to \code{station}, enriched with an additional
#'          \code{dist} attribute
#' @export
#' @examples
#' \dontrun{
#' url <- 'http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable'
#' stations <- get_stations(url, service = 'srv_6d9ccea8d609ecb74d4a512922bb7cee')
#' dm <- create_distance_matrix(stations)
#' get_nearest_stations(station, stations, dm, n=5)
#' }
#'
get_nearest_stations <- function(station, stations, dm=NULL, n=1) {
    if (is.null(dm)) {
        dm <- create_distance_matrix(stations)
    }
    if (n <= 0) {
        warning('n <= 0. Setting n to 1')
        n <- 1
    }
    if (n > length(stations)-1) {
        warning('n greater than number of stations. Setting to length(stations)-1')
        n <- length(stations)-1
    }

    dm <- as.matrix(dm)
    # the index of the station
    idx <- match(station, stations@data$id)
    # the top n nearest indices
    top <- as.integer(names(sort(dm[idx,])[seq(2,n+1)]))
    sub <- stations[top,]
    sub@data$distance <- dm[idx,top]
    return(sub)
}
