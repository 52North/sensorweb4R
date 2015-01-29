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


#' @title
#' Distance matrix methods
#' @description
#' Methods to calculate distances between \linkS4class{Station}s
#' and to find the nearest \code{Station}s.
#' @rdname distance-matrix-methods
#' @name distance-matrix-methods
NULL


#' @description
#' \code{saveDistanceMatrix} saves a distance matrix to disk.
#' @param dm the distance matrix
#' @param file the file name
#' @export
#' @examples
#' \dontrun{
#' saveDistanceMatrix(dm, 'distance_matrix.rds')
#' }
#' @rdname distance-matrix-methods
saveDistanceMatrix <- function(dm, file) {
    saveRDS(dm, file, compress=TRUE)
}


#' @description
#' \code{readDistanceMatrix} reads the a distance matrix from disk.
#' @param file the file name
#' @export
#' @examples
#' \dontrun{
#' dm <- readDistanceMatrix('distance_matrix.rds')
#' }
#' @rdname distance-matrix-methods
readDistanceMatrix <- function(file) {
    return(readRDS(file))
}

#' @description
#' \code{nearestStations} gets the \code{n} stations nearest
#' to \code{station}.
#' @param x the object to find the nearest stations for
#' @return a subset of \code{stations} orderd by their distance
#'          to \code{x}, enriched with an additional
#'          \code{distance} attribute
#' @export
#' @rdname distance-matrix-methods
setGeneric("nearestStations", function(x, ...) standardGeneric("nearestStations"))

#' Distance Matrix Creation
#'
#' \code{distanceMatrix} creates a distance matrix for the supplied object.
#'
#' @param x The object to create a distance matrix for.
#' @return the distance matrix of class \code{dist}
#' @export
#' @rdname distance-matrix-methods
setGeneric("distanceMatrix", function(x, ...) standardGeneric("distanceMatrix"))

#' @export
#' @rdname distance-matrix-methods
setGeneric("distance", function(x) standardGeneric("distance"))

setClass("DistanceStation",
         contains = "Station",
         slots = c(distance = "numeric"),)

DistanceStation <- function(station, distance) {
    station <- as.Station(station)
    new("DistanceStation", id = id(station), label = label(station),
        geometry = geometry(station), endpoint = endpoint(station),
        distance = stretch(length(station), distance, NA, as.numeric))
}


#' @param stations the \linkS4class{Station}s for the distance
#'        calculation (requested if \code{NULL})
#' @param dm the distance matrix for the stations (generated
#'        if \code{NULL})
#' @param n the number of stations that should be returned
#' @rdname distance-matrix-methods
setMethod("nearestStations",
          signature(x = "Station"),
          function(x, stations = NULL, dm = NULL, n = 1, ...) {

              if (is.null(stations)) {
                  stations <- stations(endpoint(x))
              }

              if (is.null(dm)) {
                  dm <- distanceMatrix(stations)
              }
              if (n <= 0) {
                  warning('n <= 0. Setting n to 1')
                  n <- 1
              }
              if (n > length(stations) - 1) {
                  warning('n greater than number of stations. Setting to length(stations)-1')
                  n <- length(stations) - 1
              }

              dm <- as.matrix(dm)
              # the index of the station
              idx <- match(id(x), id(stations))
              # the top n nearest indices
              top <- as.integer(names(sort(dm[idx,])[seq(2, n + 1)]))

              DistanceStation(stations[top], dm[idx,top])
          })



#' @rdname distance-matrix-methods
setMethod("distance",
          signature(x = "DistanceStation"),
          function(x) x@distance)

#' @rdname distance-matrix-methods
setMethod("distanceMatrix",
          signature(x = "SpatialPoints"),
          function(x, ...) {
              n <- length(x)
              dm <- matrix(0, ncol=n, nrow=n)
              for (i in 1:n) {
                  for (j in i:n) {
                      dm[i,j] <- dm[j,i] <-
                          geosphere::distVincentyEllipsoid(x[i,], x[j,])
                  }
              }
              return(as.dist(dm));
          })

#' @rdname distance-matrix-methods
setMethod("distanceMatrix",
          signature(x = "Station"),
          function(x, ...) distanceMatrix(geometry(x)))
