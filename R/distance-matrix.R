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
#' Methods to calculate distances between objects and to find the geographically
#' nearest neighbor(s) between objects.
#' @details
#' \code{nearest} calculates the nearest objects of \code{all} in relation
#' to \code{x}. The distance matrix can be supplied using the \code{dm}
#' parameter and can be calculated using \code{distanceMatrix}. As this
#' computation is quite expensive the distance matrix can be saved and read
#' using \code{saveDistanceMatrix} and \code{readDistanceMatrix}.
#'
#' \code{nearest} returns the same class or a subclass of \code{all}. The
#' returned vector or vectorized object is sorted by their distance to
#' \code{x}. Additionally the distance can be obtained using \code{distance}.
#'
#' The distance is calculated using computations on an ellipsoid and the
#' returned are meters.
#'
#' @param dm The distance matrix.
#' @param all The possible nearest neighbors for \code{x}. For the
#'        \linkS4class{Station} method, \code{all} can be omitted.
#'        In that case, \code{all} will default to \code{stations(endpoint(x))}.
#' @param dm The distance matrix for the objects (generated if missing).
#' @param x For \code{distanceMatrix}: The object to create a distance matrix for.\cr
#'          For \code{nearest}: the object to calculate the nearest neighbors for.
#' @param n The number of nearest objects that should be returned.
#' @param filter.fun A function returning a \code{logical} vector to subset
#'                   \code{all} and to allow only specific elements of \code{all}
#'                   to be considered for the calculation.
#' @param file the file to save to or read from
#'
#' @return
#' For \code{nearest}: a subset of \code{all} orderd by their distance
#' to \code{x}, enriched with an additional distance attribute.
#'
#' For \code{distanceMatrix} and \code{readDistanceMatrix}: a distance
#' matrix of class \code{dist}.
#'
#' For \code{distnace}: a numeric vector containing the distance in meters.
#'
#' @examples
#' endpoint <- example.endpoints()[2]
#' sta.all <- stations(endpoint)
#' station <- sample(sta.all, 1)
#' dm <- distanceMatrix(sta.all)
#' sta.near <- nearest(station, stations = sta.all, dm = dm, n = 5)
#' distance(sta.near)
#' \dontrun{
#' saveDistanceMatrix(dm, 'distance_matrix.rds')
#' sta.near <- nearest(station, stations = sta.all,
#'                     dm = readDistanceMatrix('distance_matrix.rds'))
#' }
#' @name distance-matrix-methods
#' @rdname distance-matrix-methods
NULL


#' @export
#' @rdname distance-matrix-methods
saveDistanceMatrix <- function(dm, file) {
    saveRDS(dm, file, compress=TRUE)
}

#' @export
#' @rdname distance-matrix-methods
readDistanceMatrix <- function(file) {
    return(readRDS(file))
}

setClass("DistanceStation",
         contains = "Station",
         slots = c(distance = "numeric"),)

DistanceStation <- function(station, distance) {
    station <- as.Station(station)
    new("DistanceStation", id = id(station), label = label(station),
        geometry = geometry(station), endpoint = endpoint(station),
        distance = stretch(length(station), distance, NA, as.numeric))
}

distance.DistanceStation <- function(x)
    x@distance

nearest.Station <- function(x, all, dm = NULL, n = 1, filter.fun = NULL, ...) {

    if (missing(all))
        all <- stations(endpoint(x))
    if (!id(x) %in% id(all))
        stop("x is not part of all")
    if (is.null(dm))
        dm <- distanceMatrix(all)
    if (n <= 0) {
        warning('n <= 0. Setting n to 1')
        n <- 1
    }

    dm <- as.matrix(dm)

    if (dim(dm)[1] != length(all))
        stop("Incompatible dimensions of all and dm")

    # the index of the station
    idx <- match(id(x), id(all))

    if (missing(filter.fun)) {
        if (n > length(all) - 1) {
            warning('n greater than number of stations')
            n <- length(all) - 1
        }
        # all stations are possible candidates
        idx.filter <- seq_len(length(all))
    } else {
        filter.fun <- match.fun(filter.fun)
        all.filtered <- all[filter.fun(all)]

        # limited candidates..
        idx.filter <- match(id(all.filtered), id(all))

        if (idx %in% idx.filter) {
            if (n > length(idx.filter) - 1) {
                warning('n greater than number of stations')
                n <- length(idx.filter) - 1
            }
        } else {
            if (n > length(idx.filter)) {
                warning('n greater than number of stations')
                n <- length(idx.filter)
            }
            # include x to have at least on result
            idx.filter <- c(idx, idx.filter)
        }
    }

    # indices orderd by distance
    top <- as.integer(names(sort(dm[idx,][idx.filter])))

    # the top n nearest indices
    top <- top[seq(2, n + 1)]


    DistanceStation(all[top], dm[idx, top])
}


distanceMatrix.SpatialPoints <- function(x, ...) {
    n <- length(x)
    dm <- matrix(0, ncol=n, nrow=n)
    for (i in 1:n) {
        for (j in i:n) {
            dm[i,j] <- dm[j,i] <-
                geosphere::distVincentyEllipsoid(x[i,], x[j,])
        }
    }
    return(as.dist(dm));
}

distanceMatrix.Station <- function(x, ...)
    distanceMatrix(geometry(x))



#' @export
#' @rdname distance-matrix-methods
setGeneric("distance", function(x)
    standardGeneric("distance"))

#' @rdname distance-matrix-methods
setMethod("distance",
          signature(x = "DistanceStation"),
          distance.DistanceStation)

#' @export
#' @rdname distance-matrix-methods
setGeneric("nearest", function(x, all, dm = NULL, n = 1, ...)
    standardGeneric("nearest"))

#' @rdname distance-matrix-methods
setMethod("nearest",
          signature(x = "Station",
                    all = "Station"),
          nearest.Station)

#' @rdname distance-matrix-methods
setMethod("nearest",
          signature(x = "Station",
                    all = "missing"),
          nearest.Station)

#' @export
#' @rdname distance-matrix-methods
setGeneric("distanceMatrix", function(x, ...)
    standardGeneric("distanceMatrix"))

#' @rdname distance-matrix-methods
setMethod("distanceMatrix",
          signature(x = "SpatialPoints"),
          distanceMatrix.SpatialPoints)

#' @rdname distance-matrix-methods
setMethod("distanceMatrix",
          signature(x = "Station"),
          distanceMatrix.Station)
