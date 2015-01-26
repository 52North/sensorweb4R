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

#' Distance Matrix Creation
#'
#' Creates a distance matrix for the supplied coordinates.
#'
#' @param x a Nx2 matrix of WGS84 coordinates or a \code{sp::SpatialPointsDataFrame}
#' @return the distance matrix of class \code{dist}
#' @export
#' @examples
#' \dontrun{
#' stations <- get_stations(url)
#' dm <- create_distance_matrix(stations)
#' }
create_distance_matrix <- function(x) {
    n <- dim(x)[1]
    m <- matrix(0, ncol=n, nrow=n)
    for (i in 1:n) {
        for (j in i:n) {
            m[i,j] <- m[j,i] <- geosphere::distVincentyEllipsoid(x[i,], x[j,])
        }
    }
    return(as.dist(m));
}

#' Distance Matrix Deserialization
#'
#' Saves a distance matrix to disk.
#'
#' @param dm the distance matrix
#' @param file the file name
#' @export
#' @examples
#' \dontrun{
#' save_distance_matrix(dm, 'distance_matrix.rds')
#' }
#' @describeIn create_distance_matrix
save_distance_matrix <- function(dm, file) {
    saveRDS(dm, file, compress=TRUE)
}

#' Distance Matrix Serialization
#'
#' Reads the a distance matrix from disk.
#'
#' @param file the file name
#' @export
#' @examples
#' \dontrun{
#' dm <- read_distance_matrix('distance_matrix.rds')
#' }
#' @describeIn create_distance_matrix
read_distance_matrix <- function(file) {
    return(readRDS(file))
}
