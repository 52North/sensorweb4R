# Copyright (C) 2014 52°North Initiative for Geospatial Open Source
# Software GmbH
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License version 2 as published
# by the Free Software Foundation.
#
# If the program is linked with libraries which are licensed under one of
# the following licenses, the combination of the program with the linked
# library is not considered a "derivative work" of the program:
#
#     - Apache License, version 2.0
#     - Apache Software License, version 1.0
#     - GNU Lesser General Public License, version 3
#     - Mozilla Public License, versions 1.0, 1.1 and 2.0
#     - Common Development and Distribution License (CDDL), version 1.0
#
# Therefore the distribution of the program linked with libraries licensed
# under the aforementioned licenses, is permitted by the copyright holders
# if the distribution is compliant with both the GNU General Public
# License version 2 and the aforementioned licenses.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details.

#' Distance Matrix Creation
#'
#' Creates a distance matrix for the supplied coordinates.
#'
#' @param x a Nx2 matrix of WGS84 coordinates or
#'        a \code{sp::SpatialPointsDataFrame}
#' @return the distance matrix of class \code{dist}
#'
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
#' Reads a distance matrix from disk.
#'
#' @param dm the distance matrix
#' @param the file name
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
#' Saves the a distance matrix to disk.
#'
#' @param x the distance matrix
#' @param the file name
#' @export
#' @examples
#' \dontrun{
#' dm <- read_distance_matrix('distance_matrix.rds')
#' }
#' @describeIn create_distance_matrix
read_distance_matrix <- function(file) {
    return(readRDS(file))
}
