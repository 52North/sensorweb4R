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
#


# http://adv-r.had.co.nz/S4.html

#' An S4 class to represent a sensor web API endpoint
#'
#' @slot url The URL for the API instance
setClass("SensorwebEndpoint",
         representation(url = "character"))

#' An S4 class to represent a time series.
#'
#' @slot id The identifier for the timeseries
setClass("Timeseries",
         representation(id = "character",
                        endpoint = "SensorwebEndpoint"))

#' Test function
#'
#'
#' @param endpoint An object of class \code{SensorwebEndpoint}
#' @param id The identifier of the timeseries to request from the endpoint
#'
#' @import httr
#' @export
timeseries <- function(endpoint, id) {
    .path <- paste0(endpoint@url, "timeseries/", id)
    flog.debug("Requesting %s", .path)

    response <- GET(.path)
#     cat(capture.output(str(response)))

    response.content <- content(response)
    return(response.content)
}
