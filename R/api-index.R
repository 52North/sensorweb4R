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


#' Sensorweb API index
#'
#' \code{sensorweb_index} returns an object representing the index endpoint of
#' the sensor web API.
#'
#' @param url The API endpoint to use
#' @param ... Further arguments passed on to \code{GET}
#' @return R object with the further endpoints offered by the service
#' @author Daniel Nuest \email{d.nuest@@52north.org}
#'
#' @export
#' @import httr
#' @import futile.logger
#' @examples
#' sensorweb_index(sensorweb_api_endpoints()[[1]])

sensorweb_index <- function(url, ...) {
    if(is.null(url) || is.na(url)) stop("URL parameter must be provided.")

    #flog.info("Requesting index from %s", url)

    response <- GET(url, ...)
    #flog.debug("Response: %s", response)

    content <- parsed_content(response)
    return(content)
}
