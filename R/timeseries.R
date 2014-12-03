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
