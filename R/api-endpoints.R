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

#' Example API endpoints.
#'
#' \code{sensorweb_api_endpoints} returns a list of endpoints that can be used for testing.
#'
#' @return R object with the further endpoints offered by the service
#' @author Daniel Nuest \email{d.nuest@@52north.org}
#'
#' @export
#'
#' @examples
#' sensorweb_api_endpoints()
#' sensorweb_index(sensorweb_api_endpoints()[[1]])
sensorweb_api_endpoints <- function() {
    .endpoints <- list("52N Demo" = "http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/",
                       "IRCEL-CELINE" = "http://sosrest.irceline.be/api/v1/",
                       "WV" = "http://www.fluggs.de/sos2/api/v1/",
                       "Geonovum" = "http://sensors.geonovum.nl/sos/api/v1/")
    return(.endpoints)
}
