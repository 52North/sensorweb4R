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

#' Print and toString functions
#'
#' @param x the object to print
#' @name print
NULL

.toString.SensorwebEndpoint <- function(x, ...) {
    .s <- paste("Object of class SensorwebEndpoint;\n",
                "url: ", x@url)
    return(.s)
}
.print.SensorwebEndpoint <- function(x, ...) {
    cat(.toString.SensorwebEndpoint(x, ...), "\n")
    invisible(x)
}

setMethod("print", "SensorwebEndpoint", function(x, ...) .print.SensorwebEndpoint(x, ...))
setMethod("toString", "SensorwebEndpoint", function(x, ...) .toString.SensorwebEndpoint(x, ...))
