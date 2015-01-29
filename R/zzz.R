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

#' \code{sensorweb4R}: functions and classes to download data from sensor web
#' services
#'
#' @section About: Sensor web services contain timeseries of sensoric data, such
#'   as temperature and rainfall by weather stations. This package can retrieve
#'   specific subsets of data, such as specific stations or observed phenomena
#'   by providin plain R function calls to load these datasets into your session
#'   as ready-to-use \code{R} data structures.
#'
#' @section Documentation: The main documentation for this package can be
#'   found in the vignette (\code{vignette("sensorweb4R")}) and the demos
#'   (\code{demo(package = "sensorweb4R")}).
#'
#' @section Package options: sensorweb4R uses the following
#'   \code{\link{options}} to configure behaviour:
#'
#'   \itemize{ \item \code{sensorweb4R.loggerLayout}: layout string used in
#'   \code{futile.logger::flog.layout(..)} }
#'
#' @section Debugging: sensorweb4R uses \code{futile.logger} for logging. To
#'   increase the log level of this package call \code{flog.threshold("<level>",
#'   name = "sensorweb4R")}. For more information about available logging levels
#'   or advanced logger configuration, see \code{?futile.logger}.
#'
#' @author Christian Autermann, \email{c.autermann@@52north.org}
#' @author Daniel Nuest, \email{d.nuest@@52north.org}
#'
#' @docType package
#' @name sensorweb4R
#' @import futile.logger
#' @keywords ts, spatial
#' @concept sensorweb, timeseries
NULL # documenting null to integrate this comment into a specific rd file via @name, see http://r-pkgs.had.co.nz/man.html#dry2

# when you change .onLoad and .onAttach functions, don't forget to call load_all with reset=TRUE !

.onLoad <- function(libname, pkgname) {
    op <- options()
    op.sensorweb4R <- list(
        #sensorweb4R.defaultApiVersion = "v1",
        sensorweb4R.loggerLayout = "[~l] [~t] [~n.~f] ~m"
    )
    toset <- !(names(op.sensorweb4R) %in% names(op))
    if(any(toset)) options(op.sensorweb4R[toset])

    invisible()
}

.onAttach <- function(libname, pkgname) {
    flog.threshold(INFO, name = "sensorweb4R")
    .layout <- layout.format(options("sensorweb4R.loggerLayout"))
    flog.layout(.layout, name =  "sensorweb4R")
    packageStartupMessage("Welcome to sensorweb4R! The logging level is ", flog.threshold(), " - set it with 'flog.threshold(<level>, name = \"sensorweb4R\")'")
}




