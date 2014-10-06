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


#' Sensor web for R.
#'
#' @section Package options:
#' sensorweb4R uses the following \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{sensorweb4R.defaultApiVersion}: default version of API to use
#'   \item \code{sensorweb4R.defaultLoggerThreshold}: default logger used based on the \code{futile.logger} package, logs to console
#'   \item \code{sensorweb4R.loggerLayout}: layout string used in \code{futile.logger::flog.layout(..)}
#' }
#'
#' @section Debugging:
#' sensorweb4R uses \code{futile.logger} for logging. To increase the log level of this package call \code{flog.threshold("<level>", name = "sensorweb4R")}. For more information about available logging levels or advanced logger configuration, see \code{?futile.logger}.
#'
#' @docType package
#' @name sensorweb4R
#' @import futile.logger
NULL # documenting null to integrate this comment into a specific rd file via @name, see http://r-pkgs.had.co.nz/man.html#dry2

# when you change .onLoad and .onAttach functions, don't forget to call load_all with reset=TRUE !

.onLoad <- function(libname, pkgname) {
    op <- options()
    op.sensorweb4R <- list(
        sensorweb4R.defaultApiVersion = "v1",
        sensorweb4R.loggerLayout = "[~l] [~t] [~n.~f] ~m"
    )
    toset <- !(names(op.sensorweb4R) %in% names(op))
    if(any(toset)) options(op.sensorweb4R[toset])

    invisible()
}

.onAttach <- function(libname, pkgname) {
    #flog.threshold(options("sensorweb4R.defaultLoggerThreshold"), name = "sensorweb4R")
    # cannot set via string variable, calling also does not work... call(paste0("flog.threshold(", options("sensorweb4R.defaultLoggerThreshold"), ", name = 'sensorweb4R')"))
    flog.threshold(INFO, name = "sensorweb4R")
    .layout <- layout.format(options("sensorweb4R.loggerLayout"))
    flog.layout(.layout, name =  "sensorweb4R")

    packageStartupMessage("Welcome to sensorweb4R! The logging level is ", flog.threshold(), " - set it with 'flog.threshold(<level>, name = \"sensorweb4R\")'")
}




