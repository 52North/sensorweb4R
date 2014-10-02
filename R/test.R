# Copyright (C) 2014 52\u00b0North Initiative for Geospatial Open Source
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


# Copyright (C) 2014 52\u00b0North Initiative for Geospatial Open Source
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
#' License header
#'
#' \code{license_header} adds license headers to all the R source code files of the package.
#'
#' @param pkg package name, used in \code{devtools:::find_code(..)}.
#' @param header a path to the header file to be used
#' @param add boolean variable
#' @return A message of the result of the function.
#' @examples
#' \dontrun{
#'  license_header()
#' }
license_header <- function(pkg = ".", header = "inst/license-header", add = FALSE) {
    files <- devtools:::find_code(devtools:::as.package("."))
    flog.debug("Checking license headers of %s files using '%s', adding if missing = %s", length(files), header, add)

    .result <- .addLicenseHeaderToFiles(files, header, add)
    return(.result)
}

.linesHaveHeader <- function(lines, header_content) {
    haveHeader <- FALSE

    lines[1]


    return(haveHeader)
}

.addLicenseHeaderToFiles <- function (files, header = "inst/license-header", add = FALSE) {
    flog.trace("Checking files %s", toString(paste(files)))

    files_missing_header <- list()

    headerConn <- file(file.path(devtools:::as.package(".")$path, header), 'r+')
    header_content <- readLines(headerConn)
    close(headerConn)
    flog.trace("Header: %s", paste(header_content, sep = "", collapse = ""))

    # check if files starts with license header
    for(f in files) {
        flog.trace("Checking license of file %s", f)

        fConn <- file(f, 'r')
        lines <- readLines(fConn)

        cat("line 0 ", lines[0], "\n")

        if(.linesHaveHeader(lines, header_content))
            flog.trace("File %s already has a header", f)
        else {
            file_missing_header <- c(file_missing_header, f)

            if(add) {
                flog.trace("Adding header to file %s", f)
                #             writeLines(c("Text at beginning of file\nlala\n", lines, sep = "\n"),
                #                        con = fConn)
                cat(paste0("Text at beginning of file\nlala\n", lines, sep = "\n"),
                    con = fConn)
            }
        }

        close(fConn)
    }

    flog.trace("%s files missing header: %s", length(files_missing_header), toString(files_missing_header))
    if(length(files_missing_header) < 1)
        return(paste0("All files have the license header specified in ", header))
    else return(paste0("Files missing the header: ", paste0(files_missing_header)))
}




