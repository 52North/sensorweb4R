#' @include class-tvp.R
#' @include class-timeseries.R
NULL

#' @export
#' @import zoo
as.zoo.TVP <- function(x, ...) {
   zoo(value(x), order.by = time(x))
}

#' @export
#' @import xts
as.xts.TVP <- function(x, ...) {
    xts(value(x), order.by = time(x))
}

#' @export
as.ts.TVP <- function(x, ...) {
    as.ts(as.zoo.TVP(x, ...))
}

#' @export
#' @import zoo
as.zoo.Timeseries <- function(x, ...) {
   lapply(getData(x), as.zoo)
}

#' @export
#' @import xts
as.xts.Timeseries <- function(x, ...) {
    lapply(getData(x), as.xts)
}

#' @export
as.ts.Timeseries <- function(x, ...) {
    lapply(getData(x), as.ts)
}
