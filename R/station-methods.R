
#' @include station-class.R
#' @include generic-methods.R
NULL

setMethod("services", "Station", function(x, ...) {
    services(x@endpoint, station = x, ...)
})

#' @import sp
setMethod("coordinates", "Station", function(obj, ...) {
    coordinates(obj@geometry)
})

#' @import sp
setMethod("geometry", "Station", function(obj){
    obj@geometry
})

#' @import sp
#' @export
setAs("Station", "SpatialPointsDataFrame", function(from) {
    sp::SpatialPointsDataFrame(coords = geometry(from),
                               data = data.frame(id = id(from),
                                                 label = label(from)))
})

#' @export
setAs("Station", "data.frame", function(from) {
    as.data.frame.Station(x)
})

#' @export
as.data.frame.Station <- function(x, ...) {
    data.frame(id = id(x),label = label(x),
               x = geometry(x)$x, y = geometry(x)$y)
}

setMethod("as.data.frame", "Station", as.data.frame.Station)

setMethod("as.SpatialPointsDataFrame", "Station", function(x, ...) {
    as(x, "SpatialPointsDataFrame")
})
