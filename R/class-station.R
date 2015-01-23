#' @include generic-methods.R
#' @include helper-methods.R
#' @include class-unions.R
#' @include virtual-class-api-resource.R
NULL

#' @export
#' @import sp
setClass("Station",
         contains = "ApiResource",
         slots = list(geometry = "SpatialPoints_or_NULL"),
         validity = function(object) {
             errors <- assert.same.length(id = object@id,
                                          geometry = object@geometry)
             if (length(errors) == 0) TRUE else errors
         })

#' @export
is.Station <- function(x) is(x, "Station")
#' @export
as.Station <- function(x) as(x, "Station")

setClassUnion("Station_or_characters",
              c("Station", "character"))

setClassUnion("Station_or_NULL",
              c("Station", "NULL"))

#' @export
Station <- function(id = character(), label = NULL,
                    geometry = NULL, endpoint = NULL) {
    id <- as.character(id)
    len <- length(id)
    label <- stretch(len, label, NA, as.character)
    endpoint <- stretch(len, endpoint, as.character(NA), as.Endpoint)
    new("Station",
        endpoint = endpoint,
        id = id,
        label = label,
        geometry = geometry)
}

setMethod("geometry",
          signature(obj = "Station"),
          function(obj) obj@geometry)

setMethod("geometry<-",
          signature(x = "Station",
                    value = "SpatialPoints_or_NULL"),
          function(x, value) {
              x@geometry <- value
              invisible(x)
          })

setAs("character", "Station", function(from) Station(id = from))

rbind2.Station <- function(x, y) {
    x <- as.Station(x)
    y <- as.Station(y)
    Station(endpoint = rbind(endpoint(x), endpoint(y)),
            id = c(id(x), id(y)),
            label = c(label(x), label(y)),
            geometry = c(geometry(x), geometry(y)))
}
setMethod("rbind2", signature("Station", "Station"),
          function(x, y) rbind2.Station(x, y))
setMethod("rbind2", signature("Station", "ANY"),
          function(x, y) rbind2.Station(x, as.Station(y)))
setMethod("rbind2", signature("ANY", "Station"),
          function(x, y) rbind2.Station(as.Station(x), y))
setMethod("rbind2", signature("ANY", "ANY"),
          function(x, y) rbind2.Station(as.Station(x),
                                        as.Station(y)))

setMethod("rep", signature(x = "Station"), function(x, ...)
    Station(endpoint = rep(endpoint(x), ...),
            id = rep(id(x), ...),
            label = rep(label(x), ...),
            geometry = rep(geometry(x), ...)))

#' @import sp
setMethod("rep",
          signature(x = "SpatialPoints"),
          function(x, ...) {
              coords <- coordinates(x)
              ncoords <- matrix(rep(coords, ...), ncol = dim(coords)[[2]])
              dimnames(ncoords) <- list(NULL, dimnames(coords)[[2]])
              SpatialPoints(ncoords, CRS(proj4string(x)), bbox(x))
          })


#' @import sp
setMethod("coordinates",
          signature(obj = "Station"),
          function(obj, ...) coordinates(obj@geometry))

#' @import sp
setMethod("bbox",
          signature(obj = "Station"),
          function(obj) bbox(obj@geometry))

#' @export
#' @import sp
as.SpatialPointsDataFrame <- function(x)
    as(x, "SpatialPointsDataFrame")

setAs("Station", "SpatialPointsDataFrame",
      function(from)
          SpatialPointsDataFrame(coords = geometry(x),
                                 data = data.frame(id = id(x),
                                                   label = label(x))))
