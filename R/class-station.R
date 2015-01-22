#' @include generic-methods.R
#' @include helper-methods.R
#' @include class-unions.R
#' @include virtual-class-api-resource.R
NULL

#' @export
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
Station <- function(id,
                    label = rep(as.character(NA), length(id)),
                    geometry = NULL,
                    endpoint = rep(Endpoint(as.character(NA)), length(id))) {
    new("Station",
        endpoint = endpoint,
        id = id,
        label = label,
        geometry = geometry)
}

setMethod("coordinates",
          signature(obj = "Station"),
          function(obj, ...) coordinates(obj@geometry))

setMethod("geometry",
          signature(obj = "Station"),
          function(obj) obj@geometry)

setAs("character", "Station", function(from) Station(id = from))

#' @export
as.SpatialPointsDataFrame <- function(x) as(x, "SpatialPointsDataFrame")
setAs("Station", "SpatialPointsDataFrame",
      function(from)
          SpatialPointsDataFrame(coords = geometry(x),
                                 data = data.frame(id = id(x),
                                                   label = label(x))))

rbind2.Station <- function(x, y) {
    x <- as.Station(x)
    y <- as.Station(y)
    Station(endpoint = rbind(endpoint(x), endpoint(y)),
            id = c(id(x), id(y)),
            label = c(label(x), label(y)),
            geometry = c(geometry(x), geometry(y)))
}
setMethod("rbind2", signature("Station", "Station"), function(x, y) concat.pair.Station(x, y))
setMethod("rbind2", signature("Station", "ANY"), function(x, y) concat.pair.Station(x, as.Station(y)))
setMethod("rbind2", signature("ANY", "Station"), function(x, y) concat.pair.Station(as.Station(x), y))
setMethod("rbind2", signature("ANY", "ANY"), function(x, y) concat.pair.Station(as.Station(x), as.Station(y)))

setMethod("rep", signature(x = "Station"), function(x, ...)
    Category(endpoint = rep(endpoint(x), ...),
             id = rep(id(x), ...),
             label = rep(label(x), ...),
             geometry = rep(geometry(x), ...)))
