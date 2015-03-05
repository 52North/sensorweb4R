#' @include generic-methods.R
#' @include helper-methods.R
#' @include class-unions.R
#' @include virtual-class-api-resource.R
NULL

#' Station
#'
#' Represents a station.
#'
#' @family API Resources
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @export
#' @rdname Station-class
#' @name Station-class
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
#' @describeIn Station-class Checks whether \code{x} is a \code{Station}.
is.Station <- function(x) is(x, "Station")

#' @export
#' @describeIn Station-class Coerces \code{x} into a \code{Station}.
as.Station <- function(x) as(x, "Station")

setClassUnion("Station_or_characters",
              c("Station", "character"))

setClassUnion("Station_or_NULL",
              c("Station", "NULL"))

#' @export
#' @describeIn Station-class Constructs a new \code{Station}.
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

#' @import sp
#' @rdname accessor-methods
setMethod("geometry",
          signature(obj = "Station"),
          function(obj) obj@geometry)

#' @import sp
#' @rdname accessor-methods
setMethod("geometry<-",
          signature(x = "Station",
                    value = "SpatialPoints_or_NULL"),
          function(x, value) {
              x@geometry <- value
              invisible(x)
          })

setAs("character", "Station", function(from) Station(id = from))
setAs("list", "Station", function(from) concat.list(from))

rbind2.Station <- function(x, y) {
    x <- as.Station(x)
    y <- as.Station(y)
    Station(endpoint = rbind2(endpoint(x), endpoint(y)),
            id = c(id(x), id(y)),
            label = c(label(x), label(y)),
            geometry = rbind2(sp::geometry(x), sp::geometry(y)))
}

#' @rdname rbind2-methods
setMethod("rbind2", signature("Station", "Station"),
          function(x, y) rbind2.Station(x, y))

#' @rdname rbind2-methods
setMethod("rbind2", signature("Station", "ANY"),
          function(x, y) rbind2(x, as.Station(y)))

#' @rdname rbind2-methods
setMethod("rbind2", signature("ANY", "Station"),
          function(x, y) rbind2(as.Station(x), y))

#' @rdname rep-methods
setMethod("rep", signature(x = "Station"), function(x, ...)
    Station(endpoint = rep(endpoint(x), ...),
            id = rep(id(x), ...),
            label = rep(label(x), ...),
            geometry = rep(geometry(x), ...)))

#' @import sp
#' @rdname rep-methods
setMethod("rep",
          signature(x = "SpatialPoints"),
          function(x, ...) {
              coords <- sp::coordinates(x)
              ncoords <- matrix(rep(coords, ...), ncol = dim(coords)[[2]])
              dimnames(ncoords) <- list(NULL, dimnames(coords)[[2]])
              sp::SpatialPoints(ncoords, sp::CRS(sp::proj4string(x)), sp::bbox(x))
          })

#' @import sp
rbind2.SpatialPoints <- function(x, y) {
    x <- as(x, "SpatialPoints")
    y <- as(y, "SpatialPoints")
    coordsx <- sp::coordinates(x)
    coordsy <- sp::coordinates(y)
    if (dim(coordsx)[[2]] != dim(coordsy)[[2]])
        stop("Incompatible coordinate dimensions")
    if (sp::proj4string(x) != sp::proj4string(y))
        stop("Incompatible coordinate reference systems")
    sp::SpatialPoints(rbind(coordsx, coordsy), sp::CRS(sp::proj4string(x)))
}

#' @rdname rbind2-methods
setMethod("rbind2", signature("SpatialPoints", "SpatialPoints"),
          function(x, y) rbind2.SpatialPoints(x, y))

#' @rdname rbind2-methods
setMethod("rbind2", signature("SpatialPoints", "ANY"),
          function(x, y) rbind2(x, as(y, "SpatialPoints")))

#' @rdname rbind2-methods
setMethod("rbind2", signature("ANY", "SpatialPoints"),
          function(x, y) rbind2(as(x, "SpatialPoints"), y))

#' @rdname rep-methods
setMethod("rep",
          signature(x = "SpatialPoints"),
          function(x, ...) {
              coords <- sp::coordinates(x)
              ncoords <- matrix(rep(coords, ...), ncol = dim(coords)[[2]])
              dimnames(ncoords) <- list(NULL, dimnames(coords)[[2]])
              sp::SpatialPoints(ncoords, sp::CRS(sp::proj4string(x)), sp::bbox(x))
          })


#' @import sp
#' @rdname accessor-methods
setMethod("coordinates",
          signature(obj = "Station"),
          function(obj, ...) sp::coordinates(obj@geometry))

#' @import sp
#' @rdname accessor-methods
setMethod("bbox",
          signature(obj = "Station"),
          function(obj) sp::bbox(obj@geometry))

#' @export
#' @import sp
as.SpatialPointsDataFrame <- function(x)
    as(x, "SpatialPointsDataFrame")

setAs("Station", "SpatialPointsDataFrame",
      function(from)
          SpatialPointsDataFrame(coords = sp::geometry(from),
                                 data = data.frame(id = id(from),
                                                   label = label(from))))
