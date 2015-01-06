
#' @include station-class.R
#' @include service-class.R
NULL

.parseTimeseries <- function(endpoint, json) json

# TODO vectorize classes at a later point
.parseStations <- function(endpoint, json) {
    crs <- sp::CRS("+proj=longlat +datum=WGS84")
    id <- json$properties$id
    label <- json$properties$label
    x <- sapply(json$geometry$coordinates, "[", 1)
    y <- sapply(json$geometry$coordinates, "[", 2)
    geometry <- sp::SpatialPoints(cbind(x, y), crs)
    Station(endpoint, id, label, geometry)
}

.parseServices <- function(endpoint, json) {
    Service(endpoint = endpoint,
            id = json$id,
            label = json$label,
            serviceUrl = json$serviceUrl,
            version = json$version,
            type = json$type,
            supportsFirstLatest = json$supportsFirstLatest,
            quantities = json$quantities)

}
