

SpatialPoints.parse <- function(json) {
    x <- sapply(json$coordinates, "[", 1)
    y <- sapply(json$coordinates, "[", 2)
    crs <- sp::CRS("+proj=longlat +datum=WGS84")
    sp::SpatialPoints(cbind(x, y), crs)
}

Station.parse <- function(endpoint, json) {
    id <- as.character(json$properties$id)
    label <- as.character(json$properties$label)
    geometry <- SpatialPoints.parse(json$geometry)

    Station(endpoint = endpoint, id = id, label = label, geometry = geometry)
}

Service.parse <- function(endpoint, json) {
    id <- as.character(json$id)
    label <- as.character(json$label)
    serviceURL <- as.character(json$serviceUrl)
    version <- as.character(json$version)
    type <- as.character(json$type)
    supportsFirstLatest <- as.logical(json$supportsFirstLatest)
    quantities <- as.data.frame(json$quantities)

    Service(endpoint = endpoint, id = id, label = label, serviceURL = serviceURL,
            version = version, type = type, supportsFirstLatest = supportsFirstLatest,
            quantities = quantities)
}

ReferenceValue.parse <- function(endpoint, json) {
    lapply(json, function(x) {
        id <- as.character(x$referenceValueId)
        label <- as.character(x$label)
        time <- x$lastValue$timestamp
        time <- as.POSIXct(time/1000, tz = "GMT",
                           origin = "1970-01-01 00:00:00")
        value <- as.numeric(x$lastValue$value)

        ReferenceValue(endpoint = endpoint,
                       id = id,
                       label = label,
                       time = time,
                       value = value)
    })
}

Timeseries.parse <- function(endpoint, json) {
    id <- as.character(json$id)
    label <- as.character(json$label)
    uom <- as.character(json$uom)
    phenomenon <- Phenomenon.parse(endpoint, json$parameters$phenomenon)
    category <- Category.parse(endpoint, json$parameters$category)
    service <- Service.parse(endpoint, json$parameters$service)
    feature <- Feature.parse(endpoint, json$parameters$feature)
    offering <- Offering.parse(endpoint, json$parameters$offering)
    procedure <- Procedure.parse(endpoint, json$parameters$procedure)
    station <- Station.parse(endpoint, json$station)
    referenceValues <- ReferenceValue.parse(endpoint, json$referenceValues)

    Timeseries(endpoint = endpoint, id = id, label = label, uom = uom,
        phenomenon = phenomenon, category = category, service = service,
        feature = feature, offering = offering,  procedure = procedure,
        station = station, referenceValues = referenceValues)
}

TimeseriesData.parse <- function(json) {
    value <- as.numeric(json$values$value)
    time <- json$values$timestamp
    time <- as.POSIXct(time/1000, tz = "GMT",
                       origin = "1970-01-01 00:00:00")
    TVP(time = time, value = value)
}

Phenomenon.parse <- function(endpoint, json) {
    id <- as.character(json$id)
    label <- as.character(json$label)
    domainId <- as.character(json$domainId)
    service <- Service.parse(endpoint, json$service)
    Phenomenon(endpoint = endpoint, id = id, label = label, service = service, domainId = domainId)
}

Feature.parse <- function(endpoint, json) {
    id <- as.character(json$id)
    label <- as.character(json$label)
    domainId <- as.character(json$domainId)
    service <- Service.parse(endpoint, json$service)
    Feature(endpoint = endpoint, id = id, label = label, service = service, domainId = domainId)
}

Category.parse <- function(endpoint, json) {
    id <- as.character(json$id)
    label <- as.character(json$label)
    service <- Service.parse(endpoint, json$service)
    Category(endpoint = endpoint, id = id, label = label, service = service)
}

Offering.parse <- function(endpoint, json) {
    id <- as.character(json$id)
    label <- as.character(json$label)
    domainId <- as.character(json$domainId)
    service <- Service.parse(endpoint, json$service)
    Offering(endpoint = endpoint, id = id, label = label, service = service, domainId = domainId)
}

Procedure.parse <- function(endpoint, json) {
    id <- as.character(json$id)
    label <- as.character(json$label)
    domainId <- as.character(json$domainId)
    service <- Service.parse(endpoint, json$service)
    Procedure(endpoint = endpoint, id = id, label = label, service = service, domainId = domainId)
}
