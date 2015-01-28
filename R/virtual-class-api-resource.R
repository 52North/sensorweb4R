#' @include generic-methods.R
#' @include helper-methods.R
#' @include virtual-class-http-resource.R
#' @include class-endpoint.R
#' @include class-unions.R
NULL


#' @export
setClass("ApiResource",
         contains = c("VIRTUAL",
                      "HttpResource"),
         slots = list(id = "character",
                      label = "character",
                      endpoint = "Endpoint"),
         validity = function(object) {
             errors <- assert.same.length(id = object@id,
                                          label = object@label,
                                          endpoint = object@endpoint)
             if (length(errors) == 0) TRUE else errors
         })

#' @export
is.ApiResource <- function(x) is(x, "ApiResource")

#' @export
as.ApiResource <- function(x) as(x, "ApiResource")

#' @export
length.ApiResource <- function(x) length(id(x))

setMethod("length",
          signature(x = "ApiResource"),
          length.ApiResource)

#' @export
as.list.ApiResource <- function(x, ...)
    lapply(seq_len(length(x)), function(i) x[i])

setMethod("as.list",
          signature(x = "ApiResource"),
          as.list.ApiResource)

#' @export
unique.ApiResource <- function(x, ...)
    x[sapply(unique(id(x)), function(id) which.max(id(x) == id))]

setMethod("unique",
          signature(x = "ApiResource"),
          unique.ApiResource)


setMethod("id",
          signature(x = "ApiResource"),
          function(x) x@id)

setMethod("label",
          signature(x = "ApiResource"),
          function(x) x@label)

setMethod("label<-",
          signature(x = "ApiResource",
                    value = "character_or_NULL"),
          function(x, value) {
              x@label <- stretch(length(x), value, as.character(NA), as.character)
              invisible(x)
          })

setMethod("names",
          signature(x = "ApiResource"),
          function(x) sensorweb4R::label(x))

setMethod("names<-",
          signature(x = "ApiResource",
                    value = "character_or_NULL"),
          function(x, value) {
              sensorweb4R::label(x) <- value
              invisible(x)
          })

setMethod("endpoint",
          signature(x = "ApiResource"),
          function(x) x@endpoint)

setMethod("endpoint<-",
          signature(x = "ApiResource",
                    value = "Endpoint_or_NULL"),
          function(x, value) {
              x@endpoint <- stretch(length(x), value, as.character(NA), as.Endpoint)
              invisible(x)
          })

collection.name <- function(x) {
    switch(class(x),
           Service = "services",
           Station = "stations",
           Timeseries = "timeseries",
           Category = "categories",
           Offering = "offerings",
           Feature = "features",
           Procedure = "procedures",
           Phenomenon = "phenomena")
}

class.name <- function(x) {
    switch(x,
           services = Service,
           stations = Station,
           timeseries = Timeseries,
           categories = Category,
           offerings = Offering,
           features = Feature,
           procedures = Procedure,
           phenomena = Phenomenon)
}

#' @export
fromURI <- function(uri) {
    regex <- function(pattern, text) {
        result <- regexec(pattern, text)
        length <- sapply(result, attr, "match.length")
        result <- simplify2array(result)
        function(x) {
            begin <- result[x,]
            end <- begin + length[x,] - 1
            substr(text, begin, end)
        }
    }
    pattern <- "(^.*/v1/)([^/]*)/([^/?]*)"
    result <- regex(pattern, as.character(uri))
    ept <- result(2)
    type <- result(3)
    id <- result(4)
    type.unique <- unique(type)
    o <- lapply(type.unique, function(x) {
        f <- class.name(x)
        fetch(unique(f(id = id[x == type],
                endpoint = Endpoint(ept[x == type]))))
    })
    names(o) <- type.unique
    o
}

setMethod("resourceURL",
          signature(x = "ApiResource"),
          function(x) {
              paste(resourceURL(endpoint(x)),
                    collection.name(x),
                    id(x),
                    sep = "/")
          })







